use std::collections::HashMap;
use std::fmt::Debug;

use paresse_core::grammar::{Prec, Symbol as SymbolId};
use quote::ToTokens;
use syn::{Expr, Ident};

use crate::parse::{Binding, GrammarAst, RuleAttrs, SymbolKind, TerminalKind};

#[derive(Debug)]
pub struct NonTerminal {
    pub sym_id: SymbolId,
    pub name: Ident,
}

#[derive(Debug)]
pub struct Terminal {
    pub sym_id: SymbolId,
}

#[derive(Debug)]
pub enum Symbol {
    Terminal(Terminal),
    NonTerminal(NonTerminal),
}

impl Symbol {
    fn symbol_id(&self) -> SymbolId {
        match self {
            Symbol::Terminal(t) => t.sym_id,
            Symbol::NonTerminal(nt) => nt.sym_id,
        }
    }

    pub fn as_terminal(&self) -> Option<&Terminal> {
        if let Self::Terminal(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct MaybeBoundSymbol {
    pub binding: Option<Binding>,
    pub sym: Symbol,
    pub tokens: proc_macro2::TokenStream,
}

pub struct Rule {
    lhs: NonTerminal,
    pub rhs: Vec<MaybeBoundSymbol>,
    /// the handler for that expression
    pub handler: Option<Expr>,
}

impl Debug for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Rule")
            .field("lhs", &self.lhs)
            .field("rhs", &self.rhs)
            .field(
                "handler",
                &self
                    .handler
                    .as_ref()
                    .map(|h| h.to_token_stream().to_string()),
            )
            .finish()
    }
}

impl Rule {
    pub fn lhs(&self) -> &NonTerminal {
        &self.lhs
    }

    pub(crate) fn rhs(&self) -> &[MaybeBoundSymbol] {
        &self.rhs
    }
}

#[derive(Debug)]
pub struct TerminalDef {
    name: Option<Ident>,
    pat: String,
    id: SymbolId,
}

impl TerminalDef {
    pub fn name(&self) -> Option<&Ident> {
        self.name.as_ref()
    }

    pub fn pat(&self) -> &str {
        &self.pat
    }

    pub fn id(&self) -> SymbolId {
        self.id
    }
}

#[derive(Default, Debug)]
pub struct Terminals {
    defs: Vec<TerminalDef>,
    names: HashMap<Ident, usize>,
    pats: HashMap<String, usize>,
    ids: HashMap<SymbolId, usize>,
}

impl Terminals {
    fn insert(&mut self, name: Option<Ident>, pat: String, id: SymbolId) {
        assert!(!self.pats.contains_key(&pat));
        assert!(!self.ids.contains_key(&id));

        let idx = self.defs.len();
        if let Some(ref name) = name {
            assert!(!self.names.contains_key(name));
            self.names.insert(name.clone(), idx);
        }

        self.pats.insert(pat.clone(), idx);
        self.ids.insert(id, idx);
        self.defs.push(TerminalDef { name, pat, id });
    }

    pub fn iter(&self) -> impl Iterator<Item = &TerminalDef> {
        self.defs.iter()
    }

    pub fn get_by_name(&self, name: &Ident) -> Option<&TerminalDef> {
        self.names.get(name).and_then(|i| self.defs.get(*i))
    }

    fn get_by_pattern(&self, pat: &str) -> Option<&TerminalDef> {
        self.pats.get(pat).and_then(|i| self.defs.get(*i))
    }

    pub fn get_by_id(&self, id: SymbolId) -> Option<&TerminalDef> {
        self.ids.get(&id).and_then(|i| self.defs.get(*i))
    }

    fn contains_pattern(&self, pat: &str) -> bool {
        self.pats.contains_key(pat)
    }

    fn contains_name(&self, n: &Ident) -> bool {
        self.names.contains_key(n)
    }
}

pub struct GrammarBuilder<'a> {
    ast: &'a GrammarAst,
    builder: paresse_core::grammar::Builder,
    /// maps rule name to rule id
    non_terminals: NonTerminals,
    /// maps terminal pattern to id
    terminals: Terminals,
    /// rules being built
    rules: Vec<Rule>,
}

impl<'a> GrammarBuilder<'a> {
    pub fn new(raw: &'a GrammarAst) -> Self {
        Self {
            ast: raw,
            builder: paresse_core::grammar::Builder::new(),
            non_terminals: Default::default(),
            terminals: Default::default(),
            rules: Vec::new(),
        }
    }

    pub fn build(mut self) -> syn::Result<GrammarHir> {
        // forward-define non-terminals, and named terminals
        for rule in self.ast.rules() {
            if rule.is_named_terminal_definition() {
                let pat = rule
                    .rhs()
                    .syms()
                    .first()
                    .unwrap()
                    .kind()
                    .as_terminal()
                    .unwrap()
                    .as_pattern()
                    .expect("cannot bind empty string");
                assert!(
                    !(self.terminals.contains_name(rule.lhs())
                        | self.terminals.contains_pattern(pat)),
                    "previous definition of {}",
                    rule.lhs()
                );
                let sym_id = self.builder.next_sym();
                if let Some(attr) = rule.attr().and_then(RuleAttrs::as_token) {
                    self.builder.set_sym_prec(sym_id, attr.prec, attr.assoc);
                }
                self.terminals
                    .insert(Some(rule.lhs().clone()), pat.clone(), sym_id);
            } else {
                self.get_or_create_non_terminal_symbol(rule.lhs());
            }
        }

        let rules = self
            .ast
            .rules()
            .iter()
            .filter(|r| !r.is_named_terminal_definition());
        for rule in rules {
            let sym_id = self.get_or_create_non_terminal_symbol(rule.lhs());
            let lhs = NonTerminal {
                sym_id,
                name: rule.lhs().clone(),
            };
            let mut rhs = Vec::new();
            for sym in rule.rhs().syms() {
                let s = match sym.kind() {
                    SymbolKind::Terminal(kind) => {
                        let sym_id = self.get_terminal_sym(kind);
                        Symbol::Terminal(Terminal { sym_id })
                    }
                    SymbolKind::Nonterminal(nt) => {
                        if self.contains_non_terminal(nt) {
                            let sym_id = self.get_or_create_non_terminal_symbol(nt);
                            Symbol::NonTerminal(NonTerminal {
                                sym_id,
                                name: nt.clone(),
                            })
                        } else if let Some(t) = self.terminals.get_by_name(nt) {
                            Symbol::Terminal(Terminal { sym_id: t.id() })
                        } else {
                            return Err(syn::Error::new_spanned(
                                nt,
                                format_args!("no definition for rule `{nt}`"),
                            ));
                        }
                    }
                };

                let s = MaybeBoundSymbol {
                    sym: s,
                    binding: sym.binding().cloned(),
                    tokens: sym.tokens.clone(),
                };

                rhs.push(s);
            }

            if let Some(attr) = rule.attr().and_then(RuleAttrs::as_rule) {
                self.builder
                    .rule(lhs.sym_id)
                    .is_precedence(rhs.iter().map(|s| s.sym.symbol_id()), Prec {
                        assoc: attr.assoc,
                        prec: attr.prec,
                    });
            } else {
                self.builder
                    .rule(lhs.sym_id)
                    .is(rhs.iter().map(|s| s.sym.symbol_id()));
            };

            let rule = Rule {
                lhs,
                rhs,
                handler: rule.handler().cloned(),
            };

            self.rules.push(rule);
        }

        let goal = match self.ast.config().goal {
            Some(ref g) => {
                match self.non_terminals.get_sym(g) {
                    Some(s) => Some(s),
                    None => {
                        // TODO: check non-terminals too and report error when we support named
                        // terminals
                        return Err(syn::Error::new_spanned(
                            g,
                            format_args!("`{g}` is not a grammar rule"),
                        ));
                    }
                }
            }
            None => None,
        };

        Ok(GrammarHir {
            rules: self.rules,
            non_terminals: self.non_terminals,
            terminals: self.terminals,
            grammar: self.builder.build(goal),
        })
    }

    fn get_or_create_non_terminal_symbol(&mut self, rule_name: &Ident) -> SymbolId {
        match self.non_terminals.get_sym(rule_name) {
            Some(id) => id,
            None => {
                let id = self.builder.next_sym();
                self.non_terminals.insert(rule_name.clone(), id);
                id
            }
        }
    }

    fn contains_non_terminal(&self, rule_name: &Ident) -> bool {
        self.non_terminals.contains_ident(rule_name)
    }

    fn get_terminal_sym(&mut self, t: &TerminalKind) -> SymbolId {
        match t {
            TerminalKind::Epsilon => self.builder.epsilon(),
            TerminalKind::Pattern(ref pat) => match self.terminals.get_by_pattern(pat) {
                Some(def) => def.id,
                None => {
                    let id = self.builder.next_sym();
                    self.terminals.insert(None, pat.to_owned(), id);
                    id
                }
            },
        }
    }
}

#[derive(Debug, Default)]
pub struct NonTerminals {
    inner: HashMap<Ident, SymbolId>,
}

impl NonTerminals {
    pub fn get_sym(&self, id: &Ident) -> Option<SymbolId> {
        self.inner.get(id).copied()
    }

    pub fn get_ident(&self, symbol: SymbolId) -> Option<&Ident> {
        self.inner
            .iter()
            .find_map(|(i, s)| (*s == symbol).then_some(i))
    }

    pub fn contains_ident(&self, id: &Ident) -> bool {
        self.inner.contains_key(id)
    }

    fn insert(&mut self, id: Ident, symbol: SymbolId) {
        self.inner.insert(id, symbol);
    }

    pub fn idents(&self) -> impl Iterator<Item = &Ident> {
        self.inner.keys()
    }
}

pub struct GrammarHir {
    rules: Vec<Rule>,
    grammar: paresse_core::grammar::Grammar,
    /// maps rule name to rule id
    non_terminals: NonTerminals,
    /// maps terminal pattern to id
    terminals: Terminals,
}

impl GrammarHir {
    pub fn terminals(&self) -> &Terminals {
        &self.terminals
    }

    pub fn non_terminals(&self) -> &NonTerminals {
        &self.non_terminals
    }

    pub fn grammar(&self) -> &paresse_core::grammar::Grammar {
        &self.grammar
    }

    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }
}
