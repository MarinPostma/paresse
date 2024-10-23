use std::collections::HashMap;

use syn::{Expr, Ident};
use grammar::symbol::Symbol as SymbolId;

use crate::parse::{GrammarAst, SymbolKind};


pub struct NonTerminal {
    sym_id: SymbolId,
    pub name: Ident,
}

pub struct Terminal {
    pub sym_id: SymbolId,
    pub pattern: String,
}

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
}

pub struct MaybeBoundSymbol {
    pub binding: Option<Ident>,
    pub sym: Symbol,
}

pub struct Rule {
    lhs: NonTerminal,
    pub rhs: Vec<MaybeBoundSymbol>,
    /// the handler for that expression
    pub handler: Option<Expr>,
}

impl Rule {
    pub fn lhs(&self) -> &NonTerminal {
        &self.lhs
    }
}

pub struct GrammarBuilder<'a> {
    raw: &'a GrammarAst,
    builder: grammar::cfg::Builder,
    /// maps rule name to rule id
    non_terminal_mapper: HashMap<Ident, SymbolId>,
    /// maps terminal pattern to id
    terminal_mapper: HashMap<String, SymbolId>,
    /// rules being built
    rules: Vec<Rule>,
}

impl<'a> GrammarBuilder<'a> {
    pub fn new(raw: &'a GrammarAst) -> Self {
        Self {
            raw,
            builder: grammar::cfg::Builder::new(),
            non_terminal_mapper: HashMap::new(),
            terminal_mapper: HashMap::new(),
            rules: Vec::new(),
        }
    }

    pub fn build(mut self) -> GrammarHir {
        for rule in self.raw.rules() {
            let sym_id = self.get_non_terminal_symbol(rule.lhs());
            let lhs = NonTerminal {
                sym_id,
                name: rule.lhs().clone(),
            };
            let mut rhs = Vec::new();
            for sym in rule.rhs().syms() {
                let s = match sym.kind() {
                    SymbolKind::Terminal(pat) => {
                        let sym_id = self.get_terminal_sym(pat);
                        Symbol::Terminal(Terminal {
                            sym_id, pattern: pat.clone(),
                        })
                    },
                    SymbolKind::Nonterminal(nt) => {
                        let sym_id = self.get_non_terminal_symbol(nt);
                        Symbol::NonTerminal(NonTerminal { sym_id, name: nt.clone() })
                    }
                };

                let s = MaybeBoundSymbol {
                    sym: s,
                    binding: sym.binding().cloned(),
                };

                rhs.push(s);
            }

            self.builder.rule(lhs.sym_id).is(rhs.iter().map(|s| s.sym.symbol_id()));

            let rule = Rule { lhs, rhs, handler: rule.handler().cloned() };
            self.rules.push(rule);
        }

        GrammarHir {
            rules: self.rules,
            non_terminal_mapper: self.non_terminal_mapper,
            terminal_mapper: self.terminal_mapper,
            grammar: self.builder.build(),
        }
    }

    fn get_non_terminal_symbol(&mut self, rule_name: &Ident) -> SymbolId {
        match self.non_terminal_mapper.get(rule_name) {
            Some(&id) => id,
            None => {
                let id = self.builder.next_sym();
                self.non_terminal_mapper.insert(rule_name.clone(), id);
                id
            }
        }
    }

    fn get_terminal_sym(&mut self, pat: &str) -> SymbolId {
        match self.terminal_mapper.get(pat) {
            Some(&id) => id,
            None => {
                let id = self.builder.next_sym();
                self.terminal_mapper.insert(pat.to_owned(), id);
                id
            }
        }
    }
}

pub struct GrammarHir { 
    rules: Vec<Rule>,
    grammar: grammar::cfg::Grammar,
    /// maps rule name to rule id
    non_terminal_mapper: HashMap<Ident, SymbolId>,
    /// maps terminal pattern to id
    terminal_mapper: HashMap<String, SymbolId>,
}

impl GrammarHir {
    pub fn terminal_mapper(&self) -> &HashMap<String, SymbolId> {
        &self.terminal_mapper
    }

    pub fn non_terminal_mapper(&self) -> &HashMap<Ident, SymbolId> {
        &self.non_terminal_mapper
    }

    pub fn grammar(&self) -> &grammar::cfg::Grammar {
        &self.grammar
    }

    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }
}
