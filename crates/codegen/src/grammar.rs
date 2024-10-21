use std::collections::HashMap;

use syn::{Expr, Ident};
use grammar::symbol::Symbol as SymbolId;

use crate::parse::{ParsedGrammar, SymbolKind};

struct Symbol {
    sym_id: SymbolId,
    binding: Option<Ident>,
}

struct Rule {
    lhs: SymbolId,
    rhs: Vec<Symbol>,
    /// the handler for that expression
    handler: Option<Expr>,
}

pub struct GrammarBuilder<'a> {
    raw: &'a ParsedGrammar,
    builder: grammar::cfg::Builder,
    /// maps rule name to rule id
    non_terminal_mapper: HashMap<Ident, grammar::symbol::Symbol>,
    /// maps terminal pattern to id
    terminal_mapper: HashMap<String, grammar::symbol::Symbol>,
    /// rules being built
    rules: Vec<Rule>,
}

impl<'a> GrammarBuilder<'a> {
    pub fn new(raw: &'a ParsedGrammar) -> Self {
        Self {
            raw,
            builder: grammar::cfg::Builder::new(),
            non_terminal_mapper: HashMap::new(),
            terminal_mapper: HashMap::new(),
            rules: Vec::new(),
        }
    }

    pub fn build(mut self) -> Grammar {
        for rule in self.raw.rules() {
            let rule_sym = self.get_non_terminal_symbol(rule.lhs());
            let mut rhs = Vec::new();
            for sym in rule.rhs().syms() {
                let sym_id = match sym.kind() {
                    SymbolKind::Terminal(pat) => self.get_terminal_sym(pat),
                    SymbolKind::Nonterminal(nt) => self.get_non_terminal_symbol(nt)
                };

                let s = Symbol {
                    sym_id,
                    binding: sym.binding().cloned(),
                };

                rhs.push(s);
            }

            self.builder.rule(rule_sym).is(rhs.iter().map(|s| s.sym_id));

            let rule = Rule { lhs: rule_sym, rhs, handler: rule.handler().cloned() };
            self.rules.push(rule);
        }

        Grammar {
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

pub struct Grammar { 
    rules: Vec<Rule>,
    grammar: grammar::cfg::Grammar,
    /// maps rule name to rule id
    non_terminal_mapper: HashMap<Ident, SymbolId>,
    /// maps terminal pattern to id
    terminal_mapper: HashMap<String, SymbolId>,
}

impl Grammar {
    pub fn terminal_mapper(&self) -> &HashMap<String, SymbolId> {
        &self.terminal_mapper
    }
}
