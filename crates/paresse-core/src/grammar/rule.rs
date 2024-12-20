use super::symbol::Symbol;
use super::{Builder, Grammar, RuleAttrs};

#[derive(Debug, Clone, Copy)]
pub enum Assoc {
    Left,
    Right,
}

#[derive(Debug, Clone)]
pub struct Rule {
    lhs: Symbol,
    rhs: Vec<Symbol>,
}

impl Rule {
    pub fn lhs(&self) -> Symbol {
        self.lhs
    }

    pub fn rhs(&self) -> &[Symbol] {
        &self.rhs
    }

    pub fn push_rhs(&mut self, s: Symbol) {
        self.rhs.push(s)
    }

    /// returns the last non-terminal mentionned in this rule's rhs
    pub fn last_terminal(&self, g: &Grammar) -> Option<Symbol> {
        for &s in self.rhs().iter().rev() {
            if g.is_terminal(s) {
                return Some(s);
            }
        }

        None
    }

    /// Returns the associativity of this rule, if any
    ///
    /// The precedence of a rule is the precedence of the latest non-terminal that it mentions
    pub fn assoc(&self, g: &Grammar) -> Option<Assoc> {
        self.last_terminal(g).and_then(|s| g.sym_assoc(s))
    }
}

#[derive(Debug)]
pub struct RuleBuilder<'g> {
    lhs: Symbol,
    grammar: &'g mut Builder,
}

impl<'g> RuleBuilder<'g> {
    pub fn new(lhs: Symbol, grammar: &'g mut Builder) -> Self {
        Self { lhs, grammar }
    }

    pub fn is(self, syms: impl IntoIterator<Item = Symbol>) -> Self {
        let rule = Rule {
            lhs: self.lhs,
            rhs: syms.into_iter().collect(),
        };
        self.grammar.rules.push(rule);
        self
    }

    pub fn is_precedence(self, syms: impl IntoIterator<Item = Symbol>, prec: RuleAttrs) -> Self {
        let rule = Rule {
            lhs: self.lhs,
            rhs: syms.into_iter().collect(),
        };
        let rule_id = self.grammar.rules.len();
        self.grammar.rules.push(rule);
        self.grammar.rule_attrs.insert(rule_id, prec);
        self
    }
}
