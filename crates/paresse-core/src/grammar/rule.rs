use super::Builder;
use super::symbol::Symbol;

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
}

#[derive(Debug)]
pub struct RuleBuilder<'g> {
    rhs: Symbol,
    grammar: &'g mut Builder,
}

impl<'g> RuleBuilder<'g> {
    pub fn new(rhs: Symbol, grammar: &'g mut Builder) -> Self {
        Self { rhs, grammar }
    }

    pub fn is(self, syms: impl IntoIterator<Item = Symbol>) -> Self {
        let rule = Rule {
            lhs: self.rhs,
            rhs: syms.into_iter().collect(),
        };
        self.grammar.push(rule);
        self
    }
}
