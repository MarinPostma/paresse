use super::symbol::Symbol;
use super::Builder;

#[derive(Debug, Clone, Copy)]
pub enum Assoc {
    Left,
    Right
}

#[derive(Debug, Clone)]
pub struct Rule {
    lhs: Symbol,
    rhs: Vec<Symbol>,
    assoc: Assoc,
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

    pub fn assoc(&self) -> Assoc {
        self.assoc
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
            assoc: Assoc::Left,
        };
        self.grammar.push(rule);
        self
    }
}
