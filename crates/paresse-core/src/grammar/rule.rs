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
    prec: Option<usize>,
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

    pub fn precedence(&self) -> Option<usize> {
        self.prec
    }
}

#[derive(Debug)]
pub struct RulesBuilder<'g> {
    lhs: Symbol,
    grammar: &'g mut Builder,
}

pub struct RuleBuilder<'g> {
    builder: RulesBuilder<'g>,
    rhs: Vec<Symbol>,
    assoc: Assoc,
    prec: Option<usize>,
}

impl<'g> RuleBuilder<'g> {
    pub fn is(self, syms: impl IntoIterator<Item = Symbol>) -> Self {
        let rule = Rule {
            lhs: self.builder.lhs,
            rhs: self.rhs,
            prec: self.prec,
            assoc: Assoc::Left,
        };
        self.builder.grammar.rules.push(rule);
        Self {
            builder: self.builder,
            rhs: syms.into_iter().collect(),
            assoc: self.assoc,
            prec: self.prec,
        }
    }

    pub fn with_assoc(mut self, assoc: Assoc) -> Self {
        self.assoc = assoc;
        self
    }

    pub fn with_precedence(mut self, prec: usize) -> Self {
        self.prec = Some(prec);
        self
    }

    pub fn build(self) {
        let rule = Rule {
            lhs: self.builder.lhs,
            rhs: self.rhs,
            prec: self.prec,
            assoc: Assoc::Left,
        };
        self.builder.grammar.rules.push(rule);
    }
}

impl<'g> RulesBuilder<'g> {
    pub fn new(lhs: Symbol, grammar: &'g mut Builder) -> Self {
        Self { lhs, grammar }
    }

    pub fn is(self, syms: impl IntoIterator<Item = Symbol>) -> RuleBuilder<'g> {
        RuleBuilder {
            builder: self,
            rhs: syms.into_iter().collect(),
            assoc: Assoc::Left,
            prec: None,
        }
    }
}
