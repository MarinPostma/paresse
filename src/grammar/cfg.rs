use std::ops::{Deref, DerefMut};
use std::collections::{BTreeMap, };

use super::{
    rule::{Rule, RuleBuilder},
    symbol::{Symbol, SymbolSet, SymbolSource},
};

#[derive(Default, Debug)]
pub struct Builder {
    symbol_source: SymbolSource,
    rules: Vec<Rule>,
    symbols: SymbolSet,
}

impl Builder {
    /// Instantiate a new, empty grammar
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a rule to the grammar
    pub fn push(&mut self, rule: Rule) {
        self.rules.push(rule)
    }

    /// Build a new rule for that grammar
    pub fn rule(&mut self, rhs: Symbol) -> RuleBuilder {
        RuleBuilder::new(rhs, self)
    }

    /// Returns the next new sym
    pub fn next_sym(&mut self) -> Symbol {
        let s = self.symbol_source.next().unwrap();
        self.symbols.add(s);
        s
    }

    pub fn epsilon(&mut self) -> Symbol {
        self.symbols.add(Symbol::epsilon());
        Symbol::epsilon()
    }

    /// Returns N new syms for that grammar
    pub fn syms<const N: usize>(&mut self) -> [Symbol; N] {
        let mut out: [u32; N] = [0; N];
        out.iter_mut().for_each(|s| *s = self.next_sym().as_u32());
        unsafe { std::mem::transmute_copy(&out) }
    }

    pub fn build(self) -> Grammar {
        Grammar {
            rules: self.rules,
            symbols: self.symbols,
        }
    }
}

#[derive(Debug)]
pub struct NonTerminals {
    inner: SymbolSet,
}

impl NonTerminals {
    fn new(grammar: &Grammar) -> Self {
        let mut inner = SymbolSet::new();
        for rule in &grammar.rules {
            inner.add(rule.lhs());
        }

        Self { inner }
    }
}

impl Deref for NonTerminals {
    type Target = SymbolSet;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Debug)]
pub struct Terminals {
    inner: SymbolSet,
}

impl Deref for Terminals {
    type Target = SymbolSet;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl Terminals {
    fn new(grammar: &Grammar) -> Self {
        let nt = grammar.non_terminals();
        let mut inner = grammar.symbols.difference(&nt.inner);
        inner.remove_epsilon();
        Self { inner }
    }
}

pub struct Grammar {
    rules: Vec<Rule>,
    symbols: SymbolSet,
}

impl Grammar {
    pub fn builder() -> Builder {
        Builder::new()
    }

    /// Compute the first sets of this grammar, that is, for each rule, the set of symbols that
    /// can appear as the first symbol in a string derived from that rule.
    pub fn first_set(&self) -> FirstSet {
        FirstSet::compute(self)
    }

    /// Computes the set of non-terminal symbols of this grammar.
    /// Non-terminal symbols are the symbols that can appear as the left-hand side of a grammar rule.
    pub fn non_terminals(&self) -> NonTerminals {
        NonTerminals::new(self)
    }

    /// Returns the set of terminal symbols.
    /// Terminal symbols are the symbols that don't appear on the left-hand side of a grammar rule.
    pub fn terminals(&self) -> Terminals {
        Terminals::new(self)
    }

    /// Returns the rule of this grammar
    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }

    /// Returns all the symbols defined for this grammar
    pub fn symbols(&self) -> &SymbolSet {
        &self.symbols
    }
}

#[derive(Debug)]
pub struct FirstSet {
    inner: BTreeMap<Symbol, SymbolSet>,
}

impl FirstSet {
    fn compute(grammar: &Grammar) -> Self {
        let mut first_sets: BTreeMap<Symbol, SymbolSet> = BTreeMap::new();
        let terminals = grammar.terminals();
        let non_terminals = grammar.non_terminals();

        for it in &*terminals {
            first_sets.entry(it).or_default().add(it);
        }

        first_sets.entry(Symbol::epsilon()).or_default().add_epsilon();

        for it in &*non_terminals {
            first_sets.insert(it, Default::default());
        }

        let mut changing = true;

        while changing {
            changing = false;
            for rule in &grammar.rules {
                let mut rhs = first_sets[&rule.rhs()[0]].clone();
                rhs.remove_epsilon();

                let mut idx= 0;
                while idx < rule.rhs().len() - 1 && first_sets[&rule.rhs()[idx]].contains_epsilon() {
                    *rhs |= first_sets[&rule.rhs()[idx + 1]].deref();
                    rhs.remove_epsilon();
                    idx += 1;
                }

                if idx == rule.rhs().len() - 1 && first_sets[&rule.rhs()[idx]].contains_epsilon() {
                    rhs.add_epsilon();
                }

                let first = first_sets.get_mut(&rule.lhs()).unwrap();
                let len_before = first.len();
                *first.deref_mut() |= rhs.deref();
                let len_after = first.len();
                changing |= len_before != len_after;
            }
        }

        Self { inner: first_sets }
    }
}
