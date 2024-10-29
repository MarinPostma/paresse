mod lr_items;
mod rule;
mod symbol;
mod analysis;

use std::collections::HashMap;

use rule::{Rule, RuleBuilder};
use symbol::SymbolSource;
pub use symbol::{Symbol, SymbolSet};
pub use analysis::*;

pub struct Grammar {
    goal: Symbol,
    rules: Vec<Rule>,
    symbols: SymbolSet,
    first_sets: FirstSets,
    terminals: Terminals,
    non_terminals: NonTerminals,
}

impl Grammar {
    pub fn builder() -> Builder {
        Builder::new()
    }

    /// Compute the first sets of this grammar, that is, for each rule, the set of symbols that
    /// can appear as the first symbol in a string derived from that rule.
    pub fn first_sets(&self) -> &FirstSets {
        &self.first_sets
    }

    /// Computes the set of non-terminal symbols of this grammar.
    /// Non-terminal symbols are the symbols that can appear as the left-hand side of a grammar rule.
    pub fn non_terminals(&self) -> &NonTerminals {
        &self.non_terminals
    }

    /// Returns the set of terminal symbols.
    /// Terminal symbols are the symbols that don't appear on the left-hand side of a grammar rule.
    pub fn terminals(&self) -> &Terminals {
        &self.terminals
    }

    /// Returns the rule of this grammar
    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }

    /// Returns all the rules (and index) that have s as a lhs
    pub fn rules_for(&self, s: Symbol) -> impl Iterator<Item = (usize, &Rule)> + '_ {
        self.rules.iter().enumerate().filter(move |(_, r)| r.lhs() == s)
    }

    /// Returns all the symbols defined for this grammar
    pub fn symbols(&self) -> &SymbolSet {
        &self.symbols
    }

    pub fn follow_sets(&self) -> FollowSets {
        FollowSets::compute(self)
    }

    /// Compute the augmented first sets for this grammar
    pub fn augmented_first_set(
        &self,
        first_sets: &FirstSets,
        follow_sets: &FollowSets,
    ) -> AugmentedFirstSets {
        AugmentedFirstSets::compute(self, first_sets, follow_sets)
    }

    /// Returns the goal symbol of that grammar
    pub fn goal(&self) -> Symbol {
        self.goal
    }

    pub fn is_non_terminal(&self, s: Symbol) -> bool {
        self.non_terminals.contains(s)
    }

    pub fn is_terminal(&self, s: Symbol) -> bool {
        self.terminals.contains(s)
    }
}


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

    pub fn eof(&mut self) -> Symbol {
        self.symbols.add(Symbol::eof());
        Symbol::eof()
    }

    /// Returns N new syms for that grammar
    pub fn syms<const N: usize>(&mut self) -> [Symbol; N] {
        let mut out: [u32; N] = [0; N];
        out.iter_mut().for_each(|s| *s = self.next_sym().as_u32());
        unsafe { std::mem::transmute_copy(&out) }
    }

    fn find_start(&self) -> Symbol {
        let mut nts = self
            .rules
            .iter()
            .map(|r| (r.lhs(), 0))
            .collect::<HashMap<_, _>>();

        for rule in &self.rules {
            for s in rule.rhs() {
                if let Some(cnt) = nts.get_mut(s) {
                    *cnt += 1;
                }
            }
        }

        let candidates: Vec<Symbol> = nts
            .into_iter()
            .filter_map(|(s, c)| if c == 0 { Some(s) } else { None })
            .collect();

        if candidates.len() == 1 {
            candidates[0]
        } else if candidates.len() > 1 {
            panic!("multiple start symbol candidates: {candidates:?}")
        } else {
            panic!("no candidates for a start symbol")
        }
    }

    /// Build the grammar. Optionally the start symbol of the grammar can be passed. If it's not
    /// passed then we attempt to find it, as the only lhs that doesn't appear in any rhs. Grammars
    /// that are reccursive on the start symbol may not have an obvious start symbol, in that case,
    /// either pass the start symbol to the build method, or add a new lhs that points to the start
    /// of the grammar.
    pub fn build(self, start: Option<Symbol>) -> Grammar {
        let start = start.unwrap_or_else(|| self.find_start());
        let non_terminals = NonTerminals::compute(&self.rules);
        let terminals = Terminals::compute(&non_terminals, &self.symbols);
        let first_sets = FirstSets::compute(&terminals, &non_terminals, &self.rules);
        // add eof to the start rule
        Grammar {
            goal: start,
            rules: self.rules,
            symbols: self.symbols,
            non_terminals,
            terminals,
            first_sets,
        }
    }
}
