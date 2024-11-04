mod analysis;
mod rule;
mod symbol;

use std::cell::OnceCell;
use std::collections::{hash_map::Entry, HashMap};

pub use analysis::*;
use rule::{Rule, RuleBuilder};
use symbol::SymbolSource;
pub use symbol::{Symbol, SymbolSet};

pub struct Grammar {
    goal: Symbol,
    rules: Vec<Rule>,
    symbols: SymbolSet,
    first_sets: OnceCell<FirstSets>,
    follow_sets: OnceCell<FollowSets>,
    terminals: OnceCell<Terminals>,
    non_terminals: OnceCell<NonTerminals>,
    augmented_first_sets: OnceCell<AugmentedFirstSets>,
    canonical_collection: OnceCell<CanonicalCollection>,
}

impl Grammar {
    pub fn builder() -> Builder {
        Builder::new()
    }

    /// Compute the first sets of this grammar, that is, for each rule, the set of symbols that
    /// can appear as the first symbol in a string derived from that rule.
    pub fn first_sets(&self) -> &FirstSets {
        self.first_sets.get_or_init(|| {
            FirstSets::compute(self.terminals(), self.non_terminals(), self.rules())
        })
    }

    /// Computes the set of non-terminal symbols of this grammar.
    /// Non-terminal symbols are the symbols that can appear as the left-hand side of a grammar rule.
    pub fn non_terminals(&self) -> &NonTerminals {
        self.non_terminals
            .get_or_init(|| NonTerminals::compute(self.rules()))
    }

    /// Returns the set of terminal symbols.
    /// Terminal symbols are the symbols that don't appear on the left-hand side of a grammar rule.
    pub fn terminals(&self) -> &Terminals {
        self.terminals
            .get_or_init(|| Terminals::compute(self.non_terminals(), self.symbols()))
    }

    /// Returns the rule of this grammar
    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }

    /// Returns all the rules (and index) that have s as a lhs
    pub fn rules_for(&self, s: Symbol) -> impl Iterator<Item = (usize, &Rule)> + '_ {
        self.rules
            .iter()
            .enumerate()
            .filter(move |(_, r)| r.lhs() == s)
    }

    /// Returns all the symbols defined for this grammar
    pub fn symbols(&self) -> &SymbolSet {
        &self.symbols
    }

    pub fn follow_sets(&self) -> &FollowSets {
        self.follow_sets.get_or_init(|| FollowSets::compute(self))
    }

    /// Compute the augmented first sets for this grammar
    pub fn augmented_first_set(&self) -> &AugmentedFirstSets {
        self.augmented_first_sets
            .get_or_init(|| AugmentedFirstSets::compute(self))
    }

    /// Returns the goal symbol of that grammar
    pub fn goal(&self) -> Symbol {
        self.goal
    }

    pub fn is_non_terminal(&self, s: Symbol) -> bool {
        self.non_terminals().contains(s)
    }

    pub fn is_terminal(&self, s: Symbol) -> bool {
        self.terminals().contains(s)
    }

    /// Returns Ok(()) if the grammar is backtrack free, or Err(sym) with a symbol that breaks the
    /// backtrack-free condition.
    pub fn is_backtrack_free(&self) -> Result<(), (Symbol, SymbolSet)> {
        let firstp = self.augmented_first_set().iter();
        let rules = self.rules().iter();

        let mut first_unions = HashMap::new();

        for (syms, rule) in firstp.zip(rules) {
            match first_unions.entry(rule.lhs()) {
                Entry::Occupied(mut e) => {
                    let int = e.get() & &**syms;
                    if !int.is_empty() {
                        return Err((rule.lhs(), SymbolSet::from_bitset(int)));
                    }
                    *e.get_mut() |= &**syms;
                }
                Entry::Vacant(e) => {
                    e.insert((**syms).clone());
                }
            }
        }
        Ok(())
    }

    pub fn canonical_collection(&self) -> &CanonicalCollection {
        self.canonical_collection
            .get_or_init(|| CanonicalCollection::compute(self))
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
        Grammar {
            goal: start,
            rules: self.rules,
            symbols: self.symbols,
            follow_sets: OnceCell::new(),
            non_terminals: OnceCell::new(),
            terminals: OnceCell::new(),
            first_sets: OnceCell::new(),
            augmented_first_sets: OnceCell::new(),
            canonical_collection: OnceCell::new(),
        }
    }
}
