use crate::bitset::BitSetLike;
use crate::grammar::symbol::Symbol;
use crate::grammar::rule::Rule;
use crate::grammar::cfg::{AugmentedFirstSets, Grammar, NonTerminals, Terminals};

struct ParseTable {
    trans: Vec<u32>,
    stride: usize,
}

impl ParseTable {
    /// Create a new, emtpy parse table.
    ///
    /// Initially, all entries are set to u32::MAX, that symbolizes an error state.
    fn new(non_terminals: &NonTerminals, terminals: &Terminals) -> Self {
        let trans = vec![u32::MAX; non_terminals.len() * terminals.len()];
        Self {
            trans,
            stride: terminals.len(),
        }
    }

    /// Add rule to the parse table.
    ///
    /// We use 0 to symbolize epsilon for symbols, but epsilon doesn't
    /// appear in the parse table, so we offset all rules by -1 to avoid having an empty column.
    fn add(&mut self, nt: Symbol, t: Symbol, rule: u32) {
        let idx = (nt.as_u32() - 1) * self.stride as u32 + (t.as_u32() - 1);
        self.trans[idx as usize] = rule
    }
}

pub trait MapToken {
    type TokenType;

    /// Map a TokenType to its associated terminal symbol in the grammar
    fn map(&self, tt: &Self::TokenType) -> Symbol;
}

pub struct Ll1Parser<M> {
    /// The parse table of this table-driven parser
    tt: ParseTable,
    /// The token type to symbol mapper
    mapper: M,
    /// rules in the grammar
    rules: Vec<Rule>,
}

impl<M> Ll1Parser<M> {
    /// Generare a parser from the passed grammar.
    pub fn generate(grammar: &Grammar, mapper: M) -> Self {
        let tt = TransitionTableBuilder::new(grammar).build();

        Self { tt, mapper, rules: grammar.rules().to_vec() }
    }

    /// Parse the passed token stream
    pub fn parse<I>(_tokens: I)
    where
        M: MapToken,
        I: Iterator<Item = M::TokenType>,
    {
        todo!()
    }
}

struct TransitionTableBuilder<'a> {
    grammar: &'a Grammar,
    augmented: AugmentedFirstSets,
    terminals: Terminals,
    pt: ParseTable,
}

impl<'a> TransitionTableBuilder<'a> {
    fn new(grammar: &'a Grammar) -> Self {
        let first = grammar.first_sets();
        let follow = grammar.follow_sets();
        let augmented = grammar.augmented_first_set(&first, &follow);
        let mut terminals = grammar.terminals();
        terminals.remove_epsilon();
        let non_terminals = grammar.non_terminals();

        let pt = ParseTable::new(&non_terminals, &terminals);

        Self {
            grammar,
            augmented,
            terminals,
            pt,
        }
    }

    fn build(mut self) -> ParseTable {
        for (rule_idx, rule) in self.grammar.rules().iter().enumerate() {
            let fp = self.augmented.first_p(rule_idx);
            let terminals = fp
                .intersection(&**self.terminals);

            let iter = terminals
                .items()
                .map(Symbol::from_u32);

            for t in iter {
                self.pt.add(rule.lhs(), t, rule_idx as u32);
            }
        }

        self.pt
    }
}
