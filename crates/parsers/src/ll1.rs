use core::fmt;
use std::marker::PhantomData;

use grammar::symbol::Symbol;
use grammar::rule::Rule;
use grammar::cfg::{AugmentedFirstSets, Grammar, NonTerminals, Terminals};
use bitset::BitSetLike;

struct ParseTable {
    trans: Vec<u32>,
    stride: u32,
    rules: Vec<Rule>,
}

impl ParseTable {
    /// Create a new, emtpy parse table.
    ///
    /// Initially, all entries are set to u32::MAX, that symbolizes an error state.
    fn new(non_terminals: &NonTerminals, terminals: &Terminals, rules: Vec<Rule>) -> Self {
        let trans = vec![u32::MAX; ((non_terminals.max().unwrap() + 1) * (terminals.max().unwrap() + 1)) as usize];
        Self {
            trans,
            stride: terminals.max().unwrap(),
            rules,
        }
    }

    /// Add rule to the parse table.
    ///
    /// We use 0 to symbolize epsilon for symbols, but epsilon doesn't
    /// appear in the parse table, so we offset all rules by -1 to avoid having an empty column.
    fn add(&mut self, nt: Symbol, t: Symbol, rule: u32) {
        let idx = self.index(nt, t);
        self.trans[idx] = rule;
    }

    fn index(&self, nt: Symbol, t: Symbol) -> usize {
        (nt.as_u32() * self.stride + t.as_u32()) as usize
    }

    fn get_rule(&self, focus: Symbol, current: Symbol) -> Option<&Rule> {
        match self.trans[self.index(focus, current)] {
            u32::MAX => None,
            i => self.rules.get(i as usize),
        }
    }
}

pub struct MapTokenFn<F, T>(F, PhantomData<T>);

impl<F, T> MapTokenFn<F, T> {
    pub fn new(f: F) -> Self {
        Self(f, PhantomData)
    }
}

impl<F, T> MapToken for MapTokenFn<F, T>
where
    F: Fn(&T) -> Symbol,
    T: fmt::Debug,
{
    type TokenType = T;

    fn map(&self, tt: &Self::TokenType) -> Symbol {
        (self.0)(tt)
    }
}

pub trait MapToken {
    type TokenType: fmt::Debug;

    /// Map a TokenType to its associated terminal symbol in the grammar
    fn map(&self, tt: &Self::TokenType) -> Symbol;
}

pub struct Ll1Parser<M> {
    /// The parse table of this table-driven parser
    pt: ParseTable,
    /// The token type to symbol mapper
    mapper: M,
    terminals: Terminals,
}

impl<M> Ll1Parser<M> {
    /// Generare a parser from the passed grammar.
    pub fn generate(grammar: &Grammar, mapper: M) -> Self {
        let tt = TransitionTableBuilder::new(grammar).build();

        Self { pt: tt, mapper, terminals: grammar.terminals() }
    }

    /// Attempts to parse the passed
    pub fn parse(&self, start_symbol: Symbol) -> Parse<M> {
        Parse::new(self, start_symbol)
    }
}

pub struct Parse<'a, M> {
    /// reference to the parser
    parser: &'a Ll1Parser<M>,
    /// the parse stack
    stack: Vec<Symbol>,
}

impl<'a, M> Parse<'a, M> {
    pub fn new(parser: &'a Ll1Parser<M>, start_sym: Symbol) -> Self {
        let stack = vec![Symbol::eof(), start_sym];
        Self { parser, stack }
    }

    pub fn consume<I>(&mut self, mut tokens: I)
    where
        M: MapToken,
        I: Iterator<Item = M::TokenType>,
    {
        let mut word = tokens.next();
        let mut focus = *self.stack.last().unwrap();

        loop {
            let current = match word {
                Some(ref word) => self.parser.mapper.map(word),
                None => Symbol::eof(),
            };

            if focus.is_eof() && current.is_eof() {
                todo!("success")
            } else if focus.is_eof() || self.parser.terminals.contains(focus) {
                if focus == current {
                    self.stack.pop();
                    word = tokens.next();
                } else {
                    todo!("parse error: unexected token, expected {focus:?}")
                }
            } else {
                if let Some(rule) = self.parser.pt.get_rule(focus, current) {
                    self.stack.pop();
                    for &sym in rule.rhs().iter().rev() {
                        if !sym.is_epsilon() {
                            self.stack.push(sym);
                        }
                    }
                } else {
                    panic!("no rule")
                }
            }

            focus = *self.stack.last().unwrap();
        }
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

        let pt = ParseTable::new(&non_terminals, &terminals, grammar.rules().to_vec());

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

            if fp.contains(Symbol::eof()) {
                self.pt.add(rule.lhs(), Symbol::eof(), rule_idx as u32)
            }
        }

        self.pt
    }
}
