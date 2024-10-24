use std::collections::{BTreeMap, HashMap};
use std::ops::{Deref, DerefMut, Index};

use super::rule::{Rule, RuleBuilder};
use super::symbol::{Symbol, SymbolSet, SymbolSource};

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
        // add eof to the start rule
        Grammar {
            start,
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

impl DerefMut for Terminals {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Terminals {
    fn new(grammar: &Grammar) -> Self {
        let nt = grammar.non_terminals();
        let mut inner = grammar.symbols.difference(&nt.inner);
        inner.remove_epsilon();
        inner.add_eof();
        Self { inner }
    }
}

pub struct Grammar {
    start: Symbol,
    rules: Vec<Rule>,
    symbols: SymbolSet,
}

impl Grammar {
    pub fn builder() -> Builder {
        Builder::new()
    }

    /// Compute the first sets of this grammar, that is, for each rule, the set of symbols that
    /// can appear as the first symbol in a string derived from that rule.
    pub fn first_sets(&self) -> FirstSets {
        FirstSets::compute(self)
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

    /// Returns the start symbol of that grammar
    pub fn start(&self) -> Symbol {
        self.start
    }
}

#[derive(Debug)]
pub struct FollowSets {
    inner: BTreeMap<Symbol, SymbolSet>,
}

impl Index<Symbol> for FollowSets {
    type Output = SymbolSet;

    fn index(&self, index: Symbol) -> &Self::Output {
        self.follow(index)
    }
}

impl FollowSets {
    fn compute(grammar: &Grammar) -> Self {
        let mut follow_sets: BTreeMap<Symbol, SymbolSet> = BTreeMap::new();
        let first_sets = grammar.first_sets();
        let non_terminals = grammar.non_terminals();

        for nt in &*non_terminals {
            if nt == grammar.start() {
                let mut set = SymbolSet::new();
                set.add_eof();
                follow_sets.insert(nt, set);
            } else {
                follow_sets.insert(nt, Default::default());
            }
        }
        

        follow_sets
            .entry(grammar.start)
            .or_default()
            .add(Symbol::eof());

        let mut changing = true;

        while changing {
            changing = false;
            for rule in grammar.rules() {
                let mut trailer: SymbolSet = follow_sets[&rule.lhs()].clone();
                for s in rule.rhs().iter().rev() {
                    let mut first = first_sets.first(*s).clone();
                    if non_terminals.contains(*s) {
                        let follow = follow_sets.get_mut(s).unwrap();
                        let len_before = follow.len();
                        *follow.deref_mut() |= trailer.deref();
                        let len_after = follow.len();
                        changing |= len_before != len_after;
                        if first.contains_epsilon() {
                            first.remove_epsilon();
                            *trailer.deref_mut() |= first.deref();
                        } else {
                            trailer = first;
                        }
                    } else {
                        trailer = first;
                    }
                }
            }
        }

        Self { inner: follow_sets }
    }

    /// Returns the follow set for the passed symbol
    #[inline]
    pub fn follow(&self, s: Symbol) -> &SymbolSet {
        &self.inner[&s]
    }
}

#[derive(Debug)]
pub struct FirstSets {
    inner: BTreeMap<Symbol, SymbolSet>,
}

impl Index<Symbol> for FirstSets {
    type Output = SymbolSet;

    fn index(&self, index: Symbol) -> &Self::Output {
        self.first(index)
    }
}

impl FirstSets {
    /// Returns the first set for the passed symbol
    pub fn first(&self, s: Symbol) -> &SymbolSet {
        self.inner.get(&s).expect("first set for unexisting symbol")
    }

    /// Returns the first set arising from the concatenation of the passed symbols
    pub fn first_concat(&self, ss: impl IntoIterator<Item = Symbol>) -> SymbolSet {
        let mut out = SymbolSet::new();
        for s in ss {
            let f = self.first(s);
            *out.deref_mut() |= f.deref();
            if !f.contains_epsilon() {
                break;
            }
        }

        out
    }

    fn compute(grammar: &Grammar) -> Self {
        let mut first_sets: BTreeMap<Symbol, SymbolSet> = BTreeMap::new();
        let terminals = grammar.terminals();
        let non_terminals = grammar.non_terminals();

        for it in &*terminals {
            first_sets.entry(it).or_default().add(it);
        }

        first_sets
            .entry(Symbol::epsilon())
            .or_default()
            .add_epsilon();

        for it in &*non_terminals {
            first_sets.insert(it, Default::default());
        }

        let mut changing = true;

        while changing {
            changing = false;
            for rule in &grammar.rules {
                let mut rhs = first_sets[&rule.rhs()[0]].clone();
                rhs.remove_epsilon();

                let mut idx = 0;
                while idx < rule.rhs().len() - 1 && first_sets[&rule.rhs()[idx]].contains_epsilon()
                {
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

#[derive(Debug)]
pub struct AugmentedFirstSets {
    inner: Vec<SymbolSet>,
}

impl AugmentedFirstSets {
    fn compute(grammar: &Grammar, first_sets: &FirstSets, follow_sets: &FollowSets) -> Self {
        let mut inner = Vec::new();

        for rule in grammar.rules() {
            let first = first_sets.first_concat(rule.rhs().iter().copied());
            let aug = if first.contains_epsilon() {
                &first | follow_sets.follow(rule.lhs())
            } else {
                first
            };

            inner.push(aug);
        }

        Self { inner }
    }

    pub fn first_p(&self, rule: usize) -> &SymbolSet {
        &self.inner[rule]
    }

    // /// Returns whether the grammar is backtrack free
    // pub fn is_backtrack_free(&self) -> bool {
    //     for (sym, rules) in &self.inner {
    //         for (idx, lhs) in rules.iter().take(rules.len() - 1).enumerate() {
    //             for rhs in &rules[idx + 1..] {
    //                 if !rhs.intersection(&**lhs).is_empty() {
    //                     dbg!(sym, rhs, lhs);
    //                     return false;
    //                 }
    //             }
    //         }
    //     }
    //     true
    // }
}

#[cfg(test)]
mod test {
    use bitset::bitset;

    use super::*;

    #[test]
    fn first_sets() {
        let mut builder = Builder::new();
        let syms
        @ [expr, exprp, term, termp, factor, lparen, rparen, plus, minus, mult, div, num, name] =
            builder.syms();
        let eps = builder.epsilon();
        let eof = builder.eof();
        builder.rule(expr).is([term, exprp, eof]);
        builder
            .rule(exprp)
            .is([plus, term, exprp])
            .is([minus, term, exprp])
            .is([eps]);
        builder.rule(term).is([factor, termp]);
        builder
            .rule(termp)
            .is([mult, factor, termp])
            .is([div, factor, termp])
            .is([eps]);
        builder
            .rule(factor)
            .is([num])
            .is([name])
            .is([lparen, expr, rparen]);
        let grammar = builder.build(Some(expr));
        let firsts = grammar.first_sets();

        // There is a first for every symbol in the grammar + 2 for epsilon and eof
        assert_eq!(firsts.inner.len(), syms.len() + 2);
        // all non-terminals first sets contain only themselves
        let terminals = grammar.terminals();
        for terminal in terminals.iter() {
            assert_eq!(&*firsts[terminal], &bitset![terminal]);
        }

        assert_eq!(&*firsts[expr], &bitset![lparen, name, num]);
        assert_eq!(&*firsts[exprp], &bitset![plus, minus, eps]);
        assert_eq!(&*firsts[term], &bitset![lparen, name, num]);
        assert_eq!(&*firsts[termp], &bitset![mult, div, eps]);
        assert_eq!(&*firsts[factor], &bitset![lparen, name, num]);
    }

    #[test]
    fn follow_sets() {
        let mut builder = Builder::new();
        let [expr, exprp, term, termp, factor, lparen, rparen, plus, minus, mult, div, num, name] =
            builder.syms();
        let eps = builder.epsilon();
        let eof = builder.eof();
        builder.rule(expr).is([term, exprp]);
        builder
            .rule(exprp)
            .is([plus, term, exprp])
            .is([minus, term, exprp])
            .is([eps]);
        builder.rule(term).is([factor, termp]);
        builder
            .rule(termp)
            .is([mult, factor, termp])
            .is([div, factor, termp])
            .is([eps]);
        builder
            .rule(factor)
            .is([num])
            .is([name])
            .is([lparen, expr, rparen]);
        let grammar = builder.build(Some(expr));
        let follow = grammar.follow_sets();
        let non_terminals = grammar.non_terminals();

        // there is a follow set for each non-terminal
        assert_eq!(follow.inner.len(), non_terminals.len());

        assert_eq!(&*follow[expr], &bitset![eof, rparen]);
        assert_eq!(&*follow[exprp], &bitset![eof, rparen]);
        assert_eq!(&*follow[term], &bitset![eof, rparen, plus, minus]);
        assert_eq!(&*follow[termp], &bitset![eof, rparen, plus, minus]);
        assert_eq!(&*follow[factor], &bitset![eof, rparen, plus, minus, mult, div]);
    }
}
