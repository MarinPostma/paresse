use std::ops::Index;
use std::collections::BTreeMap;

use crate::grammar::{Grammar, Symbol, SymbolSet};

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
    // TODO: this could be inproved with a worklist, by keeping track of sets that changed during
    // an iteration, and only process those
    pub fn compute(grammar: &Grammar) -> Self {
        let mut follow_sets: BTreeMap<Symbol, SymbolSet> = BTreeMap::new();
        let first_sets = grammar.first_sets();
        let non_terminals = grammar.non_terminals();

        for nt in &**non_terminals {
            if nt == grammar.goal() {
                let mut set = SymbolSet::new();
                set.add_eof();
                follow_sets.insert(nt, set);
            } else {
                follow_sets.insert(nt, Default::default());
            }
        }

        follow_sets
            .entry(grammar.goal)
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
                        **follow |= &*trailer;
                        let len_after = follow.len();
                        changing |= len_before != len_after;
                        if first.contains_epsilon() {
                            first.remove_epsilon();
                            *trailer |= &*first;
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

#[cfg(test)]
mod test {
    use crate::{bitset, grammar::Builder};

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
        assert_eq!(
            &*follow[factor],
            &bitset![eof, rparen, plus, minus, mult, div]
        );
    }
}
