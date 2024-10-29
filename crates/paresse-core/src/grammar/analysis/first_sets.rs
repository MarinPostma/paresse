use std::ops::Index;
use std::collections::BTreeMap;

use crate::grammar::{Symbol, SymbolSet};
use crate::grammar::rule::Rule;

use super::{NonTerminals, Terminals};


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
            *out |= &**f;
            if !f.contains_epsilon() {
                break;
            }
        }

        out
    }

    pub fn compute(terminals: &Terminals, non_terminals: &NonTerminals, rules: &[Rule]) -> Self {
        let mut first_sets: BTreeMap<Symbol, SymbolSet> = BTreeMap::new();

        for it in &**terminals {
            first_sets.entry(it).or_default().add(it);
        }

        first_sets
            .entry(Symbol::epsilon())
            .or_default()
            .add_epsilon();

        for it in &**non_terminals {
            first_sets.insert(it, Default::default());
        }

        let mut changing = true;

        while changing {
            changing = false;
            for rule in rules {
                let mut rhs = first_sets[&rule.rhs()[0]].clone();
                rhs.remove_epsilon();

                let mut idx = 0;
                while idx < rule.rhs().len() - 1 && first_sets[&rule.rhs()[idx]].contains_epsilon()
                {
                    *rhs |= &*first_sets[&rule.rhs()[idx + 1]];
                    rhs.remove_epsilon();
                    idx += 1;
                }

                if idx == rule.rhs().len() - 1 && first_sets[&rule.rhs()[idx]].contains_epsilon() {
                    rhs.add_epsilon();
                }

                let first = first_sets.get_mut(&rule.lhs()).unwrap();
                let len_before = first.len();
                **first |= &*rhs;
                let len_after = first.len();
                changing |= len_before != len_after;
            }
        }

        Self { inner: first_sets }
    }
}

#[cfg(test)]
mod test {
    use crate::{bitset, grammar::Builder};

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
}
