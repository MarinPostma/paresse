use std::ops::Index;

use crate::grammar::{Grammar, SymbolSet};

use super::{FirstSets, FollowSets};

#[derive(Debug)]
pub struct AugmentedFirstSets {
    inner: Vec<SymbolSet>,
}

impl Index<usize> for AugmentedFirstSets {
    type Output = SymbolSet;

    fn index(&self, index: usize) -> &Self::Output {
        &self.inner[index]
    }
}

impl AugmentedFirstSets {
    pub fn compute(grammar: &Grammar, first_sets: &FirstSets, follow_sets: &FollowSets) -> Self {
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
    use crate::grammar::Builder;

    #[ignore = "todo"]
    fn firstp_sets() {
        let mut builder = Builder::new();
        let [expr, exprp, term, termp, factor, lparen, rparen, plus, minus, mult, div, num, name] =
            builder.syms();
        let eps = builder.epsilon();
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
        let first = grammar.first_sets();
        let _first_p = grammar.augmented_first_set(&first, &follow);

        panic!()
    }
}
