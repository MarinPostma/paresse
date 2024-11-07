use std::ops::Index;

use crate::grammar::{Grammar, SymbolSet};

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
    pub fn compute(grammar: &Grammar) -> Self {
        let mut inner = Vec::new();

        for rule in grammar.rules() {
            let first = grammar
                .first_sets()
                .first_concat(rule.rhs().iter().copied());
            let aug = if first.contains_epsilon() {
                &first | grammar.follow_sets().follow(rule.lhs())
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

    pub fn iter(&self) -> impl Iterator<Item = &SymbolSet> {
        self.inner.iter()
    }
}

#[cfg(test)]
mod test {
    use crate::grammar::Builder;

    #[test]
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
        let _first_p = grammar.augmented_first_set();

        panic!()
    }
}
