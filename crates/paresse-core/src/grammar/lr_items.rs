use hashbrown::HashSet;

use super::{Grammar, Symbol, SymbolSet};

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
struct LrItem {
    /// rule id in the grammar
    rule: usize,
    /// position of the placeholder in the rule, iow, the current position in rule.rhs in a derivation of
    /// rule.lhs.
    /// rule.rhs[placeholder] points to the symbol immediately right of the placeholder
    placeholder: usize,
    /// Lookahead symbol for this item.
    lookahead: Symbol,
}

impl LrItem {
    fn new(rule: usize, placeholder: usize, lookahead: Symbol) -> Self {
        Self {
            rule,
            placeholder,
            lookahead,
        }
    }
    /// Returns the symbol immediately to the right of this item's placeholder. If None is
    /// returned, then the placeholder is past the end of the production
    fn placeholder_right(&self, grammar: &Grammar) -> Option<Symbol> {
        let rule = &grammar.rules()[self.rule];
        debug_assert!(self.placeholder <= rule.rhs().len());
        rule.rhs().get(self.placeholder).copied()
    }

    fn first_detla(&self, grammar: &Grammar) -> SymbolSet {
        let c = grammar.rules()[self.rule]
            .rhs()
            .iter()
            .copied()
            .skip(self.placeholder + 1)
            .chain(std::iter::once(self.lookahead));
        grammar.first_sets().first_concat(c)
    }
}

#[derive(Debug)]
pub struct LrItems {
    items: HashSet<LrItem>,
}

impl LrItems {
    pub fn from_iter(i: impl IntoIterator<Item = LrItem>) -> Self {
        Self {
            items: i.into_iter().collect(),
        }
    }

    pub fn contains(&self, i: &LrItem) -> bool {
        self.items.contains(i)
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn closure(&mut self, grammar: &Grammar) {
        let mut worklist = Vec::from_iter(self.items.iter().copied());
        while let Some(item) = worklist.pop() {
            if let Some(right_symbol) = item.placeholder_right(grammar) {
                let firsts = item.first_detla(grammar);
                // for each item in the form [A -> B.Cd, a], we compute first(da)
                if grammar.is_non_terminal(right_symbol) {
                    let rules = grammar
                        .rules()
                        .iter()
                        .enumerate()
                        .filter(|(_, r)| r.lhs() == right_symbol);
                    for (rule_idx, _) in rules {
                        for lookahead in &firsts {
                            let item = LrItem::new(rule_idx, 0, lookahead);
                            if self.items.insert(item) {
                                worklist.push(item);
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::grammar;

    use super::*;

    #[test]
    fn lr_closure() {
        let mut builder = grammar::Builder::new();
        let [
        goal, // 2
        list, // 3
        pair, //4
        lparen, //5
        rparen, // 6
    ] = builder.syms();
        let eof = builder.eof();

        builder.rule(goal).is([list]); // 0
        builder
            .rule(list)
            .is([list, pair]) // 1
            .is([pair]); // 2
        builder
            .rule(pair)
            .is([lparen, pair, rparen]) // 3
            .is([lparen, rparen]); // 4

        let grammar = builder.build(None);

        let mut items = LrItems::from_iter(Some(LrItem {
            rule: 0,
            placeholder: 0,
            lookahead: eof,
        }));

        items.closure(&grammar);
        assert!(items.contains(&LrItem::new(0, 0, eof)));
        assert!(items.contains(&LrItem::new(1, 0, eof)));
        assert!(items.contains(&LrItem::new(1, 0, lparen)));
        assert!(items.contains(&LrItem::new(2, 0, eof)));
        assert!(items.contains(&LrItem::new(2, 0, lparen)));
        assert!(items.contains(&LrItem::new(3, 0, eof)));
        assert!(items.contains(&LrItem::new(3, 0, lparen)));
        assert!(items.contains(&LrItem::new(4, 0, eof)));
        assert!(items.contains(&LrItem::new(4, 0, lparen)));
        assert_eq!(items.len(), 9);
    }
}
