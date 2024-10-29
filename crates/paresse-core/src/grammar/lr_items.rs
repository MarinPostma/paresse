use std::collections::{BTreeSet, HashMap};

use crate::bitset::BitSet;

use super::{Grammar, Symbol, SymbolSet};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct LrItem {
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
    pub fn new(rule: usize, placeholder: usize, lookahead: Symbol) -> Self {
        Self {
            rule,
            placeholder,
            lookahead,
        }
    }
    /// Returns the symbol immediately to the right of this item's placeholder. If None is
    /// returned, then the placeholder is past the end of the production
    pub fn placeholder_right(&self, grammar: &Grammar) -> Option<Symbol> {
        let rule = &grammar.rules()[self.rule];
        debug_assert!(self.placeholder <= rule.rhs().len());
        rule.rhs().get(self.placeholder).copied()
    }

    /// compute first(da), for the item's rule in the form [A -> B.Cd, a]
    pub fn first_detla(&self, grammar: &Grammar) -> SymbolSet {
        let c = grammar.rules()[self.rule]
            .rhs()
            .iter()
            .copied()
            .skip(self.placeholder + 1)
            .chain(std::iter::once(self.lookahead));
        grammar.first_sets().first_concat(c)
    }

    /// returns a new LrItem with the placeholder advanced by one symbol
    pub fn advance(&self) -> Self {
        Self {
            placeholder: self.placeholder + 1,
            ..*self
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct LrItems {
    items: BTreeSet<LrItem>,
}

impl LrItems {
    pub fn new() -> Self {
        Default::default()
    }

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

    pub fn iter(&self) -> impl Iterator<Item = &LrItem> {
        self.items.iter()
    }

    fn placeholder_righthands(&self, grammar: &Grammar) -> SymbolSet {
        let mut set = SymbolSet::new();
        for item in &self.items {
            match item.placeholder_right(grammar) {
                Some(s) => set.insert(s),
                _ => (),
            }
        }

        set
    }

    /// given self, and a terminal symbol s, compute a model of the state of the parser after
    /// recognizing a s.
    pub fn goto(&self, grammar: &Grammar, s: Symbol) -> Self {
        let mut moved = Self::default();
        for item in &self.items {
            if item.placeholder_right(grammar) == Some(s) {
                moved.insert(item.advance());
            }
        }

        moved.closure(grammar);

        moved
    }

    /// insert a new item in this set. Returns true if this item wasn't already in the set.
    fn insert(&mut self, i: LrItem) -> bool {
        self.items.insert(i)
    }

    pub fn closure(&mut self, grammar: &Grammar) {
        let mut worklist = Vec::from_iter(self.items.iter().copied());
        while let Some(item) = worklist.pop() {
            if let Some(right_symbol) = item.placeholder_right(grammar) {
                let firsts = item.first_detla(grammar);
                if grammar.is_non_terminal(right_symbol) {
                    let rules = grammar.rules_for(right_symbol);
                    for (rule_idx, _) in rules {
                        for lookahead in &firsts {
                            let item = LrItem::new(rule_idx, 0, lookahead);
                            if self.insert(item) {
                                worklist.push(item);
                            }
                        }
                    }
                }
            }
        }
    }
}

pub struct CanonicalCollection {
    sets: Vec<LrItems>,
    transitions: HashMap<u32, HashMap<Symbol, u32>>,
}

impl CanonicalCollection {
    pub fn compute(grammar: &Grammar) -> Self {
        let mut sets = vec![Self::compute_cc0(grammar)];
        let mut unmarked = BitSet::from_iter([0]);
        let mut transitions: HashMap<_, HashMap<_, _>> = HashMap::new();
        while let Some(from) = unmarked.first() {
            unmarked.remove(from);
            let terminals = sets[from as usize].placeholder_righthands(grammar);
            for terminal in &terminals {
                let tmp = sets[from as usize].goto(grammar, terminal);
                let to = match Self::insert(&mut sets, tmp) {
                    Ok(id) => {
                        unmarked.insert(id);
                        id
                    }
                    Err(id) => id,
                };

                if let Some(other) = transitions.entry(from).or_default().insert(terminal, to) {
                    panic!("conflicting grammar rules for `{terminal:?}`: [{from} -> {to}], [{from} -> {other}]");
                }
            }
        }

        Self { sets, transitions }
    }

    fn insert(sets: &mut Vec<LrItems>, s: LrItems) -> Result<u32, u32> {
        match Self::id(sets, &s) {
            Some(id) => Err(id),
            None => {
                let id = sets.len() as u32;
                sets.push(s);
                Ok(id)
            }
        }
    }

    fn id(sets: &[LrItems], set: &LrItems) -> Option<u32> {
        sets.iter()
            .enumerate()
            .find_map(|(i, s)| (s == set).then_some(i as u32))
    }

    fn compute_cc0(grammar: &Grammar) -> LrItems {
        let cc0_items = grammar
            .rules_for(grammar.start())
            .map(|(idx, _)| LrItem::new(idx, 0, Symbol::eof()));
        let mut cc0 = LrItems::from_iter(cc0_items);
        cc0.closure(grammar);
        cc0
    }

    pub fn len(&self) -> usize {
        self.sets.len()
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

        let mut cc0 = LrItems::from_iter(Some(LrItem {
            rule: 0,
            placeholder: 0,
            lookahead: eof,
        }));

        cc0.closure(&grammar);
        assert!(cc0.contains(&LrItem::new(0, 0, eof)));
        assert!(cc0.contains(&LrItem::new(1, 0, eof)));
        assert!(cc0.contains(&LrItem::new(1, 0, lparen)));
        assert!(cc0.contains(&LrItem::new(2, 0, eof)));
        assert!(cc0.contains(&LrItem::new(2, 0, lparen)));
        assert!(cc0.contains(&LrItem::new(3, 0, eof)));
        assert!(cc0.contains(&LrItem::new(3, 0, lparen)));
        assert!(cc0.contains(&LrItem::new(4, 0, eof)));
        assert!(cc0.contains(&LrItem::new(4, 0, lparen)));
        assert_eq!(cc0.len(), 9);
    }

    #[test]
    fn goto() {
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

        let mut cc0 = LrItems::from_iter(Some(LrItem {
            rule: 0,
            placeholder: 0,
            lookahead: eof,
        }));

        cc0.closure(&grammar);

        let goto = cc0.goto(&grammar, lparen);
        assert_eq!(goto.len(), 6);
        assert!(goto.contains(&LrItem::new(3, 1, eof)));
        assert!(goto.contains(&LrItem::new(3, 1, lparen)));
        assert!(goto.contains(&LrItem::new(4, 1, eof)));
        assert!(goto.contains(&LrItem::new(4, 1, lparen)));
        assert!(goto.contains(&LrItem::new(3, 0, rparen)));
        assert!(goto.contains(&LrItem::new(4, 0, rparen)));
    }

    #[test]
    fn canonical_collection() {
        let mut builder = grammar::Builder::new();
        let [
        goal, // 2
        list, // 3
        pair, //4
        lparen, //5
        rparen, // 6
    ] = builder.syms();
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

        let cc = CanonicalCollection::compute(&grammar);

        assert_eq!(cc.len(), 12);
    }
}
