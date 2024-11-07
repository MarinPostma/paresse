use std::collections::{hash_map::Entry, HashMap};

use crate::grammar::{Action, Grammar, Symbol};

use super::{ conflict::resolve_conflict, ActionTableError, ActionTableSlot };

pub struct LR1ActionTable {
    actions: Vec<HashMap<Symbol, ActionTableSlot>>,
}


impl LR1ActionTable {
    pub fn num_states(&self) -> usize {
        self.actions.iter().map(|a| a.len()).sum()
    }

    /// Fixme: handle error in table construction
    pub fn compute(g: &Grammar) -> Result<Self, ActionTableError> {
        let cc = g.canonical_collection();
        let mut actions: Vec<HashMap<Symbol, ActionTableSlot>> =
            std::iter::repeat_with(HashMap::new)
                .take(cc.len())
                .collect();
        for (idx, col) in cc.collections() {
            for item in col {
                let action = item.action(g, idx);
                let c = match action {
                    Action::Shift { symbol, .. } => symbol,
                    Action::Reduce { .. } => item.lookahead(),
                    Action::Accept { .. } => Symbol::eof(),
                    Action::Error => continue,
                };

                let new = ActionTableSlot {
                    item: *item,
                    action,
                };

                match actions[idx as usize].entry(c) {
                    Entry::Occupied(mut e) => resolve_conflict(g, e.get_mut(), new, c)?,
                    Entry::Vacant(e) => {
                        e.insert(new);
                    }
                }
            }
        }

        Ok(Self { actions })
    }

    pub fn actions(&self, from: u32) -> impl Iterator<Item = (Symbol, Action)> + '_ {
        self.actions[from as usize]
            .iter()
            .map(|(a, b)| (*a, b.action))
    }
}
