use std::collections::{hash_map::Entry, HashMap};

use crate::grammar::{Action, Grammar, Symbol};

use super::conflict::resolve_conflict;
use super::{ActionTable, ActionTableError, ActionTableSlot, GenAlg};

pub enum Lr1 {}

impl GenAlg for Lr1 {
    fn compute(g: &Grammar) -> Result<ActionTable, ActionTableError> {
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

        let mut goto = HashMap::new();
        for rule in g.rules() {
            let sym = rule.lhs();
            let trans = cc.transitions_for(sym).collect();
            goto.insert(sym, trans);
        }

        Ok(ActionTable { actions, goto })
    }
}
