use std::collections::{hash_map::Entry, HashMap};
use std::cmp::Ordering;

use crate::grammar::{Assoc, Grammar, Symbol};

use super::{Action, LrItem};

pub struct LR1ActionTable {
    actions: Vec<HashMap<Symbol, ActionTableSlot>>,
}

#[derive(Debug)]
struct ActionTableSlot {
    item: LrItem,
    action: Action,
}

impl LR1ActionTable {
    /// Fixme: handle error in table construction
    pub fn compute(g: &Grammar) -> Self {
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
                    Entry::Occupied(mut e) => Self::resolve_conflict(g, e.get_mut(), new, c),
                    Entry::Vacant(e) => {
                        e.insert(new);
                    }
                }
            }
        }

        Self { actions }
    }

    fn resolve_conflict(
        g: &Grammar,
        prev: &mut ActionTableSlot,
        new: ActionTableSlot,
        lookahead: Symbol,
    ) {
        if Self::is_shift_reduce(prev, &new) {
            Self::handle_shit_reduce(g, prev, new, lookahead);
        } else if Self::is_reduce_reduce(prev, &new) {
            todo!("handle reduce reduce")
        }
    }

    fn handle_shit_reduce(
        g: &Grammar,
        prev: &mut ActionTableSlot,
        new: ActionTableSlot,
        lookahead: Symbol,
    ) {
        let la_prec = g.prec(lookahead);
        let reduce_prec = if new.action.is_reduce() {
            new.item.rule(g).prec(g)
        } else {
            prev.item.rule(g).prec(g)
        };

        match reduce_prec.zip(la_prec) {
            Some((rprec, laprec)) => match rprec.cmp(&laprec) {
                Ordering::Greater if !prev.action.is_reduce() => {
                    *prev = new;
                }
                Ordering::Less if !prev.action.is_shift() => {
                    *prev = new;
                }
                Ordering::Equal => {
                    Self::resolve_assoc(g, prev, new);
                }
                _ => (),
            },
            None => todo!("no prec for rules {} and {}", prev.item.rule_id(), new.item.rule_id()),
        }
    }

    fn is_shift_reduce(lhs: &ActionTableSlot, rhs: &ActionTableSlot) -> bool {
        lhs.action.is_shift() && rhs.action.is_reduce()
            || lhs.action.is_reduce() && rhs.action.is_shift()
    }

    fn resolve_assoc(g: &Grammar, prev: &mut ActionTableSlot, new: ActionTableSlot) {
        // this is an associativity conflict
        match new.item.rule(g).assoc(g) {
            Some(Assoc::Left) if new.action.is_reduce() => {
                // if the rule is left associative, then we favor a reduce
                // operation over a shift
                *prev = new;
            }
            Some(Assoc::Right) if new.action.is_shift() => {
                // if the rule is right associative, on the other hand, we
                // favor a shift
                *prev = new;
            }
            // the branch handle the cases where the right action was
            // already in the slot
            Some(_) => (),
            None => todo!("unhandled shit-reduce conflict"),
        }
    }

    pub fn actions(&self, from: u32) -> impl Iterator<Item = (Symbol, Action)> + '_ {
        self.actions[from as usize]
            .iter()
            .map(|(a, b)| (*a, b.action))
    }

    fn is_reduce_reduce(prev: &mut ActionTableSlot, new: &ActionTableSlot) -> bool {
        prev.action.is_reduce() && new.action.is_reduce()
    }
}

