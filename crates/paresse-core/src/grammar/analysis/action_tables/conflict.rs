use std::cmp::Ordering;

use crate::grammar::{Assoc, Grammar, Symbol};

use super::{ActionTableError, ActionTableSlot};

pub fn resolve_conflict(
    g: &Grammar,
    prev: &mut ActionTableSlot,
    new: ActionTableSlot,
    lookahead: Symbol,
) -> Result<(), ActionTableError> {
    if is_shift_reduce(prev, &new) {
        handle_shit_reduce(g, prev, new, lookahead)
    } else if is_reduce_reduce(prev, &new) {
        handle_reduce_reduce(g, prev, new)
    } else {
        Ok(())
    }
}

fn handle_reduce_reduce(
    g: &Grammar,
    prev: &mut ActionTableSlot,
    new: ActionTableSlot,
) -> Result<(), ActionTableError> {
    // no conflict at all
    if &new == prev {
        return Ok(());
    }

    let prev_prio = g.rule_priority(prev.item.rule_id());
    let new_prio = g.rule_priority(new.item.rule_id());
    match (prev_prio, new_prio) {
        (None, None) => {
            return Err(ActionTableError::UnhandledReduceReduce {
                rule1: prev.item.rule_id(),
                rule2: new.item.rule_id(),
            })
        }
        (None, Some(_)) => {
            // new has a higher prio
            *prev = new;
        }
        (Some(p), Some(n)) if n > p => {
            *prev = new;
        }
        (Some(p), Some(n)) if n == p => {
            // cannot break tie between two rules
            return Err(ActionTableError::UnhandledReduceReduce {
                rule1: prev.item.rule_id(),
                rule2: new.item.rule_id(),
            });
        }
        _ => (),
    }

    Ok(())
}

fn handle_shit_reduce(
    g: &Grammar,
    prev: &mut ActionTableSlot,
    new: ActionTableSlot,
    lookahead: Symbol,
) -> Result<(), ActionTableError> {
    let la_prec = g.sym_prec(lookahead);
    let reduce_prec = if new.action.is_reduce() {
        g.rule_prec(new.item.rule_id())
    } else {
        g.rule_prec(prev.item.rule_id())
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
                resolve_assoc(g, prev, new);
            }
            _ => (),
        },
        None => {
            return Err(ActionTableError::UnhandledShiftReduce {
                rule1: prev.item.rule_id(),
                rule2: new.item.rule_id(),
            })
        }
    }

    Ok(())
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

fn is_reduce_reduce(prev: &mut ActionTableSlot, new: &ActionTableSlot) -> bool {
    prev.action.is_reduce() && new.action.is_reduce()
}
