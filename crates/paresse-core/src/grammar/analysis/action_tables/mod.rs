use super::{Action, LrItem};

pub mod lalr1;
pub mod lr1;
mod conflict;

#[derive(Debug)]
struct ActionTableSlot {
    item: LrItem,
    action: Action,
}

#[derive(Debug, Copy, Clone)]
pub enum ActionTableError {
    UnhandledShiftReduce { rule1: usize, rule2: usize },
    UnhandledReduceReduce { rule1: usize, rule2: usize },
}

