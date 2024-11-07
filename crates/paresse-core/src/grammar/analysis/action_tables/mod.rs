use std::collections::HashMap;

use crate::grammar::{Grammar, Symbol};

use super::{Action, LrItem};

mod conflict;
pub mod lalr1;
pub mod lr1;

#[derive(Debug, PartialEq, Eq)]
struct ActionTableSlot {
    item: LrItem,
    action: Action,
}

pub trait GenAlg {
    fn compute(g: &Grammar) -> Result<ActionTable, ActionTableError>;
}

#[derive(Debug)]
pub struct ActionTable {
    actions: Vec<HashMap<Symbol, ActionTableSlot>>,
    /// maps rule-id to transitions (from, to)
    goto: HashMap<Symbol, Vec<(u32, u32)>>,
}

impl ActionTable {
    pub fn compute<G: GenAlg>(g: &Grammar) -> Result<Self, ActionTableError> {
        G::compute(g)
    }

    pub fn num_states(&self) -> usize {
        self.actions.len()
    }

    pub fn actions(&self, from: u32) -> impl Iterator<Item = (Symbol, Action)> + '_ {
        self.actions[from as usize]
            .iter()
            .map(|(a, b)| (*a, b.action))
    }

    pub fn goto(&self, rule: Symbol) -> impl Iterator<Item = (u32, u32)> + '_ {
        self.goto.get(&rule).unwrap().iter().map(|(f, t)| (*f, *t))
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ActionTableError {
    UnhandledShiftReduce { rule1: usize, rule2: usize },
    UnhandledReduceReduce { rule1: usize, rule2: usize },
}
