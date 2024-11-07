use std::collections::HashMap;

use crate::grammar::{Grammar, Symbol};

use super::{Action, LrItem};

pub mod lalr1;
pub mod lr1;
mod conflict;

#[derive(Debug)]
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
}

impl ActionTable {
    pub fn compute<G: GenAlg>(g: &Grammar) -> Result<Self, ActionTableError> {
        G::compute(g)
    }

    pub fn num_states(&self) -> usize {
        self.actions.iter().map(|a| a.len()).sum()
    }

    pub fn actions(&self, from: u32) -> impl Iterator<Item = (Symbol, Action)> + '_ {
        self.actions[from as usize]
            .iter()
            .map(|(a, b)| (*a, b.action))
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ActionTableError {
    UnhandledShiftReduce { rule1: usize, rule2: usize },
    UnhandledReduceReduce { rule1: usize, rule2: usize },
}

