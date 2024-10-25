use std::collections::{BTreeSet, HashSet};

use super::alphabet::{Class, Unit};

pub type StateId = usize;

#[derive(Debug)]
pub enum State {
    Char { c: Class, target: StateId },
    Goto { target: StateId },
    Split { targets: Vec<StateId> },
    Match { match_id: usize },
}

#[derive(Debug, Default)]
pub struct Builder {
    transitions: Vec<State>,
}

impl Builder {
    pub(crate) fn push(&mut self, state: State) -> StateId {
        let state_id = self.transitions.len();
        self.transitions.push(state);
        state_id
    }

    pub(crate) fn build(self, initial: StateId) -> NFA {
        NFA {
            transitions: self.transitions,
            initial,
        }
    }

    pub(crate) fn state_mut(&mut self, id: StateId) -> &mut State {
        &mut self.transitions[id]
    }
}

#[derive(Debug)]
pub struct NFA {
    initial: StateId,
    transitions: Vec<State>,
}

impl NFA {
    /// Returns the epsilon-closure for the set of NFA states, that is the states that can be
    /// reached from init without consuming any input.
    pub(crate) fn epsilon_closure(
        &self,
        init: impl IntoIterator<Item = StateId>,
    ) -> BTreeSet<StateId> {
        let mut out = BTreeSet::from_iter(init);
        let mut workset = Vec::from_iter(out.iter().copied());
        while let Some(id) = workset.pop() {
            match &self.transitions[id] {
                State::Goto { target } => {
                    if out.insert(*target) {
                        workset.push(*target);
                    }
                }
                State::Split { targets } => {
                    for &id in targets {
                        if out.insert(id) {
                            workset.push(id);
                        }
                    }
                }
                _ => (),
            }
        }

        out
    }

    pub(crate) fn delta_trans(
        &self,
        s: impl IntoIterator<Item = StateId>,
        u: Unit,
    ) -> BTreeSet<StateId> {
        let mut out = BTreeSet::new();
        for id in s {
            match &self.transitions[id] {
                State::Char { c, target } if c.matches(u) => {
                    out.insert(*target);
                }
                _ => (),
            }
        }

        out
    }

    pub fn initial(&self) -> usize {
        self.initial
    }

    /// Returns whether the passed state is a match state
    pub(crate) fn is_match(&self, state: StateId) -> Option<usize> {
        match self.transitions[state] {
            State::Match { match_id } => Some(match_id),
            _ => None,
        }
    }

    pub(crate) fn alphabet(&self) -> HashSet<Unit> {
        let mut set = HashSet::new();
        for t in &self.transitions {
            if let State::Char { c, .. } = t {
                set.extend(c.units())
            }
        }

        set
    }
}
