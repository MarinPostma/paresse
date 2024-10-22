use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use super::alphabet::Unit;
use super::nfa::{StateId, NFA};

struct TransitionTable {
    tt: Vec<u32>,
    stride: usize,
}

impl TransitionTable {
    fn new(stride: usize) -> Self {
        Self {
            tt: Vec::new(),
            stride,
        }
    }

    fn new_state(&mut self) -> StateId {
        let id = self.tt.len();
        self.tt.extend(std::iter::repeat(0).take(self.stride));
        id
    }

    fn add_transition(&mut self, from: StateId, offset: usize, to: StateId) {
        self.tt[from + offset] = to as u32;
    }

    #[allow(dead_code)]
    fn states(&self) -> impl Iterator<Item = StateId> {
        let mut current = 0;
        let stride = self.stride;
        let until = self.tt.len();
        std::iter::from_fn(move || {
            if current >= until {
                return None;
            }
            let s = current;
            current += stride;
            Some(s)
        })
    }

    #[allow(dead_code)]
    fn state_id_to_usize(&self, state_id: StateId) -> usize {
        state_id / self.stride
    }

    #[allow(dead_code)]
    fn usize_to_state_id(&self, i: usize) -> StateId {
        i * self.stride
    }

    #[allow(dead_code)]
    fn state_count(&self) -> usize {
        self.tt.len() / self.stride
    }

    #[allow(dead_code)]
    fn translate(&self, to: &Self, id: StateId) -> StateId {
        assert_eq!(self.state_count(), to.state_count());
        to.usize_to_state_id(self.state_id_to_usize(id))
    }
}

pub struct DFA {
    initial: StateId,
    transitions: TransitionTable,
    match_states: HashMap<StateId, Vec<usize>>,
    class_map: ClassMap,
}

impl DFA {
    pub fn initial(&self) -> StateId {
        self.initial
    }

    pub fn match_states(&self) -> &HashMap<StateId, Vec<usize>> {
        &self.match_states
    }

    #[allow(dead_code)]
    pub(crate) fn pretty_state(&self, s: StateId) -> usize {
        s / self.transitions.stride
    }

    pub(crate) fn matches(&self, state: StateId) -> Option<&[usize]> {
        self.match_states.get(&state).map(|v| &**v)
    }

    pub fn transition(&self, from: StateId, u: Unit) -> Option<StateId> {
        let offset = self.class_map.offset(u)?;
        let id = self.transitions.tt[from + offset];
        if id == 0 {
            None
        } else {
            Some(id as usize)
        }
    }

    /// Minimizes the alphabet size for that alphabet by mapping characters that always map to the
    /// same state transisions to the same char classes
    /// Buggy: fix
    #[allow(dead_code)]
    pub fn minimize_aplhabet(&self) -> DFA {
        let mut state_map = HashMap::new();
        let mut class_map: Box<[u8; 256]> = Box::new([0; 256]);
        let mut class_id = 0;
        for c in 0..255 {
            let mut states = Vec::new();
            for s in self.transitions.states() {
                states.push(self.transitions.tt[s + c]);
            }

            // remove states that are always invalid
            if states.iter().all(|s| *s == 0) {
                continue;
            }

            match state_map.get(&states) {
                Some(&class_id) => {
                    class_map[c] = class_id;
                }
                None => {
                    // class_id 0 is reserve for dead states
                    class_id += 1;
                    state_map.insert(states, class_id);
                    class_map[c] = class_id;
                }
            }
        }

        let mut tt = TransitionTable::new(state_map.len() + 1);
        self.transitions.states().for_each(|_| {
            tt.new_state();
        });

        for state in self.transitions.states() {
            let ns = self.transitions.translate(&tt, state);
            for class in 1..state_map.len() {
                let representative = class_map
                    .iter()
                    .enumerate()
                    .find_map(|(i, c)| (class as u8 == *c).then_some(i))
                    .unwrap();
                let target_state = self.transitions.tt[state + representative];
                tt.tt[ns + class] = self.transitions.translate(&tt, target_state as usize) as u32;
            }

            // remap BOI and EOI
            tt.tt[ns] = self
                .transitions
                .translate(&tt, self.transitions.tt[state] as usize) as u32;
            tt.tt[ns + state_map.len()] = self.transitions.translate(
                &tt,
                self.transitions.tt[state + self.class_map.offset(Unit::Eoi).unwrap()] as usize,
            ) as u32;
        }

        let match_states = self
            .match_states
            .iter()
            .map(|(s, m)| (self.transitions.translate(&tt, *s), m.clone()))
            .collect();

        DFA {
            initial: 0,
            transitions: tt,
            match_states,
            class_map: ClassMap {
                classes: class_map,
                class_count: state_map.len(),
            },
        }
    }

    pub fn states_iter(&self) -> impl Iterator<Item = StateId> {
        self.transitions.states()
    }
}

impl fmt::Display for DFA {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let n_states = self.transitions.tt.len() / self.transitions.stride;
        let stride = self.transitions.stride;

        writeln!(
            f,
            "initial: {}, match: {:?}",
            self.initial(),
            self.match_states
                .iter()
                .map(|(s, id)| (s / stride, id))
                .collect::<Vec<_>>()
        )?;
        write!(f, "        ")?;
        for c in 0..=self.class_map.class_count {
            let representative = self.class_map.representative(c);
            write!(f, "{:<4}", format!("{representative}"))?;
        }
        writeln!(f)?;

        for s in 0..n_states {
            write!(f, "{s:>5} | ")?;
            for c in 0..=self.class_map.class_count {
                let s = self.transitions.tt[s * stride + c] as usize / stride;
                if s == 0 {
                    write!(f, "{:<4}", '_')?;
                } else {
                    write!(f, "{:<4}", s)?;
                }
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

pub struct Builder<'a> {
    tt: TransitionTable,
    nfa: &'a NFA,
    match_states: HashMap<StateId, Vec<usize>>,
    class_map: ClassMap,
}

impl<'a> Builder<'a> {
    pub(crate) fn new(nfa: &'a NFA) -> Self {
        let class_map = ClassMap::default();
        Self {
            nfa,
            tt: TransitionTable::new(class_map.alphabet_size()),
            match_states: HashMap::new(),
            class_map,
        }
    }

    pub(crate) fn build(mut self) -> DFA {
        let mut dstates = HashMap::new();
        let q0 = Rc::new(self.nfa.epsilon_closure([self.nfa.initial()]));
        let id0 = self.tt.new_state();
        dstates.insert(q0.clone(), id0);
        let mut workset = vec![(id0, q0)];
        let alphabet = self.nfa.alphabet();
        while let Some((from_id, q_prev)) = workset.pop() {
            for &unit in &alphabet {
                let q = Rc::new(
                    self.nfa
                        .epsilon_closure(self.nfa.delta_trans(q_prev.iter().copied(), unit)),
                );
                if q.is_empty() {
                    continue;
                }
                let to_id = match dstates.get(&q) {
                    Some(&id) => id,
                    None => {
                        let id = self.tt.new_state();
                        let match_ids = q
                            .iter()
                            .filter_map(|s| self.nfa.is_match(*s))
                            .collect::<Vec<_>>();
                        if !match_ids.is_empty() {
                            self.match_states.insert(id, match_ids);
                        }
                        dstates.insert(q.clone(), id);
                        workset.push((id, q));

                        id
                    }
                };

                if let Some(offset) = self.class_map.offset(unit) {
                    self.tt.add_transition(from_id, offset, to_id);
                }
            }
        }

        DFA {
            initial: id0,
            transitions: self.tt,
            match_states: self.match_states,
            class_map: self.class_map,
        }
    }
}

struct ClassMap {
    classes: Box<[u8; 256]>,
    class_count: usize,
}

impl Default for ClassMap {
    fn default() -> Self {
        let mut classes = Box::new([0; 256]);
        classes
            .iter_mut()
            .enumerate()
            .for_each(|(i, s)| *s = i as u8);
        Self {
            classes,
            class_count: 256,
        }
    }
}

impl ClassMap {
    fn offset(&self, u: Unit) -> Option<usize> {
        match u {
            Unit::Eoi => Some(self.class_count),
            Unit::Boi => Some(0),
            Unit::Byte(c) => {
                let class = self.classes[c as usize] as usize;
                if class == 0 {
                    None
                } else {
                    Some(class)
                }
            }
        }
    }

    fn representative(&self, class: usize) -> Unit {
        if class == 0 {
            Unit::Boi
        } else if class == self.class_count {
            Unit::Eoi
        } else {
            let repr = self
                .classes
                .iter()
                .enumerate()
                .find_map(|(i, c)| (class as u8 == *c).then_some(i))
                .unwrap() as u8;
            Unit::Byte(repr)
        }
    }

    /// returns the number of char classes + the number of special char classes
    fn alphabet_size(&self) -> usize {
        self.class_count + 1
    }
}
