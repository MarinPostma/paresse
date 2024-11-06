use std::collections::{hash_map::Entry, BTreeSet, HashMap};

use crate::grammar::Grammar;

use super::ActionTableError;

pub struct Lalr1ActionTable { }

impl Lalr1ActionTable {
    // FIXME: we're being lazy here and bulding the LALR action table the brute-force way, which
    // involves first building the canonnical collections and then merging kernels, but we could
    // merge duplicates as we create the canonical collection
    pub fn compute(g: &Grammar) -> Result<Self, ActionTableError> {
        let cc = g.canonical_collection();
        let mut merged: HashMap<_, Vec<u32>> = HashMap::new();
        for (i, c) in cc.collections() {
            let kernels = c.iter()
                .map(|i| *i.kernel())
                .collect::<BTreeSet<_>>();
            match merged.entry(kernels) {
                Entry::Occupied(mut e) => {
                    e.get_mut().push(i);
                },
                Entry::Vacant(e) => {
                    e.insert(vec![i]);
                },
            }
        }

        dbg!(cc.len());
        dbg!(merged.len());

        Ok(Self {})
    }
}
