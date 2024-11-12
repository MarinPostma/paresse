use std::collections::{hash_map::Entry, BTreeSet, HashMap};

use crate::grammar::{
    analysis::action_tables::conflict::resolve_conflict, Action, Grammar, ReduceAction, ShiftAction, Symbol
};

use super::{ActionTable, ActionTableError, ActionTableSlot, GenAlg};

pub enum Lalr1 {}

impl GenAlg for Lalr1 {
    // FIXME: we're being lazy here and bulding the LALR action table the brute-force way, which
    // involves first building the canonnical collections and then merging kernels, but we could
    // merge duplicates as we create the canonical collection
    fn compute(g: &Grammar) -> Result<ActionTable, ActionTableError> {
        let cc = g.canonical_collection();
        let mut merged: HashMap<_, u32> = HashMap::new();
        let mut map_states = HashMap::new();

        for (i, c) in cc.collections() {
            let kernels = c.iter().map(|i| *i.kernel()).collect::<BTreeSet<_>>();
            let next_state = merged.len() as u32;
            match merged.entry(kernels) {
                Entry::Occupied(e) => {
                    let state = e.get();
                    map_states.insert(i, *state);
                }
                Entry::Vacant(e) => {
                    e.insert(next_state);
                    map_states.insert(i, next_state);
                }
            }
        }

        let mut actions: Vec<HashMap<Symbol, ActionTableSlot>> =
            std::iter::repeat_with(HashMap::new)
                .take(merged.len())
                .collect();

        for (cci, c) in cc.collections() {
            for item in c.iter() {
                let action = match item.action(g, cci) {
                    Action::Shift(ShiftAction { state, symbol }) => Action::Shift(ShiftAction {
                        state: *map_states.get(&state).unwrap(),
                        symbol,
                    }),
                    other => other,
                };
                let cci_mapped = *map_states.get(&cci).unwrap();
                let new = ActionTableSlot {
                    item: *item,
                    action,
                };

                let lookahead = match action {
                    Action::Shift(ShiftAction { symbol, .. }) => symbol,
                    Action::Reduce(ReduceAction { .. }) => item.lookahead(),
                    Action::Accept(_) => Symbol::eof(),
                    Action::Error => continue,
                };

                match actions[cci_mapped as usize].entry(lookahead) {
                    Entry::Occupied(mut e) => resolve_conflict(g, e.get_mut(), new, lookahead)?,
                    Entry::Vacant(e) => {
                        e.insert(new);
                    }
                }
            }
        }

        let mut goto = HashMap::new();
        for rule in g.rules() {
            let sym = rule.lhs();
            let mut trans: Vec<_> = cc
                .transitions_for(sym)
                .map(|(from, to)| {
                    (
                        *map_states.get(&from).unwrap(),
                        *map_states.get(&to).unwrap(),
                    )
                })
                .collect();

            trans.sort_unstable();
            trans.dedup();
            goto.insert(sym, trans);
        }

        Ok(ActionTable { actions, goto })
    }
}

#[cfg(test)]
mod test {
    use crate::grammar::Assoc;

    use super::*;

    #[test]
    fn generate_action_table() {
        let mut builder = Grammar::builder();
        let [goal, s, x, a, b] = builder.syms();

        builder.rule(goal).is([s]);
        builder.rule(s).is([x, x]);
        builder.rule(x).is([a, x]).is([b]);

        let g = builder.build(Some(goal));

        assert!(g.action_table::<Lalr1>().is_ok());
    }

    #[test]
    fn conflict_resolution() {
        let mut builder = Grammar::builder();
        let [goal, expr, plus, star, num] = builder.syms();

        builder.rule(goal).is([expr]);
        builder
            .rule(expr)
            .is([expr, plus, expr])
            .is([expr, star, expr])
            .is([num]);

        builder.set_sym_prec(star, Some(2), Assoc::Left);
        builder.set_sym_prec(plus, Some(1), Assoc::Left);

        let g = builder.build(Some(goal));

        assert!(g.action_table::<Lalr1>().is_ok());
    }
}
