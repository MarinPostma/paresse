use super::alphabet::Class;
use super::nfa::{Builder, State, StateId};

#[derive(Debug, Clone)]
pub enum RegexAst {
    Match(Class),
    Kleene(Box<Self>),
    Alt(Box<Self>, Box<Self>),
    Concat(Box<Self>, Box<Self>),
}

impl RegexAst {
    pub(crate) fn to_nfa(&self, builder: &mut Builder, match_id: usize) -> (StateId, StateId) {
        build_nfa_from_ast(self, builder, match_id)
    }

    pub(crate) fn priority(&self) -> u16 {
        match self {
            RegexAst::Match(Class::Boi | Class::Eoi) => 2,
            RegexAst::Match(Class::Chars(set)) if set.len() == 1 => 2,
            RegexAst::Match(Class::Chars(_)) => 1,
            RegexAst::Kleene(e) => e.priority(),
            RegexAst::Alt(lhs, rhs) => lhs.priority().min(rhs.priority()),
            RegexAst::Concat(lhs, rhs) => lhs.priority() + rhs.priority(),
        }

    }
}

fn build_nfa_from_ast(
    ast: &RegexAst,
    builder: &mut Builder,
    match_id: usize,
) -> (StateId, StateId) {
    match ast {
        RegexAst::Match(c) => {
            let end = builder.push(State::Match { match_id });
            let start = builder.push(State::Char { c: *c, target: end });

            (start, end)
        }
        RegexAst::Kleene(e) => {
            let (start, end) = build_nfa_from_ast(e, builder, match_id);
            let new_end = builder.push(State::Match { match_id });

            *builder.state_mut(end) = State::Split {
                targets: vec![start, new_end],
            };

            let new_start = builder.push(State::Split {
                targets: vec![end, start],
            });

            (new_start, new_end)
        }
        RegexAst::Alt(lhs, rhs) => {
            let (lhs_start, lhs_end) = build_nfa_from_ast(lhs, builder, match_id);
            let (rhs_start, rhs_end) = build_nfa_from_ast(rhs, builder, match_id);

            let end = builder.push(State::Match { match_id });

            *builder.state_mut(lhs_end) = State::Goto { target: end };
            *builder.state_mut(rhs_end) = State::Goto { target: end };

            let start = builder.push(State::Split {
                targets: vec![lhs_start, rhs_start],
            });

            (start, end)
        }
        RegexAst::Concat(lhs, rhs) => {
            let (lhs_start, lhs_end) = build_nfa_from_ast(lhs, builder, match_id);
            let (rhs_start, rhs_end) = build_nfa_from_ast(rhs, builder, match_id);

            *builder.state_mut(lhs_end) = State::Goto { target: rhs_start };

            (lhs_start, rhs_end)
        }
    }
}
