#![allow(clippy::upper_case_acronyms)]

use dfa::DFA;
use nfa::{Builder, State, StateId};
use parser::{Config, Flags};

mod alphabet;
mod ast;
mod dfa;
mod nfa;
mod parser;

pub use alphabet::Unit;

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub span: Span,
    pub kind: u16,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub offset: u32,
    pub len: u16,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self { offset: start, len: (end - start) as _ }
    }
}

pub struct ScannerBuilder {
    builder: Builder,
    /// Matching token, with the associated priority
    matchers: Vec<(u16, u16)>,
}

impl Default for ScannerBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ScannerBuilder {
    pub fn new() -> Self {
        let mut builder = Builder::default();
        builder.push(nfa::State::Split {
            targets: Vec::new(),
        });
        Self {
            builder,
            matchers: Vec::new(),
        }
    }

    pub fn token(&mut self, pat: &str, kind: u16) -> &mut Self {
        self.token_with_flags(pat, Flags::default(), kind)
    }

    pub fn token_insensitive(&mut self, pat: &str, kind: u16) -> &mut Self {
        self.token_with_flags(pat, Flags::default().set_case_insensitive(true), kind)
    }

    fn token_with_flags(
        &mut self,
        pat: &str,
        flags: Flags,
        token_kind: u16,
    ) -> &mut Self {
        let config = Config::default().set_flags(flags);
        let parser = parser::Parser::new(config);
        let ast = parser.parse(pat);
        let match_id = self.matchers.len();
        let (start, end) = ast.to_nfa(&mut self.builder, match_id);
        // add BOI anchor so that we only match token at the begining of the input
        let new_start = self.builder.push(State::Char {
            c: alphabet::Class::Boi,
            target: start,
        });
        self.builder.state_mut(end);
        self.add_token_state(new_start);
        self.matchers.push((token_kind, ast.priority()));

        self
    }

    fn add_token_state(&mut self, state: StateId) {
        let State::Split { targets } = self.builder.state_mut(0) else {
            unreachable!()
        };
        targets.push(state);
    }

    pub fn build(self) -> Scanner {
        let nfa = self.builder.build(0);
        let dfa = dfa::Builder::new(&nfa).build();
        // let dfa = dfa.minimize_aplhabet(); BUGGY

        Scanner {
            dfa,
            matchers: self.matchers,
        }
    }
}

pub struct Scanner {
    dfa: DFA,
    matchers: Vec<(u16, u16)>,
}

impl Scanner {
    pub fn dfa(&self) -> &DFA {
        &self.dfa
    }

    pub fn matches(&self) -> &[(u16, u16)] {
        &self.matchers
    }
}
