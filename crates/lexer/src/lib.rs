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

#[derive(Debug)]
pub struct Span<'a> {
    start: usize,
    end: usize,
    input: &'a str,
}

impl<'a> Span<'a> {
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn token(&self) -> &str {
        self.input
    }
}

pub struct ScannerBuilder<T> {
    builder: Builder,
    /// Matching token, with the associated priority
    matchers: Vec<(T, u32)>,
}

impl<T: 'static + Clone> Default for ScannerBuilder<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: 'static + Clone> ScannerBuilder<T> {
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

    pub fn token(&mut self, pat: &str, t: T) -> &mut Self {
        self.token_with_flags(pat, Flags::default(), t)
    }

    pub fn token_insensitive(&mut self, pat: &str, t: T) -> &mut Self {
        self.token_with_flags(pat, Flags::default().set_case_insensitive(true), t)
    }

    fn token_with_flags(
        &mut self,
        pat: &str,
        flags: Flags,
        token_kind: T,
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

    pub fn build(self) -> Scanner<T> {
        let nfa = self.builder.build(0);
        let dfa = dfa::Builder::new(&nfa).build();
        // let dfa = dfa.minimize_aplhabet(); BUGGY

        Scanner {
            dfa,
            matchers: self.matchers,
        }
    }
}

pub struct Scanner<T> {
    dfa: DFA,
    matchers: Vec<(T, u32)>,
}

impl<T: Clone + 'static> Scanner<T> {
    pub fn scan<'a, 'b>(&'a self, input: &'b str) -> Scan<'a, 'b, T> {
        // println!("{}", self.dfa);
        Scan::new(self, input)
    }

    pub fn dfa(&self) -> &DFA {
        &self.dfa
    }

    pub fn matches(&self) -> &[(T, u32)] {
        &self.matchers
    }
}

pub struct Scan<'scanner, 'input, T> {
    scanner: &'scanner Scanner<T>,
    input: &'input str,
    start: usize,
    match_info: (StateId, usize),
    sp: usize,
    state: StateId,
}

#[derive(Debug)]
pub struct Spanned<'input, T> {
    kind: T,
    span: Span<'input>,
}

impl<'input, T> Spanned<'input, T> {
    pub fn kind(&self) -> &T {
        &self.kind
    }

    pub fn span(&self) -> &Span<'input> {
        &self.span
    }
}

impl<'scanner, 'input, T: Clone + 'static> Iterator for Scan<'scanner, 'input, T> {
    type Item = Spanned<'input, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.input.is_empty() {
            return None;
        }
        let dfa = &self.scanner.dfa;
        self.reset();
        while self.sp < self.input.len() {
            let u = Unit::Byte(self.input.as_bytes()[self.sp]);
            match dfa.transition(self.state, u) {
                Some(new_state) => {
                    self.state = new_state;
                    self.sp += 1;

                    if let Some(matches) = dfa.matches(new_state) {
                        let id = matches.iter().max_by_key(|i| self.scanner.matchers[**i].1).unwrap();
                        self.match_info = (self.sp, *id);
                    }
                }
                None if self.has_match() => return Some(self.cut_match()),
                None => {
                    panic!();
                }
            }
        }

        if self.has_match() {
            Some(self.cut_match())
        } else {
            None
        }
    }
}

impl<'scanner, 'input, T: Clone + 'static> Scan<'scanner, 'input, T> {
    fn new(scanner: &'scanner Scanner<T>, input: &'input str) -> Self {
        Self {
            scanner,
            input,
            start: 0,
            match_info: (0, 0),
            sp: 0,
            state: 0,
        }
    }

    fn reset(&mut self) {
        let dfa = &self.scanner.dfa;
        self.state = if let Some(s) = dfa.transition(dfa.initial(), Unit::Boi) {
            s
        } else {
            dfa.initial()
        };
        self.match_info = (0, 0);
        self.sp = 0;
    }

    fn has_match(&self) -> bool {
        self.match_info.0 != 0
    }

    fn cut_match(&mut self) -> Spanned<'input, T> {
        let (token, rest) = self.input.split_at(self.match_info.0);
        self.input = rest;
        let span = Span {
            start: self.start,
            end: self.start + self.match_info.0,
            input: token,
        };
        self.start = self.match_info.0;
        let kind = self.scanner.matchers[self.match_info.1].0.clone();
        Spanned { kind, span }
    }
}
