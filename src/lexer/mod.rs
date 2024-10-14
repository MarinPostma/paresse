#![allow(clippy::upper_case_acronyms)]
use std::marker::PhantomData;

use alphabet::Unit;
use dfa::DFA;
use nfa::{Builder, State, StateId};
use parser::{Config, Flags};

mod alphabet;
mod ast;
mod dfa;
mod nfa;
mod parser;

pub enum ScanError {
    Skip,
}

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

pub trait Emitter {
    type Token: 'static;
    fn emit(&self, span: Span) -> Result<Self::Token, ScanError>;
}

struct Skip<T>(PhantomData<T>);

impl<T> Skip<T> {
    fn new() -> Self {
        Self(PhantomData)
    }
}

struct Keyword<T>(T);

impl<T: Clone + 'static> Emitter for Keyword<T> {
    type Token = T;

    fn emit(&self, _: Span) -> Result<Self::Token, ScanError> {
        Ok(self.0.clone())
    }
}

impl<T: 'static> Emitter for Skip<T> {
    type Token = T;

    fn emit(&self, _: Span) -> Result<Self::Token, ScanError> {
        Err(ScanError::Skip)
    }
}

impl<T, F> Emitter for F
where
    F: for<'a> Fn(Span<'a>) -> Result<T, ScanError>,
    T: 'static,
{
    type Token = T;

    fn emit(&self, span: Span) -> Result<Self::Token, ScanError> {
        self(span)
    }
}

pub struct ScannerBuilder<T> {
    builder: Builder,
    matchers: Vec<Box<dyn Emitter<Token = T>>>,
}

impl<T: 'static> Default for ScannerBuilder<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: 'static> ScannerBuilder<T> {
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

    pub fn keyword(self, pat: &str, t: T) -> Self
    where
        T: Clone,
    {
        self.token(pat, Keyword(t))
    }

    pub fn keyword_insensitive(self, pat: &str, t: T) -> Self
    where
        T: Clone,
    {
        self.token_insensitive(pat, Keyword(t))
    }

    pub fn token(self, pat: &str, m: impl Emitter<Token = T> + 'static) -> Self {
        self.token_with_flags(pat, Flags::default(), m)
    }

    pub fn token_insensitive(self, pat: &str, m: impl Emitter<Token = T> + 'static) -> Self {
        self.token_with_flags(pat, Flags::default().set_case_insensitive(true), m)
    }

    fn token_with_flags(
        mut self,
        pat: &str,
        flags: Flags,
        m: impl Emitter<Token = T> + 'static,
    ) -> Self {
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
        self.matchers.push(Box::new(m));

        self
    }

    pub fn skip(self, pat: &str) -> Self {
        self.token(pat, Skip::new())
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
        let dfa = dfa.minimize_aplhabet();
        println!("{dfa}");
        Scanner {
            dfa,
            matchers: self.matchers,
        }
    }
}

pub struct Scanner<T> {
    dfa: DFA,
    matchers: Vec<Box<dyn Emitter<Token = T>>>,
}

impl<T: 'static> Scanner<T> {
    pub fn scan<'a, 'b>(&'a self, input: &'b str) -> Scan<'a, 'b, T> {
        // println!("{}", self.dfa);
        Scan::new(self, input)
    }
}

pub struct Scan<'a, 'b, T> {
    scanner: &'a Scanner<T>,
    input: &'b str,
    start: usize,
    match_info: (StateId, usize),
    sp: usize,
    state: StateId,
}

impl<'a, 'b, T: 'static> Iterator for Scan<'a, 'b, T> {
    type Item = T;

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

                    if let Some(id) = dfa.matches(new_state) {
                        self.match_info = (self.sp, id);
                    }
                }
                None if self.has_match() => match self.cut_match() {
                    Ok(token) => return Some(token),
                    Err(ScanError::Skip) => {
                        self.reset();
                    }
                },
                None => {
                    panic!();
                }
            }
        }

        if self.has_match() {
            match self.cut_match() {
                Ok(token) => Some(token),
                Err(ScanError::Skip) => None,
            }
        } else {
            None
        }
    }
}

impl<'a, 'b, T: 'static> Scan<'a, 'b, T> {
    fn new(scanner: &'a Scanner<T>, input: &'b str) -> Self {
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

    fn cut_match(&mut self) -> Result<T, ScanError> {
        let (token, rest) = self.input.split_at(self.match_info.0);
        self.input = rest;
        let span = Span {
            start: self.start,
            end: self.start + self.match_info.0,
            input: token,
        };
        self.start = self.match_info.0;
        self.scanner.matchers[self.match_info.1].emit(span)
    }
}
