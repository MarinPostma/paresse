use std::collections::HashMap;

use paresse_core::lexer::{ByteSet, Unit};
use paresse_core::lexer::{Scanner, ScannerBuilder};
use quote::{format_ident, quote, ToTokens};
use syn::Ident;

use crate::hir::GrammarHir;

pub struct LexerGenerator {
    scanner: Scanner,
}

impl ToTokens for LexerGenerator {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.generate().to_tokens(tokens)
    }
}

impl LexerGenerator {
    pub fn new(grammar: &GrammarHir) -> Self {
        let scanner = build_scanner(grammar);
        Self { scanner }
    }

    fn generate(&self) -> impl ToTokens {
        let state_enum = self.generate_state_enum();
        let transition_fn = self.generate_transition_fn();
        let match_states_fn = self.generate_match_states_fn();
        let start_state = self.start_state();

        quote! {
            use std::fmt::Write;
            #state_enum

            struct Scan<'a> {
                input: &'a str,
                start: usize,
                match_info: (usize, u16),
                sp: usize,
                state: __State,
            }

            impl<'a> Iterator for Scan<'a> {
                type Item = Result<paresse::Token, paresse::ScanError>;

                fn next(&mut self) -> Option<Self::Item> {
                    loop {
                        match self.next_token() {
                            /// u16::MAX is a sink token
                            Some(Ok(t)) if t.kind == u16::MAX => continue,
                            Some(r) => return Some(r),
                            None => return None,
                        }
                    }
                }
            }

            impl<'a> Scan<'a> {
                fn new(input: &'a str) -> Self {
                    Self {
                        input,
                        start: 0,
                        match_info: (0, 0),
                        sp: 0,
                        state: #start_state,
                    }
                }

                fn lexeme(&self, s: &paresse::Span) -> &str {
                    &self.input[s.offset as usize..s.offset as usize + s.len as usize]
                }

                fn next_token(&mut self) -> Option<Result<paresse::Token, paresse::ScanError>> {
                    if self.input.is_empty() {
                        return None;
                    }
                    self.reset();
                    while self.sp < self.input.len() {
                        let c = self.input.as_bytes()[self.sp];
                        match self.transition_char(c) {
                            Some(new_state) => {
                                self.state = new_state;
                                self.sp += 1;

                                if let Some(id) = Self::match_id(new_state) {
                                    self.match_info = (self.sp, id);
                                }
                            },
                            None if self.has_match() => return Some(Ok(self.cut_match())),
                            None => {
                                return Some(Err(paresse::ScanError::new(self.sp, paresse::ScanErrorKind::UnexpectedChar(c))))
                            },
                        }
                    }

                    if self.has_match() {
                        Some(Ok(self.cut_match()))
                    } else {
                        None
                    }
                }

                #match_states_fn

                #transition_fn

                fn reset(&mut self) {
                    self.state = #start_state;

                    if let Some(state) = self.transition_boi() {
                        self.state = state;
                    }

                    self.match_info = (0, 0);
                    self.sp = self.start;
                }

                fn has_match(&self) -> bool {
                    self.match_info.0 != 0
                }

                fn current_input(&self) -> &[u8] {
                    &self.input.as_bytes()[self.start..]
                }

                fn cut_match(&mut self) -> paresse::Token {
                    let kind = self.match_info.1;
                    let token = paresse::Token {
                        kind,
                        span: paresse::Span::new(self.start as u32, self.match_info.0 as u32),
                    };

                    self.start = self.match_info.0;

                    token
                }
            }
        }
    }

    fn start_state(&self) -> impl ToTokens {
        let id = self.scanner.dfa().initial();
        let id = state_ident(id);
        quote! { __State::#id }
    }

    /// We generate 3 transition function:
    /// - transition_boi: transition the begin of input
    /// - transition_eoi: transition on eoi
    /// - transition_char: transition for each indivitual char
    fn generate_transition_fn(&self) -> impl ToTokens {
        let dfa = self.scanner.dfa();
        let states = dfa.states_iter();

        let mut arms = Vec::new();

        let mut eoi_trans = Vec::new();
        let mut boi_trans = Vec::new();

        for state in states {
            let mut targets: HashMap<usize, Vec<Unit>> = HashMap::new();
            for unit in Unit::all() {
                if let Some(to) = dfa.transition(state, unit) {
                    targets.entry(to).or_default().push(unit);
                }
            }

            let pat = state_ident(state);
            let mut states = vec![quote! {__State::Invalid}; 256];
            targets.iter().for_each(|(target, units)| {
                let target = state_ident(*target);
                units.iter().for_each(|u| match u {
                    Unit::Boi => boi_trans.push(quote ! { #pat => Some(__State::#target) }),
                    Unit::Eoi => eoi_trans.push(quote ! { #pat => Some(_State::#target) }),
                    Unit::Byte(i) => { states[*i as usize] = quote!{ __State::#target }; },
                });
            });

            let arm = quote! {
                __State::#pat => {
                    const MAP: &[__State; 256] = &[#(#states),*];
                    match MAP[c as usize] {
                        __State::Invalid => None,
                        s => Some(s),
                    }
                }
            };

            arms.push(arm);
        }

        quote! {
            #[inline(always)]
            fn transition_boi(&self) -> Option<__State> {
                match self.state {
                    #(#boi_trans,)*
                    _ => None,
                }
            }

            #[inline(always)]
            fn transition_eoi(&self) -> Option<__State> {
                match self.state {
                    #(#eoi_trans,)*
                    _ => None,
                }
            }

            #[inline(always)]
            fn transition_char(&self, c: u8) -> Option<__State> {
                match self.state {
                    #(#arms,)*
                    _ => unreachable!(),
                }
            }
        }
    }

    fn generate_state_enum(&self) -> impl ToTokens {
        let dfa = self.scanner.dfa();
        let state_variants = dfa.states_iter().map(state_ident).collect::<Vec<_>>();

        quote! {
            #[derive(Clone, Copy)]
            enum __State {
                #(#state_variants,)*
                Invalid,
            }
        }
    }

    fn generate_match_states_fn(&self) -> impl ToTokens {
        let dfa = self.scanner.dfa();
        let match_arms = dfa.match_states().iter().map(|(&s_id, m_ids)| {
            let s = m_ids
                .iter()
                .map(|id| self.scanner.matches()[*id])
                .max_by_key(|m| m.1)
                .unwrap()
                .0;
            let state = state_ident(s_id);
            quote! { __State::#state => Some(#s) }
        });

        quote! {
            fn match_id(state: __State) -> Option<u16> {
                match state {
                    #(#match_arms,)*
                    _ => None,
                }
            }
        }
    }
}

fn state_ident(id: usize) -> Ident {
    format_ident!("S{id}")
}

fn build_scanner(grammar: &GrammarHir) -> Scanner {
    let mut builder = ScannerBuilder::new();

    for def in grammar.terminals().iter() {
        builder.token(def.pat(), def.id().as_u32() as u16);
    }

    // TODO: take whitespaces as a param to the parser
    builder.token("[ \n\t]+", u16::MAX);

    builder.build()
}
