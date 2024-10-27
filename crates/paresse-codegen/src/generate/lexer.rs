use std::collections::HashMap;

use quote::{format_ident, quote, ToTokens};
use syn::Ident;
use paresse_core::lexer::{Scanner, ScannerBuilder};
use paresse_core::lexer::Unit;

use crate::hir::GrammarHir;

pub struct LexerGenerator<'g> {
    scanner: Scanner,
    grammar: &'g GrammarHir,
}

impl<'g> ToTokens for LexerGenerator<'g> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.generate().to_tokens(tokens)
    }
}

impl<'g> LexerGenerator<'g> {
    pub fn new(grammar: &'g GrammarHir) -> Self {
        let scanner = build_scanner(grammar);
        Self { scanner, grammar }
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
                type Item = paresse::core::lexer::Token;

                fn next(&mut self) -> Option<Self::Item> {
                    loop {
                        match self.next_token() {
                            Some(t) if t.kind == u16::MAX => continue,
                            Some(t) => return Some(t),
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

                fn lexeme(&self, s: &paresse::core::lexer::Span) -> &str {
                    &self.input[s.offset as usize..s.offset as usize + s.len as usize]
                }

                fn next_token(&mut self) -> Option<paresse::core::lexer::Token> {
                    if self.input.is_empty() {
                        return None;
                    }
                    self.reset();
                    while self.sp < self.input.len() {
                        let u = paresse::core::lexer::Unit::Byte(self.input.as_bytes()[self.sp]);
                        match self.transition(u) {
                            Some(new_state) => {
                                self.state = new_state;
                                self.sp += 1;

                                if let Some(id) = Self::match_id(new_state) {
                                    self.match_info = (self.sp, id);
                                }
                            },
                            None if self.has_match() => return Some(self.cut_match()),
                            None => {
                                panic!("here");
                            },
                        }
                    }

                    if self.has_match() {
                        Some(self.cut_match())
                    } else {
                        None
                    }
                }

                #match_states_fn

                #transition_fn

                fn reset(&mut self) {
                    self.state = #start_state;

                    if let Some(state) = self.transition(paresse::core::lexer::Unit::Boi) {
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

                fn cut_match(&mut self) -> paresse::core::lexer::Token {
                    let kind = self.match_info.1;
                    let token = paresse::core::lexer::Token {
                        kind,
                        span: paresse::core::lexer::Span::new(self.start as u32, self.match_info.0 as u32),
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
        quote!{ __State::#id }
    }

    fn generate_transition_fn(&self) -> impl ToTokens {
        let dfa = self.scanner.dfa();
        let states = dfa
            .states_iter();

        let mut arms = Vec::new();

        for state in states {
            let mut targets: HashMap<usize, Vec<Unit>> = HashMap::new();
            for unit in Unit::all() {
                if let Some(to) = dfa.transition(state, unit) {
                    targets.entry(to).or_default().push(unit);
                }
            }

            let pat = state_ident(state);
            let inner_arms = targets
                .iter()
                .map(|(target, units)| {
                    let pats = units
                        .iter()
                        .map(|u| {
                            match u {
                                Unit::Boi => quote!(paresse::core::lexer::Unit::Boi),
                                Unit::Eoi => quote!(paresse::core::lexer::Unit::Eoi),
                                Unit::Byte(i) => quote!(paresse::core::lexer::Unit::Byte(#i)),
                            }
                        });

                    let target = state_ident(*target);
                    quote! {
                        #(#pats)|* => Some(__State::#target)
                    }
                });
            let arm = quote! {
                __State::#pat => {
                    match u {
                        #(#inner_arms,)*
                        _ => None,
                    }
                }
            };

            arms.push(arm);
        }

        quote! {
            fn transition(&self, u: paresse::core::lexer::Unit) -> Option<__State> {
                match self.state {
                    #(#arms,)*
                }
            }
        }
    }

    fn generate_state_enum(&self) -> impl ToTokens {
        let dfa = self.scanner.dfa();
        let state_variants = dfa
            .states_iter()
            .map(state_ident)
            .collect::<Vec<_>>();

        quote! {
            #[derive(Clone, Copy, Debug)]
            enum __State {
                #(#state_variants,)*
            }
        }
    }

    fn generate_match_states_fn(&self) -> impl ToTokens {
        let dfa = self.scanner.dfa();
        let match_arms = dfa.match_states()
            .iter()
            .map(|(&s_id, m_ids)| {
                let s = m_ids
                    .iter()
                    .map(|id| self.scanner.matches()[*id])
                    .max_by_key(|m| m.1)
                    .unwrap().0;
                let state = state_ident(s_id);
                quote!{ __State::#state => Some(#s) }
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

    for (pat, &sym) in grammar.terminal_mapper() {
        builder.token(pat, sym.as_u32() as u16);
    }

    // TODO: take whitespaces as a param to the parser
    builder.token("[ \n\t]+", u16::MAX);

    builder.build()
}
