use std::collections::HashMap;

use lexer::{Scanner, ScannerBuilder};
use quote::{format_ident, quote, ToTokens};
use syn::Ident;

use crate::grammar::Grammar;
use grammar::symbol::Symbol as SymbolId;
use lexer::Unit;

pub struct LexerGenerator<'g> {
    scanner: Scanner<SymbolId>,
    grammar: &'g Grammar,
}

impl<'g> LexerGenerator<'g> {
    pub fn new(grammar: &'g Grammar) -> Self {
        let scanner = build_scanner(grammar);
        Self { scanner, grammar }
    }

    pub fn generate(&self) -> impl ToTokens {
        let state_enum = self.generate_state_enum();
        let transition_fn = self.generate_transition_fn();
        let match_states_fn = self.generate_match_states_fn();
        let start_state = self.start_state();
        let spanned = self.gen_spanned();

        quote! {
            use std::fmt::Write;
            #state_enum

            #spanned

            #[derive(Copy, Clone, PartialEq, Eq, Hash)]
            pub enum Unit {
                Boi,
                Eoi,
                Byte(u8),
            }

            impl std::fmt::Debug for Unit {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{self}")
                }
            }

            impl std::fmt::Display for Unit {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        Unit::Boi => f.write_str("BOI"),
                        Unit::Eoi => f.write_str("EOI"),
                        Unit::Byte(c) => f.write_char(*c as char),
                    }
                }
            }

            struct Scan<'a> {
                input: &'a str,
                start: usize,
                match_info: (usize, u32),
                sp: usize,
                state: __State,
            }

            impl<'a> Iterator for Scan<'a> {
                type Item = Spanned<'a>;

                fn next(&mut self) -> Option<Self::Item> {
                    loop {
                        match self.next_token() {
                            Some(t) if t.kind == u32::MAX => continue,
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

                fn next_token(&mut self) -> Option<Spanned<'a>> {
                    if self.input.is_empty() {
                        return None;
                    }
                    self.reset();
                    while self.sp < self.input.len() {
                        let u = Unit::Byte(self.input.as_bytes()[self.sp]);
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

                    if let Some(state) = self.transition(Unit::Boi) {
                        self.state = state;
                    }

                    self.match_info = (0, 0);
                    self.sp = 0;
                }

                fn has_match(&self) -> bool {
                    self.match_info.0 != 0
                }

                fn cut_match(&mut self) -> Spanned<'a> {
                    let (token, rest) = self.input.split_at(self.match_info.0);
                    self.input = rest;
                    self.start += self.match_info.0;
                    let kind = self.match_info.1;
                    Spanned { 
                        kind,
                        start: self.start,
                        end: self.start + self.match_info.0,
                        token,
                    }
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
                                Unit::Boi => quote!(Unit::Boi),
                                Unit::Eoi => quote!(Unit::Eoi),
                                Unit::Byte(i) => quote!(Unit::Byte(#i)),
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
            fn transition(&self, u: Unit) -> Option<__State> {
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
                    .unwrap()
                    .0
                    .as_u32();
                let state = state_ident(s_id);
                quote!{ __State::#state => Some(#s) }
            });

        quote! {
            fn match_id(state: __State) -> Option<u32> {
                match state {
                    #(#match_arms,)*
                    _ => None,
                }
            }
        }
    }

    fn gen_spanned(&self) -> impl ToTokens {
        let arms = self.grammar.terminal_mapper().iter().map(|(pat, sym)| {
            let s = sym.as_u32();
            let p = pat.as_str();
            quote! { #s =>  #p }
        });

        quote! {
            pub struct Spanned<'a> {
                kind: u32,
                start: usize,
                end: usize,
                token: &'a str,
            }

            impl<'a> std::fmt::Debug for Spanned<'a> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    let pat = match self.kind {
                        #(#arms,)*
                        _ => unreachable!(),
                    };

                    f.debug_struct("Spanned")
                        .field("start", &self.start)
                        .field("end", &self.end)
                        .field("token", &self.token)
                        .field("kind", &pat)
                        .finish()

                }
            }
        }
    }
}

fn state_ident(id: usize) -> Ident {
    format_ident!("S{id}")
}

fn build_scanner(grammar: &Grammar) -> Scanner<SymbolId> {
    let mut builder = ScannerBuilder::new();

    for (pat, &sym) in grammar.terminal_mapper() {
        builder.token(pat, sym);
    }

    // TODO: take whitespaces as a param to the parser
    builder.token("[ \n\t]+", SymbolId::from_u32(u32::MAX));

    builder.build()
}
