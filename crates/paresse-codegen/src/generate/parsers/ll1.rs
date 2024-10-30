use std::collections::HashMap;

use paresse_core::grammar::{Symbol as SymbolId, SymbolSet};
use quote::{format_ident, quote, ToTokens};
use syn::Ident;

use crate::{
    hir::{GrammarHir, NonTerminal, Rule, Symbol, Terminal},
    parse::TerminalKind,
};

pub struct LL1Generator<'g> {
    grammar: &'g GrammarHir,
}

impl<'g> ToTokens for LL1Generator<'g> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.generate().to_tokens(tokens)
    }
}

/// A grammar rule, alongside iters augmented first set
struct AugmentedRule<'a> {
    rule: &'a Rule,
    first_set: &'a SymbolSet,
}

impl<'a> AugmentedRule<'a> {
    fn parse_items(&self) -> impl Iterator<Item = impl ToTokens> + '_ {
        let sym_action = |s: &Symbol| match s {
            Symbol::Terminal(Terminal {
                sym_id,
                kind: TerminalKind::Pattern(_),
            }) => {
                let id = sym_id.as_u32() as u16;
                quote! {
                    {
                       let t = self.advance()?;
                        match t {
                            Some(t) if t.kind == #id => self.tokens.lexeme(&t.span),
                            Some(other) => {
                                Err(
                                        paresse::ParseError::Expected {
                                            expected: &[#id],
                                            found: other,
                                })?
                            },
                            None => Err(paresse::ParseError::UnexpectedEof)?,
                        }
                    }
                }
            }
            Symbol::Terminal(Terminal {
                kind: TerminalKind::Epsilon,
                ..
            }) => {
                quote! {}
            }
            Symbol::NonTerminal(ref nt) => {
                let f = parse_fn_name(&nt.name);
                quote! {
                    self.#f()?
                }
            }
        };

        self.rule.rhs.iter().map(move |sym| {
            let action = sym_action(&sym.sym);
            match sym.binding {
                Some(ref binding) => {
                    quote! {
                        let #binding = #action;
                    }
                }
                None => {
                    quote! {
                        #action;
                    }
                }
            }
        })
    }

    fn ret_block(&self) -> impl ToTokens {
        match self.rule.handler {
            Some(ref e) => {
                quote! { return Ok(#e) }
            }
            None => {
                quote! { return Ok(std::default::Default::default()) }
            }
        }
    }
}

impl ToTokens for AugmentedRule<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let firsts = self
            .first_set
            .iter()
            .filter(|s| !s.is_eof() && !s.is_epsilon())
            .map(|s| s.as_u32() as u16)
            .collect::<Vec<_>>();
        let parse_items = self.parse_items();
        let ret_block = self.ret_block();

        let firsts = if !firsts.is_empty() {
            quote! { Some(#(#firsts)|*) }
        } else {
            quote! {}
        };

        let or_eof = if self.first_set.contains(SymbolId::eof()) {
            quote! { | None }
        } else {
            quote! {}
        };

        quote! {
            if matches!(self.peek().map(|t| t.kind), #firsts #or_eof) {
                #(#parse_items)*
                #ret_block
            }
        }
        .to_tokens(tokens)
    }
}

struct ParseFn<'a> {
    nt: &'a NonTerminal,
    rules: Vec<AugmentedRule<'a>>,
    grammar: &'a GrammarHir,
}

impl<'a> ToTokens for ParseFn<'a> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.generate().to_tokens(tokens)
    }
}

fn parse_fn_name(id: &Ident) -> Ident {
    format_ident!("parse_{id}")
}

impl<'a> ParseFn<'a> {
    fn generate(&self) -> impl ToTokens {
        let fn_name = parse_fn_name(&self.nt.name);
        let fn_ret_ty = &self.nt.name;
        let fn_body = self.generate_fn_body();
        quote! {
            fn #fn_name(&mut self) -> Result<#fn_ret_ty, paresse::Error> {
                #fn_body
            }
        }
    }

    fn generate_fn_body(&self) -> impl ToTokens {
        let rules = self.rules.iter();
        let fallback = self.generate_fallback();
        quote! {
            #(#rules)*
            #fallback
        }
    }

    fn generate_fallback(&self) -> impl ToTokens {
        let mut firsts = self
            .rules
            .iter()
            .map(|r| r.first_set)
            .fold(SymbolSet::new(), |agg, i| &agg | i);

        firsts.remove_epsilon();
        firsts.remove(SymbolId::eof().as_u32());

        let firsts = firsts.iter().map(|t| {
            self.grammar
                .terminal_mapper()
                .iter()
                .find(|(_, s)| *s == &t)
                .unwrap()
                .1
                .as_u32() as u16
        });
        quote! {
            match self.peek() {
                Some(&found) => {
                    Err(paresse::ParseError::Expected {
                        expected: &[#(#firsts),*],
                        found,
                    })?
                }
                None => Err(paresse::ParseError::UnexpectedEof)?
            }
        }
    }
}

impl<'g> LL1Generator<'g> {
    pub fn new(grammar: &'g GrammarHir) -> syn::Result<Self> {
        if let Err((sym, _ambi)) = grammar.grammar().is_backtrack_free() {
            let rule = grammar
                .non_terminal_mapper()
                .iter()
                .find_map(|(id, s)| (*s == sym).then_some(id))
                .unwrap();

            return Err(syn::Error::new_spanned(rule, format_args!("grammar is not backtack free, `{rule}` can not be disambiguished with a lookahead of 1. Transform the grammar to be backtrack free, or use a more general parser flavor such as lr1")));
        }

        Ok(Self { grammar })
    }

    pub fn generate(&self) -> impl ToTokens {
        let parse_fns = self.parse_fns();
        let parse_root_fn = self.generate_parse_root();

        quote! {
            pub struct Parser<'a> {
                tokens: Scan<'a>,
                current: Option<paresse::Token>,
            }

            impl<'a> Parser<'a> {
                fn peek(&self) -> Option<&paresse::Token> {
                    self.current.as_ref()
                }

                fn advance(&mut self) -> Result<Option<paresse::Token>, paresse::Error> {
                    let current = self.current.take();
                    self.current = self.tokens.next().transpose()?;
                    Ok(current)
                }

                #parse_root_fn
                #(#parse_fns)*
            }
        }
    }

    fn parse_fns(&self) -> impl Iterator<Item = ParseFn> {
        self.augmented_rules()
            .fold(HashMap::<_, Vec<_>>::new(), |mut agg, r| {
                agg.entry(&r.rule.lhs().name).or_default().push(r);
                agg
            })
            .into_values()
            .map(|rules| ParseFn {
                nt: rules[0].rule.lhs(),
                grammar: self.grammar,
                rules,
            })
    }

    fn augmented_rules(&self) -> impl Iterator<Item = AugmentedRule> {
        self.grammar
            .rules()
            .iter()
            .enumerate()
            .map(|(i, r)| AugmentedRule {
                rule: r,
                first_set: self.grammar.grammar().augmented_first_set().first_p(i),
            })
    }

    fn generate_parse_root(&self) -> impl ToTokens {
        let root_ty = self
            .grammar
            .non_terminal_mapper()
            .iter()
            .find(|(_, sym)| **sym == self.grammar.grammar().goal())
            .unwrap()
            .0;

        let root_fn_name = parse_fn_name(root_ty);

        quote! {
            pub fn parse(s: &'a str) -> Result<#root_ty, paresse::Error> {
                let mut tokens = Scan::new(s);
                let current = tokens.next().transpose()?;
                let mut parser = Self {
                        tokens,
                        current,
                };
                parser.#root_fn_name()
            }
        }
    }
}
