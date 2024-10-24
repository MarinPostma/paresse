use std::collections::HashMap;

use grammar::cfg::AugmentedFirstSets;
use grammar::symbol::SymbolSet;
use quote::{format_ident, quote, ToTokens};
use syn::Ident;

use crate::{hir::{GrammarHir, NonTerminal, Rule, Symbol, Terminal}, parse::TerminalKind};

pub struct Ll1Generator<'g> {
    grammar: &'g GrammarHir,
    firstp: AugmentedFirstSets,
}

impl<'g> ToTokens for Ll1Generator<'g> {
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
            Symbol::Terminal(Terminal { sym_id, kind: TerminalKind::Pattern(pat) }) => {
                let id = sym_id.as_u32();
                quote! {
                    {
                       let t = self.advance();
                        match t {
                            Some(t) if t.kind == #id => t,
                            Some(other) => panic!("expected {}, but found {}", #pat, other.token),
                            None => panic!("unexpected eof"),
                        }
                    }
                }
            }
            Symbol::Terminal(Terminal { kind: TerminalKind::Epsilon, .. }) => {
                quote! { }
            }
            Symbol::NonTerminal(ref nt) => {
                let f = parse_fn_name(&nt.name);
                quote! {
                    self.#f()
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
                quote! { return #e }
            }
            None => {
                quote! { return std::default::Default::default() }
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
            .map(|s| s.as_u32())
        .collect::<Vec<_>>();
        let parse_items = self.parse_items();
        let ret_block = self.ret_block();

        let firsts = if !firsts.is_empty() {
            quote! { Some(#(#firsts)|*) }
        } else {
            quote! { }
        };

        let or_eof = if self.first_set.contains(grammar::symbol::Symbol::eof()) {
            quote! { | None }
        } else {
            quote! { }
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
            fn #fn_name(&mut self) -> #fn_ret_ty {
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
        let rule = self.nt.name.to_string();
        let mut firsts = self
            .rules
            .iter()
            .map(|r| r.first_set)
            .fold(SymbolSet::new(), |agg, i| &agg | i);

        firsts.remove_epsilon();
        firsts.remove(grammar::symbol::Symbol::eof().as_u32());

        let firsts = firsts.iter()
            .map(|t| {
                self.grammar
                    .terminal_mapper()
                    .iter()
                    .find(|(_, s)| *s == &t)
                    .unwrap()
                    .0
                    .as_str()
            });
        let expected = quote! {
            #(#firsts),*
        }.to_string();
        quote! {
            match self.peek() {
                Some(tt) => {
                    panic!("expected one of {}, but found {}", #expected, tt.token);
                }
                None => panic!("unexpected EOF while parsing {}", #rule)
            }
        }
    }
}

impl<'g> Ll1Generator<'g> {
    pub fn new(grammar: &'g GrammarHir) -> Self {
        let first_sets = grammar.grammar().first_sets();
        let follow_sets = grammar.grammar().follow_sets();
        let firstp = grammar
            .grammar()
            .augmented_first_set(&first_sets, &follow_sets);

        // TODO: verify that the grammar is indeed predictive

        Self { grammar, firstp }
    }

    pub fn generate(&self) -> impl ToTokens {
        let parse_fns = self.parse_fns();
        let parse_root_fn = self.generate_parse_root();

        quote! {
            pub struct Parser<'a> {
                tokens: Scan<'a>,
                current: Option<Spanned<'a>>,
            }

            impl<'a> Parser<'a> {
                fn peek(&self) -> Option<&Spanned> {
                    self.current.as_ref()
                }

                fn advance(&mut self) -> Option<Spanned> {
                    let current = self.current.take();
                    self.current = self.tokens.next();
                    current
                }

                #parse_root_fn
                #(#parse_fns)*
            }
        }
    }

    fn parse_fns(&self) -> impl Iterator<Item = ParseFn> {
        self.augmented_rules()
            .fold(HashMap::new(), |mut agg, r| {
                agg.entry(&r.rule.lhs().name)
                    .or_insert_with(Vec::new)
                    .push(r);
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
            .map(|(i, r)| {
                AugmentedRule {
                rule: r,
                first_set: self.firstp.first_p(i),
            } })
    }

    fn generate_parse_root(&self) -> impl ToTokens {
        let root_ty = self.grammar
            .non_terminal_mapper()
            .iter()
            .find(|(_, sym)| **sym == self.grammar.grammar().start())
            .unwrap().0;

        let root_fn_name = parse_fn_name(root_ty);

        quote! {
            pub fn parse(s: &'a str) -> #root_ty {
                let mut tokens = Scan::new(s);
                let current = tokens.next();
                let mut parser = Self {
                        tokens,
                        current,
                };
                parser.#root_fn_name()
            }
        }
    }
}
