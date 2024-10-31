use std::collections::HashSet;

use paresse_core::grammar::Action;
use quote::{quote, ToTokens};

use crate::hir::GrammarHir;

pub struct LR1Generator<'g> {
    grammar: &'g GrammarHir,
}

impl<'g> ToTokens for LR1Generator<'g> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.generate().to_tokens(tokens)
    }
}

impl<'g> LR1Generator<'g> {
    pub fn new(grammar: &'g GrammarHir) -> syn::Result<Self> {
        Ok(Self { grammar })
    }

    pub fn generate(&self) -> impl ToTokens {
        let rules = self.gen_rules();
        let fallback = quote! {
            _ => panic!("error!"),
        };
        quote! {
            enum StackItem {
                State(u32),
                Token(paresse::Token),
                Node(()),
            }

            pub struct Parser<'input> {
                stack: Vec<StackItem>,
                tokens: Scan<'input>,
                lookahead: Option<paresse::Token>,
            }

            impl<'input> Parser<'input> {
                fn state(&self) -> u32 {
                    match self.stack.last() {
                        Some(StackItem::State(state)) => *state,
                        _ => unreachable!("top of stack is not state"),
                    }
                }

                fn run_parse(&mut self) {
                    loop {
                        let state = self.state();
                        match (state, self.lookahead.map(|t| t.kind)) {
                            #rules
                            #fallback
                        }
                    }
                }

                fn advance(&mut self) -> Option<paresse::Token> {
                    let current = self.lookahead.take();
                    self.lookahead = self.tokens.next().transpose().unwrap();
                    current
                }

                pub fn parse(s: &'input str) {
                    let mut tokens = Scan::new(s);
                    let lookahead = tokens.next().transpose().unwrap();
                    let mut parser = Self {
                        tokens,
                        lookahead,
                        stack: vec![StackItem::State(0)],
                    };
                    parser.run_parse()
                }
            }
        }
    }

    fn gen_rules(&self) -> impl ToTokens {
        let rules = (0..self.grammar.grammar().canonical_collection().len())
            .map(|i| self.gen_rule(i as u32));

        quote! {
            #(#rules,)*
        }
    }

    fn gen_rule(&self, cci: u32) -> impl ToTokens {
        let canonical_collection = self.grammar.grammar().canonical_collection();
        let items = canonical_collection.get(cci);

        let actions = items
            .iter()
            .map(|i| i.action(self.grammar.grammar(), cci))
            .collect::<HashSet<_>>();

        let t_rules = actions.iter().filter_map(|a| match a {
            Action::Shift { state, symbol } => {
                let s = symbol.as_u32() as u16;
                let state = *state;
                Some(quote! {
                    (#cci, Some(#s)) => {
                        let current = self.advance().unwrap();
                        self.stack.push(StackItem::Token(current));
                        self.stack.push(StackItem::State(#state));
                    }
                })
            }
            Action::Reduce { rule, symbol } => {
                let rule = &self.grammar.rules()[*rule];
                let n_symbols = rule.rhs.len();
                let t = if symbol.is_eof() {
                    quote! { None }
                } else {
                    let s = symbol.as_u32() as u16;
                    quote! { Some(#s)}
                };

                let transitions = canonical_collection
                    .transitions_for(rule.lhs().sym_id)
                    .map(|(from, to)| quote! { #from => #to });

                Some(quote! {
                    (#cci, t@#t) => {
                        self.stack.drain(self.stack.len() - 2 * #n_symbols..);
                        let state = self.state();
                        self.stack.push(StackItem::Node(()));
                        let next = match state {
                            #(#transitions,)*
                            invalid => unreachable!("invalid transition {invalid}, {t:?}"),
                        };
                        self.stack.push(StackItem::State(next));
                    }
                })
            }
            Action::Accept => Some(quote! {
                (#cci, None) => {
                    return
                }
            }),
            Action::Error => None,
        });

        quote! {
            #(#t_rules)*
        }
    }
}
