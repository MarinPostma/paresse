use paresse_core::grammar::{ActionTableError, Symbol as SymbolId};
use paresse_core::grammar::{Action, CanonicalCollections};
use quote::{format_ident, quote, ToTokens};

use crate::hir::{self, GrammarHir, Rule, Symbol};

pub struct LR1Generator<'g> {
    grammar: &'g GrammarHir,
}

impl<'g> ToTokens for LR1Generator<'g> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self.generate() {
            Ok(tt) => tt.to_tokens(tokens),
            Err(e) => tokens.extend(e.to_compile_error()),
        }
    }
}

impl<'g> LR1Generator<'g> {
    pub fn new(grammar: &'g GrammarHir) -> syn::Result<Self> {
        Ok(Self { grammar })
    }

    pub fn generate(&self) -> syn::Result<impl ToTokens> {
        let rules = self.gen_rules()?;
        let fallback = quote! {
            _ => panic!("error!"),
        };
        let goal_ty = self
            .grammar
            .non_terminals()
            .get_ident(self.grammar.grammar().goal())
            .unwrap();
        let non_terminals_enum = self.gen_non_terminals_enum();
        Ok(quote! {
            enum StackItem {
                State(u32),
                Token(paresse::Token),
                Node(NonTerminals),
            }

            #non_terminals_enum

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

                fn run_parse(&mut self) -> #goal_ty {
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

                pub fn parse(s: &'input str) -> #goal_ty {
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
        })
    }

    fn gen_rules(&self) -> syn::Result<impl ToTokens> {
        let rules = (0..self.grammar.grammar().canonical_collection().len())
            .map(|i| self.gen_rule(i as u32))
            .collect::<syn::Result<Vec<_>>>()?;

        Ok(quote! {
            #(#rules)*
        })
    }

    fn gen_rule(&self, cci: u32) -> syn::Result<impl ToTokens> {
        let actions = match self.grammar.grammar().lr1_action_table() {
            Ok(t) => t.actions(cci),
            Err(ActionTableError::UnhandledShiftReduce { rule1, rule2 }) => {
                let rule1 = &self.grammar.rules()[rule1];
                let rule2 = &self.grammar.rules()[rule2];
                let mut tokens = Default::default();
                rule1.rhs_tokens(&mut tokens);
                let mut tokens2 = Default::default();
                rule2.rhs_tokens(&mut tokens2);
                return Err(syn::Error::new_spanned(
                    &tokens,
                    format_args!("shift/reduce conflict between:\n\t- {}\n\t- {}", tokens, tokens2)));
            },
            Err(ActionTableError::UnhandledReduceReduce { rule2, rule1 }) => {
                let rule1 = &self.grammar.rules()[rule1];
                let rule2 = &self.grammar.rules()[rule2];
                let mut tokens = Default::default();
                rule1.rhs_tokens(&mut tokens);
                let mut tokens2 = Default::default();
                rule2.rhs_tokens(&mut tokens2);
                return Err(syn::Error::new_spanned(
                    &tokens,
                    format_args!("reduce/reduce conflict between:\n\t- {}\n\t- {}", tokens, tokens2)));
            },
        };

        let t_rules = actions.into_iter().map(|(_s, action)| GenAction {
            from: cci,
            action,
            grammar: self.grammar,
        });

        Ok(quote! {
            #(#t_rules)*
        })
    }

    fn gen_non_terminals_enum(&self) -> impl ToTokens {
        let variants = self
            .grammar
            .non_terminals()
            .idents()
            .map(|i| quote! { #i(#i) });
        quote! {
            enum NonTerminals {
                #(#variants,)*
            }
        }
    }
}

struct GenAction<'a> {
    from: u32,
    action: Action,
    grammar: &'a hir::GrammarHir,
}

impl ToTokens for GenAction<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self.action {
            Action::Shift { state, symbol } => GenShift {
                from: self.from,
                to: state,
                sym: symbol,
            }
            .to_tokens(tokens),
            Action::Reduce { rule, symbol } => GenReduce {
                cc: self.grammar.grammar().canonical_collection(),
                rule: &self.grammar.rules()[rule],
                from: self.from,
                symbol,
            }
            .to_tokens(tokens),
            Action::Accept { rule } => GenAccept {
                from: self.from,
                rule: &self.grammar.rules()[rule],
            }
            .to_tokens(tokens),
            Action::Error => quote! {}.to_tokens(tokens),
        }
    }
}

struct GenAccept<'a> {
    from: u32,
    rule: &'a Rule,
}

impl ToTokens for GenAccept<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let from = self.from;
        let reduce = gen_reduce(self.rule, quote! { self });

        quote! {
            (#from, None) => {
                return #reduce;
            }
        }
        .to_tokens(tokens)
    }
}

struct GenReduce<'a> {
    cc: &'a CanonicalCollections,
    rule: &'a Rule,
    from: u32,
    symbol: SymbolId,
}

impl GenReduce<'_> {
    fn gen_match_token(&self) -> impl ToTokens {
        if self.symbol.is_eof() {
            quote! { None }
        } else {
            let s = self.symbol.as_u32() as u16;
            quote! { Some(#s)}
        }
    }

    /// generate code handling the transition to the next state in a reduce operation.
    fn gen_next_state_transition(&self) -> impl ToTokens {
        let transitions = self
            .cc
            .transitions_for(self.rule.lhs().sym_id)
            .map(|(from, to)| quote! { #from => #to });
        quote! {
            let next = match state {
                #(#transitions,)*
                invalid => unreachable!("invalid transition {invalid}, {t:?}"),
            };
            parser.stack.push(StackItem::State(next));
        }
    }

    fn gen_reduce(&self) -> impl ToTokens {
        let reduce_type = &self.rule.lhs().name;
        let reduced = gen_reduce(self.rule, quote! { parser });
        let reduce_fn_name = format_ident!("reduce_{reduce_type}_{}", self.from);
        let state_trans = self.gen_next_state_transition();
        // FIXME: just  a hack to reduce stack size, make cleaner
        let reduce_fn = quote! {
            fn #reduce_fn_name(parser: &mut Parser, t: Option<u16>) {
                let out = #reduced;
                let state = parser.state();
                parser.stack.push(StackItem::Node(NonTerminals::#reduce_type(out)));
                #state_trans
            }
        };
        
        quote! {
            #reduce_fn
            #reduce_fn_name(self, t);
        }
    }
}

impl ToTokens for GenReduce<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let match_token = self.gen_match_token();
        let reduce = self.gen_reduce();
        let cci = self.from;

        quote! {
            (#cci, t@#match_token) => {
                // reduce
                #reduce
            }
        }
        .to_tokens(tokens)
    }
}

struct GenShift {
    from: u32,
    to: u32,
    sym: SymbolId,
}

impl ToTokens for GenShift {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let s = self.sym.as_u32() as u16;
        let to = self.to;
        let from = self.from;
        quote! {
            (#from, Some(t@#s)) => {
                // shift
                let current = self.advance().unwrap();
                self.stack.push(StackItem::Token(current));
                self.stack.push(StackItem::State(#to));
            }
        }
        .to_tokens(tokens)
    }
}

fn gen_reduce(rule: &Rule, target: impl ToTokens) -> impl ToTokens {
    let bindings = rule.rhs().iter().rev().map(|s| {
        // the stack is layed out in that way: state, sym, state, sym...
        let binding = match &s.binding {
            Some(_)
                if s.sym
                    .as_terminal()
                    .map(|s| s.sym_id.is_epsilon())
                    .unwrap_or(false) =>
            {
                panic!("cannot bind epsilon transition")
            }
            Some(b) => {
                let mutable = if b.is_mutable() {
                    quote! { mut }
                } else {
                    quote! {}
                };
                let name = b.name();
                quote! { let #mutable #name = }
            }
            None => quote! {},
        };
        let action = match &s.sym {
            Symbol::Terminal(t) if t.sym_id.is_epsilon() => quote! {},
            Symbol::Terminal(_) => {
                quote! {
                    {
                        #target.stack.pop();
                        match #target.stack.pop() {
                            Some(StackItem::Token(t)) => {
                                #target.tokens.lexeme(&t.span)
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
            Symbol::NonTerminal(nt) => {
                let id = &nt.name;
                quote! {
                    {
                        #target.stack.pop();
                        match #target.stack.pop() {
                            Some(StackItem::Node(NonTerminals::#id(i))) => i,
                            _ => unreachable!(),
                        }
                    }
                }
            }
        };

        quote! {
            #binding #action;
        }
    });

    let handler = match &rule.handler {
        None => quote! { Default::default() },
        Some(h) => quote! { #h },
    };
    quote! {
        {
            #(#bindings)*
            #handler
        }
    }
}
