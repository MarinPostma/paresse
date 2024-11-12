use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

use paresse_core::grammar::{AcceptAction, Action, ActionTable, GenAlg, ReduceAction, ShiftAction};
use paresse_core::grammar::{ActionTableError, Symbol as SymbolId};
use quote::{format_ident, quote, ToTokens};

use crate::hir::{GrammarHir, Rule, Symbol};

/// Generate a direct-coded parsed of the LR family. The parser generated depends on the Gen type
/// parameter.
pub struct LRGenerator<'g, Gen> {
    grammar: &'g GrammarHir,
    action_table: ActionTable,
    _p: PhantomData<Gen>,
}

struct Actions<'g> {
    grammar: &'g GrammarHir,
    shifts: HashMap<u32, HashSet<ShiftAction>>,
    reduces: HashMap<u32, HashSet<ReduceAction>>,
    accepts: HashMap<u32, AcceptAction>,
}

impl<'g> Actions<'g> {
    fn new(
        grammar: &'g GrammarHir,
    ) -> Self {
        Self {
            grammar,
            shifts: Default::default(),
            reduces: Default::default(),
            accepts: Default::default(),
        }
    }
}

/// generates action branches
impl ToTokens for Actions<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for (&from, actions) in self.shifts.iter() {
            GenShift { from, actions }.to_tokens(tokens)
        }

        for (&from, reduces) in self.reduces.iter() {
            for reduce in reduces {
                GenReduce {
                    rule: self.grammar.rule(reduce.rule),
                    from,
                    symbol: reduce.symbol,
                }
                .to_tokens(tokens);
            }
        }

        for (&from, accept) in self.accepts.iter() {
            GenAccept {
                from,
                rule: self.grammar.rule(accept.rule),
            }.to_tokens(tokens)
        }
    }
}

impl<'g, Gen: GenAlg> ToTokens for LRGenerator<'g, Gen> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self.generate() {
            Ok(tt) => tt.to_tokens(tokens),
            Err(e) => tokens.extend(e.to_compile_error()),
        }
    }
}

impl<'g, Gen: GenAlg> LRGenerator<'g, Gen> {
    pub fn new(grammar: &'g GrammarHir) -> syn::Result<Self> {
        // TODO: cleanup
        let action_table = match grammar.grammar().action_table::<Gen>() {
            Ok(t) => t,
            Err(ActionTableError::UnhandledShiftReduce { rule1, rule2 }) => {
                let rule1 = &grammar.rules()[rule1];
                let rule2 = &grammar.rules()[rule2];
                let mut tokens = Default::default();
                rule1.rhs_tokens(&mut tokens);
                let mut tokens2 = Default::default();
                rule2.rhs_tokens(&mut tokens2);
                return Err(syn::Error::new_spanned(
                    &tokens,
                    format_args!(
                        "shift/reduce conflict between:\n\t- {}\n\t- {}",
                        tokens, tokens2
                    ),
                ));
            }
            Err(ActionTableError::UnhandledReduceReduce { rule2, rule1 }) => {
                let rule1 = &grammar.rules()[rule1];
                let rule2 = &grammar.rules()[rule2];
                let mut tokens = Default::default();
                rule1.rhs_tokens(&mut tokens);
                let mut tokens2 = Default::default();
                rule2.rhs_tokens(&mut tokens2);
                return Err(syn::Error::new_spanned(
                    &tokens,
                    format_args!(
                        "reduce/reduce conflict between:\n\t- {}\n\t- {}",
                        tokens, tokens2
                    ),
                ));
            }
        };

        Ok(Self {
            grammar,
            action_table,
            _p: PhantomData,
        })
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
        let goto_fns = self.gen_goto_funs();
        Ok(quote! {
            enum StackItem {
                State(u32),
                Token(paresse::Token),
                Node(NonTerminals),
            }

            #goto_fns
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

    fn gen_rules(&self) -> syn::Result<impl ToTokens + '_> {
        let mut actions = Actions::new(self.grammar);

        for cci in 0..self.action_table.num_states() as u32 {
            let acts = self.action_table.actions(cci);
            for (_s, act) in acts {
                match act {
                    Action::Shift(s) => {
                        actions.shifts.entry(cci).or_default().insert(s);
                    }
                    Action::Reduce(r) => {
                        actions.reduces.entry(cci).or_default().insert(r);
                    }
                    Action::Accept(a) => {
                        actions.accepts.insert(cci, a);
                    }
                    Action::Error => (),
                }
            }
        }

        Ok(actions)
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

    fn gen_goto_funs(&self) -> impl ToTokens {
        let nts = self.grammar.grammar().non_terminals();
        let fns = nts.iter().map(|nt| {
            let trans = self
                .action_table
                .goto(nt)
                .map(|(from, to)| quote! { #from => #to });

            let name = format_ident!("__goto_{}", nt.as_u32());
            quote! {
                fn #name(state: u32) -> u32 {
                    match state {
                        #(#trans,)*
                        invalid => unreachable!("invalid transition {invalid}"),
                    }
                }
            }
        });

        quote! {
            #(#fns)*

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
        let goto_fn = format_ident!("__goto_{}", self.rule.lhs().sym_id.as_u32());
        quote! {
            parser.stack.push(StackItem::State(#goto_fn(state)));
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

struct GenShift<'a> {
    from: u32,
    actions: &'a HashSet<ShiftAction>,
}

impl ToTokens for GenShift<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let s = self.actions.iter().map(|a| a.symbol.as_u32() as u16);
        let to = self.actions.iter().map(|a| {
            let s = a.symbol.as_u32() as u16;
            let to = a.state;
            quote! { #s => #to }
        });
        let from = self.from;
        quote! {
            (#from, Some(s @ (#(#s)|*))) => {
                // shift
                let current = self.advance().unwrap();
                self.stack.push(StackItem::Token(current));
                self.stack.push(StackItem::State(match s {
                    #(#to,)*
                    _ => unreachable!(),
                }));
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
