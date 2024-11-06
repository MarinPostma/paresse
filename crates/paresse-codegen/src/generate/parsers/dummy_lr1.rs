use paresse_core::grammar::ActionTableError;
use quote::format_ident;
use quote::quote;
use quote::ToTokens;

use crate::hir::GrammarHir;

/// Dummy LR1 doesn't emit code for all the states, but only for the handlers, as well as reporting
/// conctruction errors. The generated parser, of course, doesn't work, but it is usefull during
/// developpement as it compiles fast. It allows the LSP to keep up, for example, so one can
/// iterate quickly on writing rules. When you actually need to generate the parser, switch to an
/// actual parser instead.
pub struct DummyLR1Generator<'g> {
    grammar: &'g GrammarHir,
}

impl<'g> ToTokens for DummyLR1Generator<'g> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self.generate() {
            Ok(tt) => tt.to_tokens(tokens),
            Err(e) => tokens.extend(e.to_compile_error()),
        }
    }
}

impl<'g> DummyLR1Generator<'g> {
    pub fn new(grammar: &'g GrammarHir) -> syn::Result<Self> {
        Ok(Self { grammar })
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
                    format_args!(
                        "shift/reduce conflict between:\n\t- {}\n\t- {}",
                        tokens, tokens2
                    ),
                ));
            }
            Err(ActionTableError::UnhandledReduceReduce { rule2, rule1 }) => {
                let rule1 = &self.grammar.rules()[rule1];
                let rule2 = &self.grammar.rules()[rule2];
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

        let handlers = actions.filter_map(|(l, a)| {
            let rule_id = match a {
                paresse_core::grammar::Action::Reduce { rule, .. } => rule,
                paresse_core::grammar::Action::Accept { rule } => rule,
                _ => return None,
            };
            let rule = self.grammar.rule(rule_id);
            let bindings = rule.rhs().iter().filter(|r| r.binding.is_some()).map(|r| {
                let n = r.binding.as_ref().unwrap().name();
                let ty = match r.sym.ty() {
                    Some(ty) => quote! { #ty },
                    None => quote! { &str },
                };

                let mutable = if r.binding.as_ref().unwrap().is_mutable() {
                    quote! { mut }
                } else {
                    quote! {}
                };

                quote! { let #mutable #n: #ty = __make_value(); }
            });
            let handler = match rule.handler {
                None => quote! { std::default::Default::default() },
                Some(ref e) => {
                    quote! { #e }
                }
            };

            let fn_name = format_ident!("__handler_{rule_id}_{cci}_{}", l.as_u32());
            let ret_ty = &rule.lhs().name;
            Some(quote! {
                fn #fn_name() -> #ret_ty {
                    #(#bindings)*
                    #handler
                }
            })
        });

        Ok(quote! {
            #(#handlers)*
        })
    }

    pub fn generate(&self) -> syn::Result<impl ToTokens> {
        let goal_ty = self
            .grammar
            .non_terminals()
            .get_ident(self.grammar.grammar().goal())
            .unwrap();
        let rules = self.gen_rules()?;

        Ok(quote::quote! {
            pub struct Parser<'input> { p: std::marker::PhantomData<&'input ()> }

            impl<'input> Parser<'input> {
                pub fn parse(s: &'input str) -> #goal_ty {
                    unimplemented!("this is a dummy parser, generate a LR1 parser instead")
                }
            }

            fn __make_value<A>() -> A {
                unreachable!()
            }

            #rules
        })
    }
}
