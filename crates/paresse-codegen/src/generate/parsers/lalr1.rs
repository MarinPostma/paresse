use quote::ToTokens;
use quote::quote;

use crate::hir::GrammarHir;


pub struct Lalr1Generator<'g> {
    grammar: &'g GrammarHir,
}

impl<'g> ToTokens for Lalr1Generator<'g> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self.generate() {
            Ok(tt) => tt.to_tokens(tokens),
            Err(e) => tokens.extend(e.to_compile_error()),
        }
    }
}

impl<'g> Lalr1Generator<'g> {
    pub fn new(grammar: &'g GrammarHir) -> syn::Result<Self> {
        Ok(Self { grammar })
    }

    pub fn generate(&self) -> syn::Result<impl ToTokens> {
        Ok(quote! {})
    }
}
