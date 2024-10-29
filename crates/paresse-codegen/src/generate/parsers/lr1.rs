use quote::ToTokens;

use crate::hir::GrammarHir;

pub struct LR1Generator<'g> {
    grammar: &'g GrammarHir,
}

impl<'g> LR1Generator<'g> {
    pub fn new(grammar: &'g GrammarHir) -> syn::Result<Self> {
        Ok(Self { grammar })
    }

    pub fn generate(&self) -> impl ToTokens {
    }
}
