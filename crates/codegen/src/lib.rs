use proc_macro::TokenStream;
use parse::GrammarAst;
use syn::parse_macro_input;

mod parse;
mod hir;
mod generate;

#[proc_macro]
pub fn grammar(input: TokenStream) -> TokenStream {
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        println!("{}", info);
        hook(info)
    }));

    let parsed = parse_macro_input!(input as GrammarAst);
    let grammar = crate::hir::GrammarBuilder::new(&parsed).build();
    let lexer = generate::lexer::LexerGenerator::new(&grammar);
    let parser = generate::parsers::ll1::Ll1Generator::new(&grammar);

    quote::quote! {
        mod parser {
            #![allow(non_snake_case)]
            use super::*;
            #lexer
            #parser
        }
    }.into()
}
