use parse::GrammarAst;
use proc_macro::TokenStream;
use syn::parse_macro_input;

mod config;
mod generate;
mod hir;
mod parse;

#[proc_macro]
pub fn grammar(input: TokenStream) -> TokenStream {
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        println!("{}", info);
        hook(info)
    }));

    let ast = parse_macro_input!(input as GrammarAst);
    match grammar_inner(ast) {
        Ok(out) => out,
        Err(e) => e.to_compile_error().into(),
    }
}

fn grammar_inner(ast: GrammarAst) -> syn::Result<TokenStream> {
    let grammar = crate::hir::GrammarBuilder::new(&ast).build()?;
    let lexer = generate::lexer::LexerGenerator::new(&grammar);
    let parser = generate::parsers::ll1::Ll1Generator::new(&grammar)?;

    Ok(quote::quote! {
        mod parser {
            #![allow(non_snake_case)]
            use super::*;
            #lexer
            #parser
        }
    }
    .into())
}
