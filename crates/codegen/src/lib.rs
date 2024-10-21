use proc_macro::TokenStream;
use parse::ParsedGrammar;
use quote::ToTokens;
use syn::parse_macro_input;

mod parse;
mod grammar;
mod generate;

#[proc_macro]
pub fn parser_ll1(input: TokenStream) -> TokenStream {
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        println!("{}", info);
        hook(info)
    }));

    let parsed = parse_macro_input!(input as ParsedGrammar);
    let grammar = crate::grammar::GrammarBuilder::new(&parsed).build();
    let lexer = generate::lexer::LexerGenerator::new(&grammar).generate();

    quote::quote! {
        #lexer
    }.into_token_stream().into()
}
