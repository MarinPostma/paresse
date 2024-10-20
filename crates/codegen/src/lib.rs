use proc_macro::TokenStream;
use parse::ParsedGrammar;
use syn::parse_macro_input;

mod parse;
mod grammar;

#[proc_macro]
pub fn parser_ll1(input: TokenStream) -> TokenStream {
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        println!("{}", info);
        hook(info)
    }));

    let raw_grammar = parse_macro_input!(input as ParsedGrammar);
    crate::grammar::GrammarBuilder::new(&raw_grammar).build();

    todo!()
}
