use proc_macro::TokenStream;
use raw_grammar::RawGrammar;
use syn::parse_macro_input;

mod raw_grammar;

#[proc_macro]
pub fn parser_ll1(input: TokenStream) -> TokenStream {
    let raw_grammar = parse_macro_input!(input as RawGrammar);
    todo!("{raw_grammar:?}")
}
