use config::ParserFlavor;
use parse::GrammarAst;
use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse_macro_input;

mod config;
mod generate;
mod hir;
mod parse;

#[proc_macro]
pub fn grammar(input: TokenStream) -> TokenStream {
    // todo: check if grammar has changed before re-generating the code.
    // - either compute the hash of the grammar
    // - check file timestamp?
    // let input = proc_macro2::TokenStream::from(input);
    // let mut hasher = DefaultHasher::default();
    // dbg!(input.span().source_text()).hash(&mut hasher);
    // dbg!(hasher.finish());

    // update the hook for debug purposes, so that we get the location of a panic in the proc
    // macro. remove when the project is no more WIP
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
    let before = std::time::Instant::now();
    let grammar = crate::hir::GrammarBuilder::new(&ast).build()?;
    let lexer: &dyn ToTokens = match ast.config().parser_flavor {
        // skip generating a lexer for dummy parsers
        ParserFlavor::DummyLr1 => &quote::quote! {},
        _ => &generate::lexer::LexerGenerator::new(&grammar),
    };

    let parser: &dyn ToTokens = match ast.config().parser_flavor {
        ParserFlavor::Ll1 => &generate::parsers::ll1::LL1Generator::new(&grammar)?,
        ParserFlavor::Lr1 => {
            if let Ok(t) = grammar.grammar().lr1_action_table() {
                println!("generating {} states", t.num_states());
            }
            &generate::parsers::lr1::LR1Generator::new(&grammar)?
        }
        ParserFlavor::DummyLr1 => {
            if let Ok(t) = grammar.grammar().lr1_action_table() {
                println!("generating {} states", t.num_states());
            }
            &generate::parsers::dummy_lr1::DummyLR1Generator::new(&grammar)?
        }
    };

    println!("generated grammar in {:?}", before.elapsed());

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
