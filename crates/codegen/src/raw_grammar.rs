use core::fmt;

use syn::{
    braced, parse::Parse, token::Brace, Expr, Ident, LitStr, Token
};
use quote::ToTokens;

#[derive(Debug)]
enum BindingTy {
    Re(String),
    Ty(String),
}

impl Parse for BindingTy {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(LitStr) {
            let s: LitStr = input.parse()?;
            Ok(Self::Re(s.value()))
        } else if lookahead.peek(Ident) {
            let s: Ident = input.parse()?;
            Ok(Self::Ty(s.to_string()))
        } else {
            panic!()
        }
    }
}

/// Binding in the form `<{name}:{ty}|{regex}>`
#[derive(Debug)]
pub struct Binding {
    pub name: String,
    pub ty: BindingTy,
}

impl Parse for Binding {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![<]>()?;
        let name = input.parse::<Ident>()?.to_string();
        input.parse::<Token![:]>()?;
        let ty: BindingTy = input.parse()?;
        input.parse::<Token![>]>()?;
        Ok(Self { name, ty })
    }
}

#[derive(Debug)]
pub enum Sym {
    /// a simple token in the form `"<expr>"`
    Token(String),
    Binding(Binding),
}

impl Parse for Sym {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(LitStr) {
            let s: LitStr = input.parse()?;
            Ok(Self::Token(s.value()))
        } else if lookahead.peek(Token![<]) {
            input.parse().map(Self::Binding)
        } else {
            panic!("invalid")
        }
    }
}

pub struct Rhs {
    syms: Vec<Sym>,
    handler: Option<Expr>,
}

impl fmt::Debug for Rhs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Rhs")
            .field("syms", &self.syms)
            .field("handler", &self.handler.as_ref().map(|h| h.to_token_stream().to_string()))
            .finish()
    }
}

impl Parse for Rhs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut syms = Vec::new();

        loop {
            let lookahead = input.lookahead1();
            if lookahead.peek(LitStr) || lookahead.peek(Token![<]) {
                let sym: Sym = input.parse()?;
                syms.push(sym);
            } else {
                break;
            }
        }

        let lookahead = input.lookahead1();
        let handler = if lookahead.peek(Token![=>]) {
            input.parse::<Token![=>]>()?;
            Some(input.parse::<Expr>()?)
        } else {
            None
        };

        Ok(Self { syms, handler })
    }
}

#[derive(Debug)]
pub struct Rule {
    lhs: String,
    rhs: Vec<Rhs>,
}

impl Parse for Rule {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lhs = input.parse::<Ident>()?.to_string();
        input.parse::<Token![=]>()?;
        let mut rhs = Vec::new();
        let lookahead = input.lookahead1();
        if lookahead.peek(Brace) {
            let content;
            braced!(content in input);
            let rhss = content.parse_terminated(|s| s.parse::<Rhs>(), Token![,])?;
            rhs.extend(rhss);
        } else {
            rhs.push(input.parse()?);
        }

        Ok(Self { lhs, rhs })
    }
}

#[derive(Debug)]
pub struct RawGrammar {
    rules: Vec<Rule>,
}

impl Parse for RawGrammar {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut rules = Vec::new();
        while input.peek(Ident) {
            rules.push(input.parse()?);
        }

        Ok(Self { rules })
    }
}
