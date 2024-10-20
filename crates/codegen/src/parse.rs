use core::fmt;

use quote::ToTokens;
use syn::{
    braced,
    parse::{Lookahead1, Parse},
    token::Brace,
    Expr, Ident, LitStr, Token,
};

#[derive(Debug)]
pub enum SymbolKind {
    Terminal(String),
    Nonterminal(Ident),
}

impl Parse for SymbolKind {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(LitStr) {
            let pat = input.parse::<LitStr>()?.value();
            Ok(Self::Terminal(pat))
        } else if lookahead.peek(Ident) {
            let nt = input.parse::<Ident>()?;
            Ok(Self::Nonterminal(nt))
        } else {
            panic!()
        }
    }
}

#[derive(Debug)]
pub struct Symbol {
    name: Option<Ident>,
    kind: SymbolKind,
}

impl Symbol {
    pub(crate) fn kind(&self) -> &SymbolKind {
        &self.kind
    }

    pub(crate) fn binding(&self) -> Option<&Ident> {
        self.name.as_ref()
    }
}

impl Parse for Symbol {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        // this is a named binding
        if lookahead.peek(Token![<]) {
            input.parse::<Token![<]>()?;
            let name = input.parse::<Ident>()?;
            input.parse::<Token![:]>()?;
            let kind = input.parse::<SymbolKind>()?;
            input.parse::<Token![>]>()?;
            Ok(Self {
                name: Some(name),
                kind,
            })
        } else if lookahead.peek(Ident) || lookahead.peek(LitStr) {
            let kind = input.parse::<SymbolKind>()?;
            Ok(Self { name: None, kind })
        } else {
            panic!("invalid symbol")
        }
    }
}

pub struct Rhs {
    syms: Vec<Symbol>,
    handler: Option<Expr>,
}

impl Rhs {
    pub fn syms(&self) -> &[Symbol] {
        &self.syms
    }
}

impl fmt::Debug for Rhs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Rhs")
            .field("syms", &self.syms)
            .field(
                "handler",
                &self
                    .handler
                    .as_ref()
                    .map(|h| h.to_token_stream().to_string()),
            )
            .finish()
    }
}

impl Parse for Rhs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut syms = Vec::new();

        loop {
            let lookahead = input.lookahead1();
            if lookahead.peek(LitStr) || lookahead.peek(Token![<]) {
                let sym: Symbol = input.parse()?;
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
    lhs: Ident,
    rhs: Rhs,
}
impl Rule {
    pub(crate) fn lhs(&self) -> &Ident {
        &self.lhs
    }

    pub(crate) fn rhs(&self) -> &Rhs {
        &self.rhs
    }

    pub(crate) fn handler(&self) -> Option<&Expr> {
        self.rhs.handler.as_ref()
    }
}

#[derive(Debug)]
pub struct ParsedGrammar {
    rules: Vec<Rule>,
}

impl ParsedGrammar {
    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }
}

fn parse_rule(input: syn::parse::ParseStream, rules: &mut Vec<Rule>) -> syn::Result<()> {
    let lhs = input.parse::<Ident>()?;
    input.parse::<Token![=]>()?;
    let lookahead = input.lookahead1();
    if lookahead.peek(Brace) {
        let content;
        braced!(content in input);
        let rhss = content.parse_terminated(|s| s.parse::<Rhs>(), Token![,])?;
        for rhs in rhss {
            rules.push(Rule {
                lhs: lhs.clone(),
                rhs,
            });
        }
    } else {
        rules.push(Rule {
            lhs,
            rhs: input.parse()?,
        });
    }

    Ok(())
}

impl Parse for ParsedGrammar {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut rules = Vec::new();
        while input.peek(Ident) {
            parse_rule(input, &mut rules)?;
        }

        Ok(Self { rules })
    }
}
