//! This modules parses the content of the grammar macro into a raw GrammarAst, that can then be
//! analyzed into a GrammarHir
use std::cell::OnceCell;

use quote::ToTokens;
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::token::Brace;
use syn::{braced, Attribute, Expr, Ident, LitStr, MetaNameValue, Token};

use crate::config::{Config, ParserFlavor};

#[derive(Clone, Debug)]
pub enum TerminalKind {
    /// matches the empty string
    Epsilon,
    /// matches a regex pattern
    Pattern(String),
}

impl TerminalKind {
    pub fn as_pattern(&self) -> Option<&String> {
        if let Self::Pattern(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

pub enum SymbolKind {
    Terminal(TerminalKind),
    Nonterminal(Ident),
}

impl SymbolKind {
    /// Returns `true` if the symbol kind is [`Terminal`].
    ///
    /// [`Terminal`]: SymbolKind::Terminal
    #[must_use]
    pub fn is_terminal(&self) -> bool {
        matches!(self, Self::Terminal(..))
    }

    pub fn as_terminal(&self) -> Option<&TerminalKind> {
        if let Self::Terminal(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl Parse for SymbolKind {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(LitStr) {
            let pat = input.parse::<LitStr>()?.value();
            if pat.is_empty() {
                Ok(Self::Terminal(TerminalKind::Epsilon))
            } else {
                Ok(Self::Terminal(TerminalKind::Pattern(pat)))
            }
        } else if lookahead.peek(Ident) {
            let nt = input.parse::<Ident>()?;
            Ok(Self::Nonterminal(nt))
        } else {
            panic!()
        }
    }
}

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

impl Parse for Rhs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut syms = Vec::new();

        loop {
            let lookahead = input.lookahead1();
            if lookahead.peek(LitStr) || lookahead.peek(Token![<]) || lookahead.peek(Ident) {
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

pub struct Rule {
    lhs: Ident,
    rhs: Rhs,
    is_named_terminal_definition: OnceCell<bool>,
}

impl Rule {
    fn new(lhs: Ident, rhs: Rhs) -> Self {
        Self {
            lhs,
            rhs,
            is_named_terminal_definition: Default::default(),
        }
    }

    pub(crate) fn lhs(&self) -> &Ident {
        &self.lhs
    }

    pub(crate) fn rhs(&self) -> &Rhs {
        &self.rhs
    }

    pub(crate) fn handler(&self) -> Option<&Expr> {
        self.rhs.handler.as_ref()
    }

    /// A rule that is in the form RULE = pat, where rule is all uppercase is interpretted as a
    /// named token definition
    pub fn is_named_terminal_definition(&self) -> bool {
        *self.is_named_terminal_definition.get_or_init(|| {
            let is_lhs_uppercase = self.lhs().to_string().chars().all(|c| c.is_uppercase());
            let is_rhs_pattern = self.rhs().syms().len() == 1
                && self.rhs().syms().first().unwrap().kind().is_terminal();
            is_lhs_uppercase && is_rhs_pattern
        })
    }
}

pub struct GrammarAst {
    config: Config,
    rules: Vec<Rule>,
}

impl GrammarAst {
    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }

    pub fn config(&self) -> &Config {
        &self.config
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
            rules.push(Rule::new(lhs.clone(), rhs));
        }
    } else {
        rules.push(Rule::new(lhs, input.parse()?));
    }

    input.parse::<Token![;]>()?;

    Ok(())
}

// TODO: cleanup this function
fn parse_config(input: syn::parse::ParseStream, config: &mut Config) -> syn::Result<()> {
    let attr = input.call(Attribute::parse_inner)?;
    match &attr[0].meta {
        syn::Meta::List(l) => {
            if l.path.segments.len() != 1 || l.path.segments.first().unwrap().ident != "config" {
                return Err(syn::Error::new_spanned(
                    &l.path,
                    "expect the attribute to be `config`",
                ));
            }

            let entries = l.parse_args_with(
                Punctuated::<MetaNameValue, Token![,]>::parse_separated_nonempty,
            )?;

            for entry in entries {
                match entry.path.to_token_stream().to_string().as_str() {
                    "parser_flavor" => match entry.value {
                        Expr::Path(p)
                            if ["ll1", "lr1"]
                                .contains(&p.to_token_stream().to_string().as_str()) =>
                        {
                            match p.to_token_stream().to_string().as_str() {
                                "ll1" => config.parser_flavor = ParserFlavor::Ll1,
                                "lr1" => config.parser_flavor = ParserFlavor::Lr1,
                                _ => unreachable!(),
                            }
                        }
                        _ => {
                            return Err(syn::Error::new_spanned(
                                &entry.value,
                                format_args!("parser_flavor must be one of `ll1`, `lr1`"),
                            ))
                        }
                    },
                    "goal" => match entry.value {
                        Expr::Path(p) if p.path.segments.len() == 1 => {
                            config.goal = Some(p.path.segments.first().unwrap().ident.clone());
                        }
                        _ => {
                            return Err(syn::Error::new_spanned(
                                &entry.value,
                                format_args!(
                                    "goal must be the type name for a rule of the grammar"
                                ),
                            ))
                        }
                    },
                    o => {
                        return Err(syn::Error::new_spanned(
                            &entry.path,
                            format_args!("unknown config attribute `{o}`"),
                        ))
                    }
                }
            }
        }
        _ => {
            return Err(syn::Error::new_spanned(
                &attr[0],
                "global config must be in the form `#![config(..)]`",
            ))
        }
    }
    Ok(())
}

impl Parse for GrammarAst {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut rules = Vec::new();
        let mut config = Config::default();
        loop {
            if input.peek(Token![#]) && input.peek2(Token![!]) {
                parse_config(input, &mut config)?;
            } else if input.peek(Token![#]) || input.peek(Ident) {
                parse_rule(input, &mut rules)?;
            } else {
                break;
            }
        }

        Ok(Self { rules, config })
    }
}
