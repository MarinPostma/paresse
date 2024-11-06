//! This modules parses the content of the grammar macro into a raw GrammarAst, that can then be
//! analyzed into a GrammarHir

use paresse_core::grammar::Assoc;
use quote::ToTokens;
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::token::Brace;
use syn::{braced, Attribute, Expr, ExprLit, Ident, Lit, LitStr, MetaNameValue, Token};

use crate::config::{Config, ParserFlavor};
use crate::generate::parsers::dummy_lr1;

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

struct SpannedSymbolKind {
    tokens: proc_macro2::TokenStream,
    kind: SymbolKind,
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

impl Parse for SpannedSymbolKind {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut tokens = Default::default();
        let lookahead = input.lookahead1();
        if lookahead.peek(LitStr) {
            let pat = input.parse::<LitStr>()?.value();
            pat.to_tokens(&mut tokens);
            if pat.is_empty() {
                Ok(Self {
                    tokens,
                    kind: SymbolKind::Terminal(TerminalKind::Epsilon),
                })
            } else {
                Ok(Self {
                    tokens,
                    kind: SymbolKind::Terminal(TerminalKind::Pattern(pat)),
                })
            }
        } else if lookahead.peek(Ident) {
            let nt = input.parse::<Ident>()?;
            nt.to_tokens(&mut tokens);
            Ok(Self {
                tokens,
                kind: SymbolKind::Nonterminal(nt),
            })
        } else {
            panic!()
        }
    }
}

pub struct Symbol {
    binding: Option<Binding>,
    kind: SymbolKind,
    pub tokens: proc_macro2::TokenStream,
}

#[derive(Clone, Debug)]
pub struct Binding {
    name: Ident,
    mutable: bool,
}

impl Binding {
    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    fn new(name: Ident, mutable: bool) -> Self {
        Self { name, mutable }
    }
}

impl Symbol {
    pub(crate) fn kind(&self) -> &SymbolKind {
        &self.kind
    }

    pub(crate) fn binding(&self) -> Option<&Binding> {
        self.binding.as_ref()
    }
}

impl Parse for Symbol {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut tokens = Default::default();
        let lookahead = input.lookahead1();
        // this is a named binding
        if lookahead.peek(Token![<]) {
            input.parse::<Token![<]>()?.to_tokens(&mut tokens);
            let mutable = if input.peek(Token![mut]) {
                input.parse::<Token![mut]>()?.to_tokens(&mut tokens);
                true
            } else {
                false
            };

            let name = input.parse::<Ident>()?;
            name.to_tokens(&mut tokens);
            input.parse::<Token![:]>()?.to_tokens(&mut tokens);
            let SpannedSymbolKind { tokens: tt, kind } = input.parse::<SpannedSymbolKind>()?;
            tokens.extend(tt);
            input.parse::<Token![>]>()?.to_tokens(&mut tokens);
            Ok(Self {
                binding: Some(Binding::new(name, mutable)),
                kind,
                tokens,
            })
        } else if lookahead.peek(Ident) || lookahead.peek(LitStr) {
            let SpannedSymbolKind { tokens: tt, kind } = input.parse::<SpannedSymbolKind>()?;
            tokens.extend(tt);
            Ok(Self {
                binding: None,
                kind,
                tokens,
            })
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

/// Rhs arm in an alt definition:
/// <Lhs> = {
///     [#[rule(...)]]   <,
///     syms... => ...,  <+-- this
/// };
struct AltRhs {
    attr: Option<RuleAttrs>,
    rhs: Rhs,
}

impl Parse for AltRhs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let attr = if input.peek(Token![#]) {
            Some(parse_rule_config(input)?)
        } else {
            None
        };

        Ok(Self {
            attr,
            rhs: input.parse()?,
        })
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
    attr: Option<RuleAttrs>,
}

impl Rule {
    fn new(lhs: Ident, rhs: Rhs, attr: Option<RuleAttrs>) -> Self {
        Self { lhs, rhs, attr }
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

    /// A rule that is in the form rule = pat, where rule is all uppercase can be interpretted as a
    /// named token definition, and there is no handler
    pub fn matches_terminal_definition(&self) -> bool {
        let is_rhs_pattern =
            self.rhs().syms().len() == 1 && self.rhs().syms().first().unwrap().kind().is_terminal();
        is_rhs_pattern && self.handler().is_none()
    }

    /// A rule that is in the form RULE = pat, where rule is all uppercase is interpretted as a
    /// named token definition, and there is no handler
    pub fn is_named_terminal_definition(&self) -> bool {
        let is_lhs_uppercase = self.lhs().to_string().chars().all(|c| c.is_uppercase());
        is_lhs_uppercase && self.matches_terminal_definition()
            || self.attr().map(RuleAttrs::is_token).unwrap_or_default()
    }

    pub fn attr(&self) -> Option<&RuleAttrs> {
        self.attr.as_ref()
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

#[derive(Debug, Clone)]
pub struct RuleAttr {
    pub assoc: Assoc,
    pub prec: Option<usize>,
    pub priority: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct TokenAttr {
    pub assoc: Assoc,
    pub prec: Option<usize>,
}

#[derive(Debug, Clone)]
pub enum RuleAttrs {
    Token(TokenAttr),
    Rule(RuleAttr),
}

impl RuleAttrs {
    pub(crate) fn as_rule(&self) -> Option<&RuleAttr> {
        match self {
            RuleAttrs::Token(_) => None,
            RuleAttrs::Rule(ref r) => Some(r),
        }
    }

    pub(crate) fn as_token(&self) -> Option<&TokenAttr> {
        match self {
            RuleAttrs::Token(ref t) => Some(t),
            RuleAttrs::Rule(_) => None,
        }
    }

    /// Returns `true` if the rule attrs is [`Token`].
    ///
    /// [`Token`]: RuleAttrs::Token
    #[must_use]
    pub fn is_token(&self) -> bool {
        matches!(self, Self::Token(..))
    }
}

fn parse_rule_config(input: syn::parse::ParseStream) -> syn::Result<RuleAttrs> {
    let attr = input.call(Attribute::parse_outer)?;
    match &attr[0].meta {
        syn::Meta::List(l)
            if l.path.segments.len() == 1 && l.path.segments.first().unwrap().ident == "rule" =>
        {
            let entries = l.parse_args_with(
                Punctuated::<MetaNameValue, Token![,]>::parse_separated_nonempty,
            )?;

            let mut prec: Option<usize> = None;
            let mut assoc = Assoc::Left;
            let mut priority: Option<usize> = None;

            for entry in entries {
                let name = entry.path.to_token_stream().to_string();
                match name.as_str() {
                    "prec" => match entry.value {
                        Expr::Lit(ExprLit {
                            lit: Lit::Int(i), ..
                        }) => {
                            prec = Some(i.base10_parse().unwrap());
                        }
                        _ => {
                            return Err(syn::Error::new_spanned(
                                &entry.value,
                                format_args!("prec must be a literal integer"),
                            ))
                        }
                    },
                    "assoc" => match entry.value {
                        Expr::Path(p) => {
                            assoc = match p.to_token_stream().to_string().as_str() {
                                "left" => Assoc::Left,
                                "right" => Assoc::Right,
                                _ => {
                                    return Err(syn::Error::new_spanned(
                                        p,
                                        "assoc must be `left` or `right`",
                                    ))
                                }
                            };
                        }
                        _ => todo!(),
                    },
                    "priority" => match entry.value {
                        Expr::Lit(ExprLit {
                            lit: Lit::Int(i), ..
                        }) => {
                            priority = Some(i.base10_parse().unwrap());
                        }
                        _ => {
                            return Err(syn::Error::new_spanned(
                                &entry.value,
                                format_args!("priority must be a literal integer"),
                            ))
                        }
                    },
                    _ => {
                        return Err(syn::Error::new_spanned(
                            &entry.value,
                            format_args!(
                                "unrecognized rule attribute: {}",
                                entry.path.to_token_stream()
                            ),
                        ))
                    }
                }
            }

            Ok(RuleAttrs::Rule(RuleAttr { prec, assoc, priority }))
        }
        syn::Meta::List(l)
            if l.path.segments.len() == 1 && l.path.segments.first().unwrap().ident == "token" =>
        {
            let entries = l.parse_args_with(
                Punctuated::<MetaNameValue, Token![,]>::parse_separated_nonempty,
            )?;

            let mut prec: Option<usize> = None;
            let mut assoc = Assoc::Left;
            for entry in entries {
                let name = entry.path.to_token_stream().to_string();
                match name.as_str() {
                    "prec" => match entry.value {
                        Expr::Lit(ExprLit {
                            lit: Lit::Int(i), ..
                        }) => {
                            prec = Some(i.base10_parse().unwrap());
                        }
                        _ => {
                            return Err(syn::Error::new_spanned(
                                &entry.value,
                                format_args!("prec must be a literal integer"),
                            ))
                        }
                    },
                    "assoc" => match entry.value {
                        Expr::Path(p) => {
                            assoc = match p.to_token_stream().to_string().as_str() {
                                "left" => Assoc::Left,
                                "right" => Assoc::Right,
                                _ => {
                                    return Err(syn::Error::new_spanned(
                                        p,
                                        "assoc must be `left` or `right`",
                                    ))
                                }
                            };
                        }
                        _ => todo!(),
                    },
                    _ => {
                        return Err(syn::Error::new_spanned(
                            &entry.value,
                            format_args!(
                                "unrecognized rule attribute: {}",
                                entry.path.to_token_stream()
                            ),
                        ))
                    }
                }
            }
            Ok(RuleAttrs::Token(TokenAttr { prec, assoc }))
        }
        _ => Err(syn::Error::new_spanned(
            &attr[0],
            "rule config in the form `#[rule(..)]` or `#[token(..)]`",
        )),
    }
}

fn parse_rule(input: syn::parse::ParseStream, rules: &mut Vec<Rule>) -> syn::Result<()> {
    let rule_attr = if input.peek(Token![#]) {
        Some(parse_rule_config(input)?)
    } else {
        None
    };

    let lhs = input.parse::<Ident>()?;
    input.parse::<Token![=]>()?;
    let lookahead = input.lookahead1();
    if lookahead.peek(Brace) {
        if rule_attr.is_some() {
            panic!("alt rules attributes must be defined on the branches");
        }
        let content;
        braced!(content in input);
        let rhss = content.parse_terminated(|s| s.parse::<AltRhs>(), Token![,])?;
        for AltRhs { attr, rhs } in rhss {
            rules.push(Rule::new(lhs.clone(), rhs, attr));
        }
    } else {
        let rule = Rule::new(lhs.clone(), input.parse()?, rule_attr);
        if rule.matches_terminal_definition()
            && !rule.attr().map(RuleAttrs::is_token).unwrap_or(true)
        {
            return Err(syn::Error::new_spanned(lhs.clone(), format_args!("{lhs} doesn't match a token definition rule. Token definition rules are in the form <name> = <pat>")));
        }
        rules.push(rule);
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
                            if ["ll1", "lr1", "dummy_lr1"]
                                .contains(&p.to_token_stream().to_string().as_str()) =>
                        {
                            match dbg!(p.to_token_stream().to_string().as_str()) {
                                "ll1" => config.parser_flavor = ParserFlavor::Ll1,
                                "lr1" => config.parser_flavor = ParserFlavor::Lr1,
                                "dummy_lr1" => config.parser_flavor = ParserFlavor::DummyLr1,
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
