use std::iter::{Copied, Peekable};

use super::{
    alphabet::{ByteSet, Class},
    ast::RegexAst,
};

macro_rules! expect {
    ($tokens:expr, $t:literal) => {
        $tokens.next_if_eq(&$t).unwrap()
    };
}

type Tokens<'a> = Peekable<Copied<std::slice::Iter<'a, u8>>>;

#[derive(Debug, Default)]
pub struct Flags {
    case_insensitive: bool,
}

impl Flags {
    pub fn set_case_insensitive(mut self, insensitive: bool) -> Self {
        self.case_insensitive = insensitive;
        self
    }
}

#[derive(Debug, Default)]
pub struct Config {
    flags: Flags,
}

impl Config {
    pub fn set_flags(mut self, flags: Flags) -> Self {
        self.flags = flags;
        self
    }
}

pub struct Parser {
    config: Config,
}

///   <regex> ::= <term> '|' <regex>
///            |  <term>
///
///   <term> ::= { <factor> }
///
///   <factor> ::= <base> { '*' }
///             
///   <base> ::= <char>
///           | ^
///           | $
///           |  '\' <char>
///           |  '(' <regex> ')'  
impl Parser {
    pub(crate) fn new(config: Config) -> Self {
        Self { config }
    }

    pub(crate) fn parse(&self, pat: &str) -> RegexAst {
        let mut tokens = pat.as_bytes().iter().copied().peekable();
        self.parse_regex(&mut tokens).unwrap()
    }

    fn parse_regex(&self, tokens: &mut Tokens) -> Option<RegexAst> {
        let lhs = self.parse_term(tokens)?;
        if tokens.next_if_eq(&b'|').is_some() {
            let rhs = self.parse_regex(tokens).unwrap();
            Some(RegexAst::Alt(lhs.into(), rhs.into()))
        } else {
            Some(lhs)
        }
    }

    fn parse_term(&self, tokens: &mut Tokens) -> Option<RegexAst> {
        let mut lhs = self.parse_factor(tokens)?;
        while let Some(rhs) = self.parse_factor(tokens) {
            lhs = RegexAst::Concat(lhs.into(), rhs.into());
        }
        Some(lhs)
    }

    fn parse_factor(&self, tokens: &mut Tokens) -> Option<RegexAst> {
        let base = self.parse_base(tokens)?;
        if tokens.next_if_eq(&b'*').is_some() {
            Some(RegexAst::Kleene(base.into()))
        } else if tokens.next_if_eq(&b'+').is_some() {
            Some(RegexAst::Concat(
                base.clone().into(),
                Box::new(RegexAst::Kleene(base.into())),
            ))
        } else {
            Some(base)
        }
    }

    fn parse_base(&self, tokens: &mut Tokens) -> Option<RegexAst> {
        match tokens.peek()? {
            b'^' => {
                expect!(tokens, b'^');
                Some(RegexAst::Match(Class::Boi))
            }
            b'$' => {
                expect!(tokens, b'$');
                Some(RegexAst::Match(Class::Eoi))
            }
            b'(' => {
                expect!(tokens, b'(');
                let e = self.parse_regex(tokens).unwrap();
                expect!(tokens, b')');
                Some(e)
            }
            b'[' => {
                expect!(tokens, b'[');
                let range = self.parse_range(tokens).unwrap();
                expect!(tokens, b']');
                Some(RegexAst::Match(range))
            }
            b'|' | b')' => None,
            _ => {
                let b = self.parse_char(tokens).unwrap();
                let mut set = ByteSet::empty();
                self.add_byte_to_set(b, &mut set);
                Some(RegexAst::Match(Class::Chars(set)))
            }
        }
    }

    /// <range> = '^' <subranges> | <subranges>
    /// <subranges> = <subrange> <subranges> | eps
    /// <subrange> = <char> '-' <char> | char
    fn parse_range(&self, tokens: &mut Tokens) -> Option<Class> {
        let mut set = ByteSet::empty();
        let negate = tokens.next_if_eq(&b'^').is_some();

        while self.parse_subrange(tokens, &mut set).is_some() {}

        if negate {
            set.negate()
        }

        Some(Class::Chars(set))
    }

    fn parse_char(&self, tokens: &mut Tokens) -> Option<u8> {
        match tokens.next()? {
            b'\\' => tokens.next(),
            c => Some(c),
        }
    }

    fn parse_subrange(&self, tokens: &mut Tokens, set: &mut ByteSet) -> Option<()> {
        match tokens.peek()? {
            b']' => None,
            _ => {
                let start = self.parse_char(tokens).unwrap();
                if tokens.next_if_eq(&b'-').is_some() {
                    let end = self.parse_char(tokens).unwrap();
                    for b in start..=end {
                        self.add_byte_to_set(b, set);
                    }
                } else {
                    self.add_byte_to_set(start, set);
                    set.add(start);
                }
                Some(())
            }
        }
    }

    fn add_byte_to_set(&self, b: u8, set: &mut ByteSet) {
        if self.config.flags.case_insensitive {
            set.add(b.to_ascii_lowercase());
            set.add(b.to_ascii_uppercase());
        } else {
            set.add(b);
        }
    }
}
