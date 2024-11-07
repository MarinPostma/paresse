use syn::Ident;

#[derive(Debug, Default)]
pub struct Config {
    pub parser_flavor: ParserFlavor,
    pub goal: Option<Ident>,
    /// Whether to generator a dummy parser
    pub dummy: bool,
}

/// The the type of parser to generate
#[derive(Debug, Default)]
pub enum ParserFlavor {
    /// A direct-coded ll(1) parser
    #[default]
    Ll1,
    /// A direct-coded lr(1) parser
    Lr1,
    Lalr1,
}
