pub struct Goal(String);

paresse::grammar! {
    Goal = <i:IDENT> => Goal(i.to_string());
    IDENT = <i:"[a-zA-Z][a-zA-Z0-9]*">;
}

#[test]
fn named_token() {
    assert_eq!(parser::Parser::parse("hello").unwrap().0, "hello");
    assert_eq!(parser::Parser::parse("he11").unwrap().0, "he11");
    assert!(parser::Parser::parse("12he11").is_err());
}
