#[test]
fn named_token() {
    struct IDENT(String);
    struct Goal(String);

    paresse::grammar! {
        Goal = <i:IDENT> => Goal(i.to_string());
        IDENT = <i:"[a-zA-Z][a-zA-Z0-9]*"> => IDENT(i.to_string());
    }
}
