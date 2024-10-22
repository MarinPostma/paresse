codegen::parser_ll1! {
    Item = "[a-zA-Z]+";
    IfKw = "foobar";
    Start = { Item, IfKw, };
}

fn main() {
    let s = Scan::new(r#"foobar"#);
    
    for s in s {
        dbg!(s);
    }
}
