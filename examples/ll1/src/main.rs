codegen::parser_ll1! {
    Json = Element;
    Value = {
        Object,
        Array,
        String,
        Number,
        "true",
        "false",
    };
    Object = {
        "{" Ws "}",
        "{" Members "}",
    };
    Members = Ws String Ws ":" Element;
    Array = {
        "\\[" Ws "\\]",
        "\\[" Elements "\\]",
    };
    Elements = {
        Element,
        Element "," Elements,
    };
    Element = Ws Value Ws;
    String = "\"[^\"]*\"";
    Number = "[0-9]+";
}

fn main() {
    let s = Scan::new(r#"{"hello": 12, "bool": true}"#);
    
    for s in s {
        dbg!(s);
    }
}
