codegen::grammar! {
    Expr = Term Exprp;
    Exprp = {
        "+" Term Exprp,
        "-" Term Exprp,
        "",
    };
}

fn main() {
    // dbg!(parser::Parser::parse(""));
}
