type Num = u64;
#[derive(Default, Debug)]
struct Expr;
#[derive(Default)]
struct Exprp;

#[derive(Default, Debug)]
struct Start;

#[derive(Default, Debug)]
struct Term;

#[derive(Default, Debug)]
struct Termp;

#[derive(Default, Debug)]
struct Factor;

// TODO:
// - pass token content, instead of spanned
// - Detect non-terminal reference and return an error 
// - error handling
// - Named terminals

codegen::grammar! {
    Start = Expr;
    Expr = Term Exprp;
    Exprp = {
        "+" Term Exprp,
        "-" Term Exprp
        "",
    };
    // todo pass token str instead
    Term = Factor Termp;
    Termp = {
        "*" Factor Termp,
        "/" Factor Termp,
        "",
    };
    Factor = {
        Num,
        "\\(" Expr "\\)",
    };
    Num = <n:"[0-9]+"> => n.token.parse().unwrap();
}

fn main() {
    dbg!(parser::Parser::parse("1 + 1"));
}
