#![allow(dead_code)]
type Expr = u64;
type Term = u64;
type Num = u64;

// paresse::grammar! {
//     #![config(parser_flavor = lr1, goal = Expr)]
//     Num = <n:"[0-9]+"> => n.parse().unwrap();
//     Expr = {
//         Expr "+" Term,
//         Expr "-" Term,
//         Term,
//     };
//     Term = {
//         Term "*" Factor,
//     };
// }

type ArgList = u64;
type MoreArgs = u64;

paresse::grammar! {
    #![config(parser_flavor = lr1, goal = Factor)]
    Expr = Term Exprp;
    Exprp = {
        "+" Term Exprp,
        "-" Term Exprp,
        "",
    };
    Term = Factor Termp;
    Termp = {
        "\\*" Factor Termp,
        "/" Factor Termp,
        "",
    };
    Factor = {
        Name,
        Name "\\[" ArgList "\\]",
        Name "\\(" ArgList "\\)",
    };
    ArgList = Expr MoreArgs;
    MoreArgs = { 
        "." Expr MoreArgs,
        "",
    };
}

fn main() {
    println!("Hello, world!");
}
