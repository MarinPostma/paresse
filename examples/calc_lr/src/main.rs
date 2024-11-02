#![allow(dead_code)]
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

use paresse::grammar;

type Goal = u64;
type List = u64;
type Pair = u64;

grammar! {
    #![config(parser_flavor = lr1)]
    Goal = List;
    List = {
        List Pair, Pair, };
    Pair = {
        "\\(" Pair "\\)",
        "\\(" "\\)", };
}

fn main() {
    let expr = std::env::args().skip(1).collect::<String>();
    parser::Parser::parse(&expr);
}
