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
    #![config(parser_flavor = lr1, goal = Expr)]
    Expr = Term;
}

fn main() {
    println!("Hello, world!");
}
