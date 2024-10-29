#![allow(dead_code)]
type Goal = u64;
type Expr = u64;
type Term = u64;
type Num = u64;

paresse::grammar! {
    #![config(parser_flavor = lr1)]
    Num = <n:"[0-9]+"> => n.parse().unwrap();
    // Goal = Expr;
    // Expr = {
    //     Expr "+" Term,
    //     Expr "-" Term,
    //     Term,
    // };
    // Term = {
    //     Term "*" Factor,
    // };
}
fn main() {
    println!("Hello, world!");
}
