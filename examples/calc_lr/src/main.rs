type Expr = u64;
type Term = u64;
type Factor = u64;
type Num = u64;

paresse::grammar! {
    #![config(parser_flavor = lr1, goal = Expr)]
    Expr = {
        <e:Expr> "+" <t:Term> => e + t,
        <e:Expr> "-" <t:Term> => e - t,
        <t:Term> => t,
    };
    Term = {
        <t:Term> "*" <f:Factor> =>  t * f,
        <t:Term> "/" <f:Factor> => t / f,
        <f:Factor> => f,
    };
    Factor = {
        <n:Num> => n,
        "\\(" <e:Expr> "\\)" => e,
    };
    Num = <s:"[0-9]+"> => s.parse().unwrap();
}

fn main() {
    let expr = std::env::args().skip(1).collect::<String>();
    println!("{expr} = {}", parser::Parser::parse(&expr));
}
