codegen::parser_ll1! {
    Num = "(" <n:"[0-9]+"> ")" => {
        n.parse::<u64>().unwrap()
    }
    Expr = {
        <n:Num> => say_hello(),
        "struct" => its_a_struct(),
    }
}

fn main() {
}
