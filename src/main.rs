use regex::grammar::cfg::Builder;

// #[derive(Debug, Clone)]
// enum Token {
//     Create,
//     Table,
//     Select,
//     From,
//     String(String),
//     LParen,
//     RParen,
// }

fn main() {
    let mut builder = Builder::new();
    let [
        expr,
        exprp, 
        plus,
        minus, 
        term, 
        termp, 
        factor, 
        cross, 
        div, 
        rparen, 
        lparen, 
        num,
        name
    ] = builder.syms();
    let epsilon = builder.epsilon();

    builder.rule(expr).is([term, exprp]);

    builder
        .rule(exprp)
        .is([plus, term, exprp])
        .is([minus, term, exprp])
        .is([epsilon]);

    builder.rule(term).is([factor, termp]);

    builder
        .rule(termp)
        .is([cross, factor, termp])
        .is([div, factor, termp])
        .is([epsilon]);

    builder
        .rule(factor)
        .is([lparen, expr, rparen])
        .is([num])
        .is([name]);

    let grammar = builder.build(expr);
    let first = grammar.first_sets();
    let follow = grammar.follow_sets();
    let augmented= grammar.augmented_first_set(&first, &follow);

    dbg!(&augmented);
    dbg!(augmented.is_backtrack_free());
    // dbg!(grammar.terminals());
    // dbg!(grammar.non_terminals());
    // dbg!(grammar.first_set());
}

// fn scan() {
// let scanner = ScannerBuilder::new()
//     .skip("[\n \t]+")
//     .keyword_insensitive("select", Token::Select)
//     .keyword_insensitive("create", Token::Create)
//     .keyword_insensitive("table", Token::Table)
//     .keyword_insensitive("from", Token::From)
//     .keyword_insensitive("\\(", Token::LParen)
//     .keyword_insensitive("\\)", Token::RParen)
//     .token(r#""([^"]|\\")*""#, |span: Span| { Ok(Token::String(span.token().to_string()))})
//     .build();
//
// let mut tokens = scanner.scan(r#"create table "test"("hello")")"#);
//
// while let Some(token) = tokens.next() {
//     dbg!(token);
// }
// }
