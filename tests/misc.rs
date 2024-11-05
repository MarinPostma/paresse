// mod named_token {
//     pub struct Goal(String);
//
//     paresse::grammar! {
//         Goal = <i:IDENT> => Goal(i.to_string());
//         IDENT = <i:"[a-zA-Z][a-zA-Z0-9]*">;
//     }
//
//     #[test]
//     fn named_token() {
//         assert_eq!(parser::Parser::parse("hello").unwrap().0, "hello");
//         assert_eq!(parser::Parser::parse("he11").unwrap().0, "he11");
//         assert!(parser::Parser::parse("12he11").is_err());
//     }
// }
//
// mod lr1_epsilon_rule {
//     type Goal = Next;
//     #[derive(Debug, PartialEq, Eq)]
//     pub enum Next {
//         None,
//         Bar,
//     }
//
//     paresse::grammar! {
//         #![config(parser_flavor = lr1)]
//         Goal = "foo" <n:Next> => n;
//         Next = {
//             "" => Next::None,
//             "bar" => Next::Bar,
//         };
//     }
//
//     #[test]
//     fn epsilon_rule() {
//         assert_eq!(parser::Parser::parse("foo bar"), Next::Bar);
//         assert_eq!(parser::Parser::parse("foo"), Next::None);
//     }
// }

mod ambiguous_grammar {
    type Goal = Expr;

    #[derive(Debug, PartialEq, Eq)]
    enum BinOp {
        Mult,
        Add,
        Div,
        And,
        Or,
    }

    #[derive(Debug, PartialEq, Eq)]
    enum Expr {
        Bin {
            op: BinOp,
            lhs: Box<Self>,
            rhs: Box<Self>,
        },
        Num(u64),
    }

    impl From<u64> for Expr {
        fn from(value: u64) -> Self {
            Self::Num(value)
        }
    }

    paresse::grammar! {
        #![config(parser_flavor = lr1)]

        Goal = <e:Expr> => e;

        Expr = {
            <mut lhs:Expr> ADD <rhs:Expr> => Expr::Bin { lhs: lhs.into(), rhs: rhs.into(), op: BinOp::Add },
            <mut lhs:Expr> MULT <rhs:Expr> => Expr::Bin { lhs: lhs.into(), rhs: rhs.into(), op: BinOp::Mult },
            <mut lhs:Expr> DIV <rhs:Expr> => Expr::Bin { lhs: lhs.into(), rhs: rhs.into(), op: BinOp::Div },
            <mut lhs:Expr> AND <rhs:Expr> => Expr::Bin { lhs: lhs.into(), rhs: rhs.into(), op: BinOp::And },
            <mut lhs:Expr> OR <rhs:Expr> => Expr::Bin { lhs: lhs.into(), rhs: rhs.into(), op: BinOp::Or },
            <n:"[0-9]+"> => Expr::Num(n.parse().unwrap()),
        };


        #[token(assoc = right, prec = 3)]
        AND = "&";
        #[token(assoc = right, prec = 3)]
        OR = "\\|";
        #[token(assoc = left, prec = 2)]
        MULT = "\\*";
        #[token(assoc = left, prec = 2)]
        DIV = "/";
        #[token(assoc = right, prec = 1)]
        ADD = "\\+";

    }

    #[test]
    fn shift_reduce_conflicts() {
        // we defined addition to be right associative
        assert_eq!(
            parser::Parser::parse("1 + 2 + 3"),
            Expr::Bin {
                op: BinOp::Add,
                lhs: Box::new(1.into()),
                rhs: Box::new(Expr::Bin {
                    op: BinOp::Add,
                    lhs: Box::new(2.into()),
                    rhs: Box::new(3.into())
                })
            }
        );

        // // we defined multiplication to be left associative
        assert_eq!(
            parser::Parser::parse("1 * 2 * 3"),
            Expr::Bin {
                op: BinOp::Mult,
                lhs: Box::new(Expr::Bin {
                    op: BinOp::Mult,
                    lhs: Box::new(1.into()),
                    rhs: Box::new(2.into())
                }),
                rhs: Box::new(3.into())
            }
        );
        // multiplication has a higher precedence than addition
        assert_eq!(
            parser::Parser::parse("1 * 2 + 3"),
            Expr::Bin {
                op: BinOp::Add,
                lhs: Box::new(Expr::Bin {
                    op: BinOp::Mult,
                    lhs: Box::new(1.into()),
                    rhs: Box::new(2.into())
                }),
                rhs: Box::new(3.into()),
            }
        );

        // // multiplication has a higher precedence than addition
        assert_eq!(
            parser::Parser::parse("1 + 2 * 3"),
            Expr::Bin {
                op: BinOp::Add,
                lhs: Box::new(1.into()),
                rhs: Box::new(Expr::Bin {
                    op: BinOp::Mult,
                    lhs: Box::new(2.into()),
                    rhs: Box::new(3.into())
                }),
            }
        );

        assert_eq!(
            parser::Parser::parse("1 * 2 / 3"),
            Expr::Bin {
                op: BinOp::Div,
                lhs: Box::new(Expr::Bin {
                    op: BinOp::Mult,
                    lhs: Box::new(1.into()),
                    rhs: Box::new(2.into())
                }),
                rhs: Box::new(3.into()),
            }
        );

        assert_eq!(
            parser::Parser::parse("1 / 2 * 3"),
            Expr::Bin {
                op: BinOp::Mult,
                lhs: Box::new(Expr::Bin {
                    op: BinOp::Div,
                    lhs: Box::new(1.into()),
                    rhs: Box::new(2.into())
                }),
                rhs: Box::new(3.into()),
            }
        );

        // same prio, but right associative
        assert_eq!(
            parser::Parser::parse("1 & 2 | 3"),
            Expr::Bin {
                op: BinOp::And,
                lhs: Box::new(1.into()),
                rhs: Box::new(Expr::Bin {
                    op: BinOp::Or,
                    lhs: Box::new(2.into()),
                    rhs: Box::new(3.into())
                }),
            }
        );
    }
}
