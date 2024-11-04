mod named_token {
    pub struct Goal(String);

    paresse::grammar! {
        Goal = <i:IDENT> => Goal(i.to_string());
        IDENT = <i:"[a-zA-Z][a-zA-Z0-9]*">;
    }

    #[test]
    fn named_token() {
        assert_eq!(parser::Parser::parse("hello").unwrap().0, "hello");
        assert_eq!(parser::Parser::parse("he11").unwrap().0, "he11");
        assert!(parser::Parser::parse("12he11").is_err());
    }
}

mod lr1_epsilon_rule {
    type Goal = Next;
    #[derive(Debug, PartialEq, Eq)]
    pub enum Next {
        None,
        Bar,
    }

    paresse::grammar! {
        #![config(parser_flavor = lr1)]
        Goal = "foo" <n:Next> => n;
        Next = {
            "" => Next::None,
            "bar" => Next::Bar,
        };
    }

    #[test]
    fn epsilon_rule() {
        assert_eq!(parser::Parser::parse("foo bar"), Next::Bar);
        assert_eq!(parser::Parser::parse("foo"), Next::None);
    }
}
