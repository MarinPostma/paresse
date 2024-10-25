
type Num = u64;

#[derive(Debug)]
enum Expr {
    Num(u64),
    Mult(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
    Plus(Box<Self>, Box<Self>),
    Minus(Box<Self>, Box<Self>),
}

impl Expr {
    fn merge(lhs: Term, rhs: Exprp) -> Self {
        let lhs: Box<Expr> = lhs.into();

        match rhs {
            Exprp::Plus(t, e) => { 
                let rhs = t.into();
                let lhs = Self::Plus(lhs, rhs);
                Self::merge(Term::Expr(lhs.into()), *e)
            },
            Exprp::Minus(t, e) => {
                let rhs = t.into();
                let lhs = Self::Minus(lhs, rhs);
                Self::merge(Term::Expr(lhs.into()), *e)
            },
            Exprp::None => *lhs,
        }
    }

    fn eval(&self) -> u64 {
        match self {
            Expr::Num(u) => *u,
            Expr::Mult(l, r) => l.eval() * r.eval(),
            Expr::Div(l, r) => l.eval() / r.eval(),
            Expr::Plus(l, r) => l.eval() + r.eval(),
            Expr::Minus(l, r) => l.eval() - r.eval(),
        }
    }
}

#[derive(Debug, Default)]
enum Exprp {
    Plus(Term, Box<Self>),
    Minus(Term, Box<Self>),
    #[default]
    None,
}

#[derive(Debug)]
struct Start(Expr);

#[derive(Debug)]
enum Term {
    Expr(Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

impl From<Term> for Box<Expr> {
    fn from(value: Term) -> Self {
        match value {
            Term::Expr(e) => e,
            Term::Mult(l, r) => Expr::Mult(l, r).into(),
            Term::Div(l, r) => Expr::Div(l, r).into(),
        }
    }
}

impl Term {
    fn merge(lhs: Factor, rhs: Termp) -> Self {
        let lhs: Box<Expr> = lhs.into();

        let merge = |f, t| {
            match Self::merge(f, t) {
                Term::Expr(e) => e,
                Term::Mult(lhs, rhs) => Box::new(Expr::Mult(lhs, rhs)),
                Term::Div(lhs, rhs) => Box::new(Expr::Div(lhs, rhs)),
            }
        };

        match rhs {
            Termp::Mult(f, t) => { 
                let rhs = f.into();
                let f = Factor::Expr(Expr::Mult(lhs, rhs).into());
                Self::merge(f, *t)
            },
            Termp::Div(f, t) => {
                let rhs = f.into();
                let f = Factor::Expr(Expr::Div(lhs, rhs).into());
                Self::merge(f, *t)
            },
            Termp::None => Term::Expr(lhs),
        }
    }
}

#[derive(Default, Debug)]
enum Termp {
    Mult(Factor, Box<Self>),
    Div(Factor, Box<Self>),
    #[default]
    None,
}

#[derive(Debug)]
enum Factor {
    Num(u64),
    Expr(Box<Expr>),
}

impl From<Factor> for Box<Expr> {
    fn from(value: Factor) -> Self {
        match value {
            Factor::Num(n) => Box::new(Expr::Num(n)),
            Factor::Expr(e) => e,
        }
    }
}

// TODO:
// - pass token content, instead of spanned
// - Detect non-terminal reference and return an error 
// - error handling
// - Named terminals
// - add positional args?
// - explicitely pass start symbol
// - parser config
// - support pattern in bindings: e.g <Expr { op, lhs, rhs }:Expr>, or maybe infer type?
// - type inference? see lalrpop
// - return meaningful error if grammar is not predictive

paresse::grammar! {
    Start = <e:Expr> => Start(e);
    Expr = <t:Term> <e:Exprp> => Expr::merge(t, e);
    Exprp = {
        "+" <t:Term> <e:Exprp> => Exprp::Plus(t, e.into()),
        "-" <t:Term> <e:Exprp> => Exprp::Minus(t, e.into()),
        "",
    };
    // todo pass token str instead
    Term = <f:Factor> <t:Termp> => Term::merge(f, t);
    Termp = {
        "*" <f:Factor> <t:Termp> => Termp:: Mult(f, t.into()),
        "/" <f:Factor> <t:Termp> => Termp::Div(f, t.into()),
        "",
    };
    Factor = {
        <n:Num> => Factor::Num(n),
        "\\(" <e:Expr> "\\)" => Factor::Expr(e.into()),
    };
    Num = <n:"[0-9]+"> => n.token.parse().unwrap();
}

fn main() {
    let expr = std::env::args().skip(1).collect::<String>();
    let res = parser::Parser::parse(&expr).0.eval();
    println!("{expr} = {res}");
}
