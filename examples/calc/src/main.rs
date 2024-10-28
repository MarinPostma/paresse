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
        let lhs = lhs.0;

        fn merge(lhs: Box<Expr>, t: Term, e: Exprp, cons: fn(Box<Expr>, Box<Expr>) -> Expr) -> Expr {
            let rhs = t.0;
            let lhs = cons(lhs, rhs);
            Expr::merge(Term(lhs.into()), e)
        }

        match rhs {
            Exprp::Plus(t, e) => merge(lhs, t, *e, Self::Plus),
            Exprp::Minus(t, e) => merge(lhs, t, *e, Self::Minus),
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
struct Term(Box<Expr>);

impl Term {
    fn merge(lhs: Factor, rhs: Termp) -> Self {
        let lhs: Box<Expr> = lhs.into();

        fn merge(lhs: Box<Expr>, f: Factor, t: Termp, cons: fn(Box<Expr>, Box<Expr>) -> Expr) -> Term {
            let rhs = f.into();
            let f = Factor::Expr(cons(lhs, rhs).into());
            Term::merge(f, t)
        }

        match rhs {
            Termp::Mult(f, t) => merge(lhs, f, *t, Expr::Mult),
            Termp::Div(f, t) => merge(lhs, f, *t, Expr::Div),
            Termp::None => Self(lhs),
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
    Num = <n:"[0-9]+"> => n.parse().unwrap();
}

fn main() {
    let expr = std::env::args().skip(1).collect::<String>();
    let res = parser::Parser::parse(&expr).unwrap().0.eval();
    println!("{expr} = {res}");
}
