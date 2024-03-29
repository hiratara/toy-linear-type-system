use combine::char::{char, space, spaces, string};
use combine::easy::Errors;
use combine::error::ParseError;
use combine::stream::PointerOffset;
use combine::{attempt, many1, satisfy, skip_many1, unexpected_any, value, Parser, Stream};

fn spaces1_<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    skip_many1(space())
}

parser! {
    fn spaces1[I]()(I) -> ()
    where [I: Stream<Item = char>]
    {
        spaces1_()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Qualifier {
    Lin,
    Un,
}

fn qualifiers_p_<I>() -> impl Parser<Input = I, Output = Qualifier>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let lin = string("lin").with(value(Qualifier::Lin));
    let un = string("un").with(value(Qualifier::Un));
    lin.or(un)
}

parser! {
    fn qualifiers_p[I]()(I) -> Qualifier
    where [I: Stream<Item = char>]
    {
        qualifiers_p_()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Boolean {
    True,
    False,
}

fn booleans_p_<I>() -> impl Parser<Input = I, Output = Boolean>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let true_ = string("true").with(value(Boolean::True));
    let false_ = string("false").with(value(Boolean::False));
    true_.or(false_)
}

parser! {
    fn booleans_p[I]()(I) -> Boolean
    where [I: Stream<Item = char>]
    {
        booleans_p_()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable(String);

static RESERVED: &'static [&'static str] = &[
    "lin", "un", "true", "false", "if", "then", "else", "split", "as", "in", "Bool",
];

fn variable_p_<I>() -> impl Parser<Input = I, Output = Variable>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1::<String, _>(satisfy(|c: char| c.is_ascii_lowercase())).then(|var| {
        if RESERVED.iter().any(|&r| r == var) {
            unexpected_any("variable").left()
        } else {
            value(Variable(var)).right()
        }
    })
}

parser! {
    fn variable_p[I]()(I) -> Variable
    where [I: Stream<Item = char>]
    {
        variable_p_()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Variable(Variable),
    Boolean(Qualifier, Boolean),
    If(Box<Term>, Box<Term>, Box<Term>),
    Pair(Qualifier, Box<Term>, Box<Term>),
    Split(Box<Term>, Variable, Variable, Box<Term>),
    Abstraction(Qualifier, Variable, Type, Box<Term>),
    Application(Box<Term>, Box<Term>),
}

fn term_p_<I>() -> impl Parser<Input = I, Output = Term>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    enum Sum3<S, T, U> {
        A(S),
        B(T),
        C(U),
    }

    let variable = variable_p().map(Term::Variable);

    let boolean_tail = booleans_p();

    let if_ = (
        string("if").skip(spaces1()),
        term_p().skip(spaces1()),
        string("then").skip(spaces1()),
        term_p().skip(spaces1()),
        string("else").skip(spaces1()),
        term_p(),
    )
        .map(|t| Term::If(Box::new(t.1), Box::new(t.3), Box::new(t.5)));

    let pair_tail = (
        char('<').skip(spaces()),
        term_p().skip(spaces()),
        char(',').skip(spaces()),
        term_p().skip(spaces()),
        char('>'),
    );

    let split = (
        string("split").skip(spaces1()),
        term_p().skip(spaces1()),
        string("as").skip(spaces1()),
        variable_p().skip(spaces()),
        char(',').skip(spaces()),
        variable_p().skip(spaces1()),
        string("in").skip(spaces1()),
        term_p(),
    )
        .map(|t| Term::Split(Box::new(t.1), t.3, t.5, Box::new(t.7)));

    let abstraction_tail = (
        char('\\').skip(spaces()),
        variable_p().skip(spaces()),
        char(':').skip(spaces()),
        type_p().skip(spaces()),
        char('.').skip(spaces()),
        term_p(),
    );

    let paren = char('(')
        .skip(spaces())
        .with(term_p())
        .skip(spaces())
        .skip(char(')'));

    let boolean_or_pair_or_abstraction = qualifiers_p()
        .skip(spaces1())
        .and(
            boolean_tail
                .map(Sum3::A)
                .or(pair_tail.map(Sum3::B))
                .or(abstraction_tail.map(Sum3::C)),
        )
        .map(|(q, t)| match t {
            Sum3::A(b) => Term::Boolean(q, b),
            Sum3::B(t) => Term::Pair(q, Box::new(t.1), Box::new(t.3)),
            Sum3::C(t) => Term::Abstraction(q, t.1, t.3, Box::new(t.5)),
        });

    let term = paren
        .or(attempt(variable))
        .or(boolean_or_pair_or_abstraction)
        .or(if_)
        .or(split);

    (term, term_tail_p()).map(|t| match t.1 {
        Some(t2) => Term::Application(Box::new(t.0), Box::new(t2)),
        None => t.0,
    })
}

parser! {
    fn term_p[I]()(I) -> Term
    where [I: Stream<Item = char>]
    {
        term_p_()
    }
}

fn term_tail_p_<I>() -> impl Parser<Input = I, Output = Option<Term>>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let application_tail = spaces1()
        .with((term_p(), term_tail_p()))
        .map(|t| match t.1 {
            Some(t2) => Term::Application(Box::new(t.0), Box::new(t2)),
            None => t.0,
        });
    attempt(application_tail.map(Some)).or(value(None))
}

parser! {
    fn term_tail_p[I]()(I) -> Option<Term>
    where [I: Stream<Item = char>]
    {
        term_tail_p_()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PreType {
    Bool,
    Pair(Box<Type>, Box<Type>),
    Function(Box<Type>, Box<Type>),
}

fn pre_type_p_<I>() -> impl Parser<Input = I, Output = PreType>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let bool_ = || string("Bool").with(value(PreType::Bool));

    let pair = || {
        (type_p().skip(spaces()), char('*').skip(spaces()), type_p())
            .map(|t| PreType::Pair(Box::new(t.0), Box::new(t.2)))
    };

    let function = || {
        (
            type_p().skip(spaces()),
            string("->").skip(spaces()),
            type_p(),
        )
            .map(|t| PreType::Function(Box::new(t.0), Box::new(t.2)))
    };

    let raw = || bool_().or(attempt(pair())).or(function());

    let atom = char('(')
        .skip(spaces())
        .with(raw())
        .skip(spaces())
        .skip(char(')'));

    atom.or(raw())
}

parser! {
    fn pre_type_p[I]()(I) -> PreType
    where [I: Stream<Item = char>]
    {
        pre_type_p_()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type(Qualifier, PreType);

fn type_p_<I>() -> impl Parser<Input = I, Output = Type>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (qualifiers_p(), spaces1(), pre_type_p()).map(|t| Type(t.0, t.2))
}

parser! {
    fn type_p[I]()(I) -> Type
    where [I: Stream<Item = char>]
    {
        type_p_()
    }
}

pub fn parse(input: &str) -> Result<Term, Errors<char, &str, PointerOffset>> {
    let mut t = term_p();
    t.easy_parse(input).map(|x| x.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_qualifiers_p() {
        let mut q = qualifiers_p();
        assert_eq!(q.parse("lin"), Ok((Qualifier::Lin, "")));
        assert_eq!(q.parse("un"), Ok((Qualifier::Un, "")));
        assert!(q.parse("abc").is_err());
    }

    #[test]
    fn test_variable_p() {
        let mut v = variable_p();
        assert_eq!(v.parse("li"), Ok((Variable("li".to_owned()), "")));
        assert!(v.parse("lin").is_err());
        assert!(v.parse(" ").is_err());
    }

    #[test]
    fn test_type_p() {
        let mut v = type_p();
        assert_eq!(
            v.parse("un Bool"),
            Ok((Type(Qualifier::Un, PreType::Bool), "",)),
        );
        assert_eq!(
            v.parse("un (un Bool * un Bool)"),
            Ok((
                Type(
                    Qualifier::Un,
                    PreType::Pair(
                        Box::new(Type(Qualifier::Un, PreType::Bool)),
                        Box::new(Type(Qualifier::Un, PreType::Bool)),
                    ),
                ),
                "",
            )),
        );
        assert_eq!(
            v.parse("lin ( lin Bool -> un Bool )"),
            Ok((
                Type(
                    Qualifier::Lin,
                    PreType::Function(
                        Box::new(Type(Qualifier::Lin, PreType::Bool)),
                        Box::new(Type(Qualifier::Un, PreType::Bool)),
                    ),
                ),
                "",
            )),
        );
        assert_eq!(
            v.parse("un (un Bool -> lin (un Bool * lin Bool))"),
            Ok((
                Type(
                    Qualifier::Un,
                    PreType::Function(
                        Box::new(Type(Qualifier::Un, PreType::Bool)),
                        Box::new(Type(
                            Qualifier::Lin,
                            PreType::Pair(
                                Box::new(Type(Qualifier::Un, PreType::Bool)),
                                Box::new(Type(Qualifier::Lin, PreType::Bool)),
                            ),
                        )),
                    ),
                ),
                "",
            )),
        );
        assert_eq!(
            v.parse("lin (un (lin Bool * un Bool) -> un Bool)"),
            Ok((
                Type(
                    Qualifier::Lin,
                    PreType::Function(
                        Box::new(Type(
                            Qualifier::Un,
                            PreType::Pair(
                                Box::new(Type(Qualifier::Lin, PreType::Bool)),
                                Box::new(Type(Qualifier::Un, PreType::Bool))
                            )
                        )),
                        Box::new(Type(Qualifier::Un, PreType::Bool))
                    )
                ),
                "",
            )),
        );
        assert!(v.parse("Fool").is_err());
    }

    #[test]
    fn test_term_p() {
        let mut t = term_p();
        assert_eq!(
            t.parse("x"),
            Ok((Term::Variable(Variable("x".to_owned())), ""))
        );
        assert_eq!(
            t.parse(r"x y"),
            Ok((
                Term::Application(
                    Box::new(Term::Variable(Variable("x".to_owned()))),
                    Box::new(Term::Variable(Variable("y".to_owned()))),
                ),
                "",
            ))
        );
        // assert_eq!(t.parse(r"un \t: un Bool.t"), Ok((Term::Variable(Variable("4".to_owned())), "")));
        // assert_eq!(t.parse("un <un true, un true>"), Ok((Term::Variable(Variable("5".to_owned())), "")));
        // assert_eq!(t.parse("if (un true) then (un true) else (un true)"), Ok((Term::Variable(Variable("6".to_owned())), "")));
        // assert_eq!(t.parse("split (un true) as x, y in (un true)"), Ok((Term::Variable(Variable("7".to_owned())), "")));
        assert_eq!(
            t.parse(r"lin \t: un (un Bool * un Bool).split t as x, y in if x then lin <x, y> else lin <y, x>"),
            Ok((
                Term::Abstraction(
                    Qualifier::Lin,
                    Variable("t".to_owned()),
                    Type(
                        Qualifier::Un,
                        PreType::Pair(
                            Box::new(Type(Qualifier::Un, PreType::Bool)),
                            Box::new(Type(Qualifier::Un, PreType::Bool)),
                        )
                    ),
                    Box::new(Term::Split(
                        Box::new(Term::Variable(Variable("t".to_owned()))),
                        Variable("x".to_owned()),
                        Variable("y".to_owned()),
                        Box::new(Term::If(
                            Box::new(Term::Variable(Variable("x".to_owned()))),
                            Box::new(Term::Pair(
                                Qualifier::Lin,
                                Box::new(Term::Variable(Variable("x".to_owned()))),
                                Box::new(Term::Variable(Variable("y".to_owned()))),
                            )),
                            Box::new(Term::Pair(
                                Qualifier::Lin,
                                Box::new(Term::Variable(Variable("y".to_owned()))),
                                Box::new(Term::Variable(Variable("x".to_owned()))),
                            )),
                        )),
                    )),
                ),
                "",
            ))
        );
        assert!(t.parse("if").is_err());
    }
}
