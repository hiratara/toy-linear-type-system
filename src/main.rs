#[macro_use]
extern crate combine;

use combine::char::{char, digit, spaces, string};
use combine::error::ParseError;
use combine::stream::state::State;
use combine::{attempt, many, many1, optional, satisfy, token, value, Parser, Stream};

#[derive(Clone, Debug, PartialEq, Eq)]
enum Qualifier {
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
enum Boolean {
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
struct Variable(String);

fn variable_p_<I>() -> impl Parser<Input = I, Output = Variable>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1::<String, _>(satisfy(|c: char| c.is_ascii_lowercase())).map(Variable)
}

parser! {
    fn variable_p[I]()(I) -> Variable
    where [I: Stream<Item = char>]
    {
        variable_p_()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Term {
    Variable(Variable),
    Boolean(Qualifier, Boolean),
    If(Box<Term>, Box<Term>, Box<Term>),
    Pair(Qualifier, Box<Term>, Box<Term>),
    Split(Box<Term>, Variable, Variable, Box<Term>),
    Abstraction(Qualifier, Variable, Type, Box<Term>),
    Application(Box<Term>, Box<Term>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum PreType {
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
struct Type(Qualifier, PreType);

fn type_p_<I>() -> impl Parser<Input = I, Output = Type>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (qualifiers_p(), spaces(), pre_type_p()).map(|t| Type(t.0, t.2))
}

parser! {
    fn type_p[I]()(I) -> Type
    where [I: Stream<Item = char>]
    {
        type_p_()
    }
}

fn main() {
    let sign = optional(token('+').or(token('-')));
    let number = many1::<Vec<char>, _>(digit()).map(|ds| {
        ds.into_iter()
            .map(|c| c.to_digit(10).unwrap() as i64)
            .fold(0, |acc, x| acc * 10 + x)
    });
    let mut signed_number = (sign, number).map(|(s, num)| match s {
        None | Some('+') => num,
        Some('-') => -num,
        _ => unreachable!(),
    });

    // println!("{:?}", signed_number.parse("-1023"));
    println!("{:?}", signed_number.parse(State::new("abc")));
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
        assert_eq!(v.parse("lin"), Ok((Variable("lin".to_owned()), "")));
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
}
