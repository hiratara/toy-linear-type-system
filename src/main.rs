use combine::char::{digit, string};
use combine::error::ParseError;
use combine::stream::state::State;
use combine::{many, many1, optional, token, value, satisfy, Parser, Stream};

#[derive(Clone, Debug, PartialEq, Eq)]
enum Qualifier {
    Lin,
    Un,
}

fn qualifiers_p<I>() -> impl Parser<Input = I, Output = Qualifier>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let lin = string("lin").with(value(Qualifier::Lin));
    let un = string("un").with(value(Qualifier::Un));
    lin.or(un)
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Boolean {
    True,
    False,
}

fn booleans_p<I>() -> impl Parser<Input = I, Output = Boolean>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let true_ = string("true").with(value(Boolean::True));
    let false_ = string("false").with(value(Boolean::False));
    true_.or(false_)
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Variable(String);

fn variable_p<I>() -> impl Parser<Input = I, Output = Variable>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many1::<String, _>(satisfy(|c: char| c.is_alphanumeric()))
        .map(Variable)
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

#[derive(Clone, Debug, PartialEq, Eq)]
struct Type(Qualifier, PreType);

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
}
