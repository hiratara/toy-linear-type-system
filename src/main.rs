#![recursion_limit = "67"]

#[macro_use]
extern crate combine;

mod parser;

use std::env;

fn main() {
    let args = &env::args().collect::<Vec<_>>()[1];
    println!("{:?}", parser::parse(args));
}
