#![allow(non_snake_case)]

#[cfg(test)]
mod AoC2016;
#[cfg(test)]
mod AoC2022;

use combine::error::StreamError;
use combine::{any, count, many1};
use combine::{attempt, parser::char::digit, stream::StreamErrorFor, Parser, Stream};
use extend::ext;

pub fn p_int<Input: Stream<Token = char>>() -> impl Parser<Input, Output = usize> {
    many1(digit()).map(|digits: String| digits.parse().unwrap())
}

pub fn p_any_string<Input: Stream<Token = char>>(i: usize) -> impl Parser<Input, Output = String> {
    count(i, any())
}

#[ext(name = ParserCombinatorExt)]
pub impl<This, Input, I, O> This
where
    This: Parser<Input, Output = Option<O>>,
    Input: Stream<Token = I>,
{
    /// Parser combinator to raise parser error when parsed value is [Option::None].
    fn map_opt(self) -> impl Parser<Input, Output = O> {
        // Attempt to parse, but backtrack (not consume input) in case of an error
        attempt(
            self.and_then(move |opt: Option<O>| {
                opt.ok_or_else(|| StreamErrorFor::<Input>::expected("map_opt success"))
            })
            // Swallow above error (it's expected and input is not consumed)
            .silent(),
        )
    }
}
