use crate::{p_any_string, p_int};
use combine::parser::char::{char, letter};
use combine::*;
use itertools::Itertools;
use std::{char, iter};

pub fn p_int_pair<Input: Stream<Token = char>>() -> impl Parser<Input, Output = (usize, usize)> {
    (p_int(), char('x'), p_int()).map(|(x, _, y)| (x, y))
}

pub fn p_code<Input: Stream<Token = char>>() -> impl Parser<Input, Output = (usize, usize)> {
    (char('('), p_int_pair(), char(')')).map(|(_, x, _)| x)
}

pub fn mk_p_sub_code<Input>(i: (usize, usize)) -> impl Parser<Input, Output = String>
where
    Input: Stream<Token = char>,
{
    let (x, y) = i;
    p_any_string(x as usize).map(move |s| iter::repeat(s).take(y as usize).join(""))
}

pub fn p_code_exp<Input: Stream<Token = char>>() -> impl Parser<Input, Output = String> {
    p_code().then(mk_p_sub_code)
}

pub fn p_word<Input: Stream<Token = char>>() -> impl Parser<Input, Output = String> {
    many(letter())
}

pub fn p_word1<Input: Stream<Token = char>>() -> impl Parser<Input, Output = String> {
    many1(letter())
}

pub fn p_exp<Input: Stream<Token = char>>() -> impl Parser<Input, Output = String> {
    p_word1().or((p_code_exp(), p_word()).map(|(x, y)| x + &y))
}

pub fn p_grammar<Input: Stream<Token = char>>() -> impl Parser<Input, Output = String> {
    many(p_exp())
}

// Part 2

pub fn p_word_len<Input: Stream<Token = char>>() -> impl Parser<Input, Output = usize> {
    p_word().map(|s| s.len())
}

pub fn p_word1_len<Input: Stream<Token = char>>() -> impl Parser<Input, Output = usize> {
    p_word1().map(|s| s.len())
}

pub fn mk_p_sub_code_len<Input>(i: (usize, usize)) -> impl Parser<Input, Output = usize>
where
    Input: Stream<Token = char>,
{
    let (x, y) = i;
    p_any_string(x as usize).map(move |s| {
        p_grammar_len()
            .parse(s.as_str())
            .map(|(len, rest)| if rest.is_empty() { len * y } else { y * s.len() })
            .unwrap()
    })
}

pub fn p_code_exp_len<Input: Stream<Token = char>>() -> impl Parser<Input, Output = usize> {
    p_code().then(mk_p_sub_code_len)
}

pub fn p_exp_len<Input: Stream<Token = char>>() -> impl Parser<Input, Output = usize> {
    p_word1_len().or((p_code_exp_len(), p_word_len()).map(|(x, y)| x + y))
}

pub fn p_grammar_len<Input: Stream<Token = char>>() -> impl Parser<Input, Output = usize> {
    many(p_exp_len()).map(|xs: Vec<_>| xs.into_iter().sum())
}

#[cfg(test)]
mod tests {
    use super::*;

    const PUZZLE_INPUT: &'static str = include_str!("./puzzles/input-2016-day09.txt");

    #[test]
    fn part1() {
        let (res, _) = p_grammar().parse(PUZZLE_INPUT).unwrap();

        assert_eq!(res.len(), 97714);
    }

    #[test]
    fn part2() {
        let (res, _) = p_grammar_len().parse(PUZZLE_INPUT).unwrap();

        assert_eq!(res, 10762972461);
    }
}
