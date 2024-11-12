use crate::{p_int, ParserCombinatorExt};
use combine::*;
use itertools::Itertools;
use parser::char::newline;

pub fn row<Input: Stream<Token = char>>() -> impl Parser<Input, Output = usize> {
    (p_int(), newline()).map(|(x, _)| x)
}

pub fn elf<Input: Stream<Token = char>>() -> impl Parser<Input, Output = usize> {
    many(row()).map(|xs: Vec<_>| xs.into_iter().sum())
}

pub fn elfs<Input: Stream<Token = char>>() -> impl Parser<Input, Output = Vec<usize>> {
    sep_by1(elf(), newline())
}

pub fn elfs_sorted_top3<Input: Stream<Token = char>>() -> impl Parser<Input, Output = Vec<usize>> {
    elfs().map(|xs| xs.into_iter().sorted().rev().take(3).collect_vec())
}

pub fn elf_max<Input: Stream<Token = char>>() -> impl Parser<Input, Output = usize> {
    elfs_sorted_top3().map(|xs| xs.into_iter().next()).map_opt()
}

pub fn elf_top3_sum<Input: Stream<Token = char>>() -> impl Parser<Input, Output = usize> {
    elfs_sorted_top3().map(|xs| xs.into_iter().sum())
}

#[cfg(test)]
mod tests {
    use super::*;

    const PUZZLE_INPUT: &'static str = include_str!("./puzzles/day-01.txt");

    #[test]
    fn part1() {
        let (res, _) = elf_max().parse(PUZZLE_INPUT).unwrap();

        assert_eq!(res, 70116);
    }

    #[test]
    fn part2() {
        let (res, _) = elf_top3_sum().parse(PUZZLE_INPUT).unwrap();

        assert_eq!(res, 206582);
    }
}
