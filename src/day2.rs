use std::{collections::HashSet, iter};

use nom::{
    Parser as _,
    character::complete::{char, digit1, multispace0},
    combinator::eof,
};
use nom_supreme::{
    ParserExt as _, error::ErrorTree, final_parser::final_parser,
    multi::collect_separated_terminated,
};

use crate::{
    library::{Definitely, ITResult},
    parser,
};

#[derive(Debug)]
struct Range {
    min: u64,
    max: u64,
}

fn parse_range(input: &str) -> ITResult<&str, Range> {
    parser! {
        digit1.parse_from_str_cut()=> min,
        char('-'),
        digit1.parse_from_str_cut() => max;
        Range { min, max }
    }
    .parse(input)
}

#[derive(Debug)]
pub struct Input {
    ranges: Vec<Range>,
}

fn parse_input(input: &str) -> ITResult<&str, Input> {
    collect_separated_terminated(parse_range, char(','), multispace0.terminated(eof))
        .map(|ranges| Input { ranges })
        .parse(input)
}

impl TryFrom<&str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

fn count_digits(value: u64) -> u32 {
    ((value as f64).log10().floor() as u32) + 1
}

fn solve<I>(input: &Input, bad_ids: impl Fn(u64) -> I) -> Definitely<u64>
where
    I: IntoIterator<Item = u64>,
{
    let max = input.ranges.iter().map(|r| r.max).max().unwrap_or(0);

    let bad_ids_found: HashSet<u64> = (1..)
        .map(|value| {
            let mut candidates = bad_ids(value)
                .into_iter()
                .take_while(|&value| value <= max)
                .peekable();
            (candidates.peek().is_some(), candidates)
        })
        .take_while(|&(extant, _)| extant)
        .flat_map(|(_, candidates)| candidates)
        .filter(|&candidate| {
            input
                .ranges
                .iter()
                .any(|r| candidate >= r.min && candidate <= r.max)
        })
        .collect();

    Ok(bad_ids_found.iter().copied().sum())
}

pub fn part1(input: Input) -> Definitely<u64> {
    solve(&input, |value| {
        let digits = count_digits(value);
        [value * 10u64.pow(digits) + value]
    })
}

pub fn part2(input: Input) -> Definitely<u64> {
    solve(&input, |value| {
        let digits = count_digits(value);
        let factor = 10u64.pow(digits);

        iter::successors(Some(value), move |&state| {
            state.checked_mul(factor)?.checked_add(value)
        })
        .skip(1)
    })
}
