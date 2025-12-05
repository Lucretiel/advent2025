use std::cmp;

use nom::{
    Parser,
    character::complete::{char, digit1, multispace0},
    combinator::eof,
};
use nom_supreme::{
    ParserExt as _, error::ErrorTree, final_parser::final_parser,
    multi::collect_separated_terminated, tag::complete::tag,
};

use crate::{
    library::{Definitely, ITResult},
    parser,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Range {
    min: i64,
    max: i64,
}

impl Range {
    pub fn contains(&self, value: i64) -> bool {
        self.min <= value && value < self.max
    }

    pub fn width(&self) -> i64 {
        self.max - self.min
    }
}

fn parse_range(input: &str) -> ITResult<&str, Range> {
    parser! {
        digit1.parse_from_str_cut() => min,
        char('-'),
        digit1.parse_from_str_cut().map(|max: i64| max + 1) => max;
        Range { min, max }
    }
    .parse(input)
}

#[derive(Debug)]
pub struct Input {
    fresh_ranges: Vec<Range>,
    available_values: Vec<i64>,
}

fn parse_input(input: &str) -> ITResult<&str, Input> {
    parser! {
        collect_separated_terminated(
            parse_range,
            char('\n'),
            tag("\n\n")
        ) => fresh_ranges,

        collect_separated_terminated(
            digit1.parse_from_str_cut::<i64>(),
            char('\n'),
            multispace0.terminated(eof)
        ) => available_values;

        Input { fresh_ranges, available_values}
    }
    .parse(input)
}

impl TryFrom<&str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(input: Input) -> Definitely<usize> {
    Ok(input
        .available_values
        .iter()
        .filter(|&&value| input.fresh_ranges.iter().any(|range| range.contains(value)))
        .count())
}

pub fn part2(mut input: Input) -> Definitely<i64> {
    input.fresh_ranges.sort_unstable();

    // We know all inputs are positive, so we can start at 0
    let (total, _) = input
        .fresh_ranges
        .iter()
        .fold((0, 0), |(total, max), range| {
            let min = cmp::max(max, range.min);
            let max = cmp::max(min, range.max);
            let range = Range { min, max };
            (total + range.width(), range.max)
        });

    Ok(total)
}
