use nom::{
    Parser as _,
    branch::alt,
    character::complete::{char, digit1, multispace0, multispace1},
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Left,
    Right,
}

use Direction::*;

#[derive(Debug, Clone, Copy)]
struct Rotation {
    distance: i32,
    direction: Direction,
}

impl Rotation {
    pub fn apply(&self, state: i32) -> i32 {
        match self.direction {
            Left => state - self.distance,
            Right => state + self.distance,
        }
        .rem_euclid(100)
    }

    pub fn apply_count(&self, state: i32) -> (i32, i32) {
        let clicks = self.distance.div_euclid(100);
        let distance = self.distance.rem_euclid(100);

        let direction = match self.direction {
            Left => -1,
            Right => 1,
        };

        (0..distance).fold((state, clicks), |(state, clicks), _| {
            let state = state + direction;
            (
                state,
                clicks + if state.rem_euclid(100) == 0 { 1 } else { 0 },
            )
        })
    }
}

fn parse_rotation(input: &str) -> ITResult<&str, Rotation> {
    parser! {
        alt((char('L').value(Left), char('R').value(Right))) => direction,
        digit1.parse_from_str_cut() => distance;
        Rotation { direction, distance }
    }
    .parse(input)
}

#[derive(Debug)]
pub struct Input {
    rotations: Vec<Rotation>,
}

fn parse_input(input: &str) -> ITResult<&str, Input> {
    collect_separated_terminated(parse_rotation, multispace1, multispace0.terminated(eof))
        .map(|rotations| Input { rotations })
        .parse(input)
}

impl TryFrom<&str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(input: Input) -> Definitely<usize> {
    let mut state = 50;

    Ok(input
        .rotations
        .iter()
        .map(|rotation| {
            state = rotation.apply(state);
            state
        })
        .filter(|&state| state == 0)
        .count())
}

pub fn part2(input: Input) -> Definitely<i32> {
    let mut state = 50;

    Ok(input
        .rotations
        .iter()
        .map(|rotation| {
            let (ns, clicks) = rotation.apply_count(state);
            state = ns;
            clicks
        })
        .sum())
}
