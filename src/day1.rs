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

use crate::library::{Definitely, ITResult};

#[derive(Debug, Clone, Copy)]
enum Direction {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy)]
struct Rotation {
    distance: i32,
    direction: Direction,
}

impl Rotation {
    pub fn apply(&self, state: i32) -> i32 {
        match self.direction {
            Direction::Left => state - self.distance,
            Direction::Right => state + self.distance,
        }
        .rem_euclid(100)
    }

    pub fn apply_count(&self, state: i32) -> (i32, u32) {
        let new_state = match self.direction {
            Direction::Left => state - self.distance,
            Direction::Right => state + self.distance,
        };

        let c1 = state.div_euclid(100);
        let c2 = new_state.div_euclid(100);

        let clicks = (c1 - c2).unsigned_abs();

        (new_state, clicks)
    }
}

fn parse_rotation(input: &str) -> ITResult<&str, Rotation> {
    alt((
        char('L').value(Direction::Left),
        char('R').value(Direction::Right),
    ))
    .and(digit1.parse_from_str_cut())
    .map(|(direction, distance)| Rotation {
        direction,
        distance,
    })
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

pub fn part2(input: Input) -> Definitely<u32> {
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
