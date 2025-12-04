use std::{collections::HashSet, convert::Infallible};

use gridly::prelude::*;
use itertools::Itertools;

use crate::library::{Definitely, IterExt};

#[derive(Debug)]
pub struct Input {
    rolls: HashSet<Location>,
}

impl TryFrom<&str> for Input {
    type Error = Infallible;

    fn try_from(value: &str) -> Definitely<Self> {
        Ok(Input {
            rolls: value
                .trim()
                .lines()
                .map(|line| line.trim().chars().with_columns(Column(0)))
                .with_rows(Row(0))
                .flat_map(|(row, chars)| {
                    chars.map(move |(column, cell)| (row.combine(column), cell))
                })
                .filter(|&(_, cell)| cell == '@')
                .map(|(loc, _cell)| loc)
                .collect(),
        })
    }
}

fn get_accessible(input: &Input) -> impl Iterator<Item = &Location> {
    input.rolls.iter().filter(|&&location| {
        TOUCHING_ADJACENCIES
            .iter()
            .map(|&delta| location + delta)
            .filter(|neighbor| input.rolls.contains(neighbor))
            .count()
            < 4
    })
}

pub fn part1(input: Input) -> Definitely<usize> {
    Ok(get_accessible(&input).count())
}

pub fn part2(mut input: Input) -> Definitely<usize> {
    let initial_rolls = input.rolls.len();

    Ok(loop {
        let accessible = get_accessible(&input).copied().collect_vec();

        if accessible.is_empty() {
            break initial_rolls - input.rolls.len();
        }

        accessible.iter().for_each(|loc| {
            input.rolls.remove(loc);
        });
    })
}
