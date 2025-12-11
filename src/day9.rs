use std::{
    cmp,
    collections::{BTreeMap, BTreeSet, HashSet, btree_map::Entry},
    convert::Infallible,
    ops::{Bound, RangeInclusive},
};

use anyhow::Context;
use gridly::prelude::*;
use nom::{
    Parser as _,
    character::complete::{char, digit1, multispace0, multispace1},
    combinator::eof,
};
use nom_supreme::{
    ParserExt as _, error::ErrorTree, final_parser::final_parser,
    multi::collect_separated_terminated,
};
use rayon::prelude::*;

use crate::library::{ITResult, IterExt};

#[derive(Debug)]
pub struct Input {
    tiles: Vec<Location>,
}

fn parse_input(input: &str) -> ITResult<&str, Input> {
    collect_separated_terminated(
        (digit1.parse_from_str_cut().map(Column))
            .terminated(char(','))
            .and(digit1.parse_from_str_cut().map(Row))
            .map(|(x, y)| x.combine(y)),
        multispace1,
        multispace0.terminated(eof),
    )
    .map(|tiles| Input { tiles })
    .parse(input)
}

impl TryFrom<&str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

fn area(corner1: &Location, corner2: &Location) -> isize {
    let diagonal = *corner1 - *corner2;
    let diagonal = Vector::new(
        diagonal.rows.manhattan_length(),
        diagonal.columns.manhattan_length(),
    );
    let diagonal = diagonal + (Rows(1), Columns(1));
    diagonal.rows.value() * diagonal.columns.value()
}

fn all_possible_rectangles(
    tiles: &[Location],
) -> impl ParallelIterator<Item = (&Location, &Location)> {
    tiles
        .par_iter()
        .flat_map_iter(|corner1| tiles.iter().map(move |corner2| (corner1, corner2)))
        .filter(|(corner1, corner2)| corner1 != corner2)
}

pub fn part1(input: Input) -> anyhow::Result<isize> {
    all_possible_rectangles(&input.tiles)
        .map(|(corner1, corner2)| area(corner1, corner2))
        .max()
        .context("no tiles in the input")
}

// Use a funky path-tracing algorithm. Assume clockwise orientation.
fn find_best_rectangle<'a>(
    root: &'a Location,
    subsequent: impl Iterator<Item = &'a Location>,
) -> anyhow::Result<&'a Location> {
    let mut upper_bound = None;
    let mut lower_bound = None;
    let mut left_bound = None;
    let mut right_bound = None;

    let mut previous_root = root;

    for location in subsequent {
        let direction = (*previous_root - *location)
            .direction()
            .context("wasn't a straight line")?;

        previous_root = location;

        match direction {
            Up => {
                left_bound = Some(
                    left_bound
                        .map(|left| cmp::max(left, location.column))
                        .unwrap_or(location.column),
                );
            }
            Right => {
                upper_bound = Some(
                    upper_bound
                        .map(|top| cmp::max(top, location.row))
                        .unwrap_or(location.row),
                );
            }
            Down => {
                right_bound = Some(
                    right_bound
                        .map(|right| cmp::min(right, location.column))
                        .unwrap_or(location.column),
                )
            }
            Left => {
                lower_bound = Some(
                    lower_bound
                        .map(|bottom| cmp::min(bottom, location.row))
                        .unwrap_or(location.row),
                )
            }
        }
    }

    todo!()
}

pub fn part2(_input: Input) -> anyhow::Result<Infallible> {
    anyhow::bail!("not implemented yet")
}
