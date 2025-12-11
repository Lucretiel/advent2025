use std::{cmp, collections::HashSet};

use gridly::prelude::*;

use crate::{
    express,
    library::{Definitely, char_lines_with_locations, counter::HashCounter},
};

#[derive(Debug, Default)]
pub struct Input {
    start: Location,
    splitters: HashSet<Location>,
    boundary: Row,
}

impl From<&str> for Input {
    fn from(value: &str) -> Self {
        char_lines_with_locations(Location::zero(), value).fold(
            Input::default(),
            |Input {
                 start,
                 splitters,
                 boundary,
             },
             (location, cell)| {
                let boundary = cmp::max(boundary, location.row + Rows(3));

                match cell {
                    'S' => Input {
                        start: location,
                        splitters,
                        boundary,
                    },
                    '^' => Input {
                        start,
                        splitters: express!(splitters.insert(location)),
                        boundary,
                    },
                    _ => Input {
                        start,
                        splitters,
                        boundary,
                    },
                }
            },
        )
    }
}

pub fn part1(input: Input) -> Definitely<usize> {
    let mut lasers = HashSet::from([input.start.column]);
    let mut splits = 0;

    for row in input.start.row.range_to(input.boundary) {
        let mut updated_lasers = lasers.clone();

        for &laser in &lasers {
            if input.splitters.contains(&row.combine(laser)) {
                updated_lasers.remove(&laser);
                updated_lasers.insert(laser + Columns(1));
                updated_lasers.insert(laser - Columns(1));
                splits += 1;
            }
        }

        lasers = updated_lasers;
    }

    Ok(splits)
}

pub fn part2(input: Input) -> Definitely<usize> {
    let mut lasers = HashCounter::from_iter([input.start.column]);

    for row in input.start.row.range_to(input.boundary) {
        let mut updated_lasers = HashCounter::new();

        for (&laser, count) in lasers.iter() {
            if input.splitters.contains(&row.combine(laser)) {
                updated_lasers.extend(
                    [laser + Columns(1), laser - Columns(1)]
                        .iter()
                        .copied()
                        .map(|split| (split, count)),
                );
            } else {
                updated_lasers.add(laser, count);
            }
        }

        lasers = updated_lasers;
    }

    Ok(lasers
        .iter()
        .map(|(_, count)| count)
        .map(|count| count.get())
        .sum())
}
