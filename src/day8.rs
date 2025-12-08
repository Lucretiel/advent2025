use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet, hash_map::Entry},
    num::NonZero,
};

use anyhow::Context;
use itertools::Itertools;
use nom::{
    Parser,
    character::complete::{char, digit1, multispace0, multispace1, space0},
    combinator::eof,
};
use nom_supreme::{
    ParserExt, error::ErrorTree, final_parser::final_parser, multi::collect_separated_terminated,
};
use rayon::prelude::*;

use crate::{library::ITResult, parser};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Coordinates {
    x: i64,
    y: i64,
    z: i64,
}

fn parse_coordinate(input: &str) -> ITResult<&str, i64> {
    digit1
        .opt_preceded_by(char('-'))
        .recognize()
        .parse_from_str_cut()
        .parse(input)
}

fn parse_coordinates(input: &str) -> ITResult<&str, Coordinates> {
    parser! {
        parse_coordinate => x,
        char(',').delimited_by(space0),
        parse_coordinate => y,
        char(',').delimited_by(space0),
        parse_coordinate => z;
        Coordinates { x, y, z }
    }
    .parse(input)
}

#[derive(Debug)]
pub struct Input {
    boxes: Vec<Coordinates>,
}

fn parse_input(input: &str) -> ITResult<&str, Input> {
    collect_separated_terminated(parse_coordinates, multispace1, multispace0.terminated(eof))
        .map(|boxes| Input { boxes })
        .parse(input)
}

impl TryFrom<&str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

fn square_distance(left: &Coordinates, right: &Coordinates) -> i64 {
    (left.x - right.x).strict_pow(2)
        + (left.y - right.y).strict_pow(2)
        + (left.z - right.z).strict_pow(2)
}

// Compute the distance between each coordinate and each OTHER coordinate
fn sorted_distance_sets(boxes: &[Coordinates]) -> HashMap<&Coordinates, Vec<(i64, &Coordinates)>> {
    boxes
        .par_iter()
        .map(|start| {
            let mut distances = boxes
                .iter()
                .filter(|&end| end != start)
                .map(|end| (square_distance(start, end), end))
                .collect_vec();

            distances.sort_unstable_by_key(|&(distance, _end)| distance);
            (start, distances)
        })
        .collect()
}

fn overall_distance_set<'a>(
    distances: &HashMap<&'a Coordinates, Vec<(i64, &'a Coordinates)>>,
) -> Vec<(i64, (&'a Coordinates, &'a Coordinates))> {
    let mut overall_distances = distances
        .iter()
        .flat_map(|(&start, distances)| {
            distances
                .iter()
                .filter(move |&&(_, end)| start < end)
                .map(move |&(distance, end)| (distance, (start, end)))
        })
        .collect_vec();

    overall_distances.sort_unstable_by_key(|&(distance, _)| distance);
    overall_distances
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct NetworkID(NonZero<u32>);

#[derive(Clone)]
struct NetworkMap<'a> {
    networks: HashMap<NetworkID, HashSet<&'a Coordinates>>,
    memberships: HashMap<&'a Coordinates, NetworkID>,
}

impl<'a> NetworkMap<'a> {
    pub fn new_disconnected(boxes: &'a [Coordinates]) -> Self {
        let ids = (1..).map(|id| NetworkID(NonZero::new(id).unwrap()));
        let associations = boxes.iter().zip(ids);

        let memberships = associations.clone().collect();
        let networks = associations
            .map(|(item, id)| (id, HashSet::from([item])))
            .collect();

        Self {
            networks,
            memberships,
        }
    }

    pub fn connect(
        &mut self,
        origin: &Coordinates,
        subordinate: &Coordinates,
    ) -> anyhow::Result<()> {
        let target_id = self
            .memberships
            .get(origin)
            .copied()
            .with_context(|| format!("origin {origin:?} doesn't exist"))?;

        let defunct_id = self
            .memberships
            .get(subordinate)
            .copied()
            .with_context(|| format!("subordinate {origin:?} doesn't exist"))?;

        if defunct_id == target_id {
            return Ok(());
        }

        let defunct_items = self.networks.remove(&defunct_id).unwrap_or_default();

        defunct_items.iter().for_each(|defunct| {
            *self
                .memberships
                .get_mut(defunct)
                .expect("bijection between memberships and networks") = target_id;
        });

        match self.networks.entry(target_id) {
            Entry::Occupied(occupied) => {
                occupied.into_mut().extend(defunct_items);
            }
            Entry::Vacant(vacant) => {
                vacant.insert(defunct_items);
            }
        }

        Ok(())
    }
}

pub fn part1(input: Input) -> anyhow::Result<usize> {
    let distances = sorted_distance_sets(&input.boxes);
    let all_distances = overall_distance_set(&distances);
    let mut map = NetworkMap::new_disconnected(&input.boxes);

    all_distances
        .iter()
        .take(1000)
        .try_for_each(|&(_, (start, end))| map.connect(start, end))?;

    let mut all_networks = map.networks.values().collect_vec();
    all_networks.sort_unstable_by_key(|&network| Reverse(network.len()));

    Ok(all_networks
        .iter()
        .take(3)
        .map(|network| network.len())
        .product())
}

pub fn part2(input: Input) -> anyhow::Result<i64> {
    let distances = sorted_distance_sets(&input.boxes);
    let all_distances = overall_distance_set(&distances);
    let mut map = NetworkMap::new_disconnected(&input.boxes);

    for (_, (start, end)) in &all_distances {
        map.connect(start, end)?;

        if map.networks.len() == 1 {
            return Ok(start.x * end.x);
        }
    }

    anyhow::bail!("couldn't form a fully connected network, this shouldn't be possible")
}
