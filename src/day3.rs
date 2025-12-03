use std::cmp::Reverse;

use anyhow::Context;
use itertools::Itertools;

use crate::library::Definitely;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct Battery {
    voltage: u64,
}

#[derive(Debug)]
struct Bank {
    batteries: Vec<Battery>,
}

#[derive(Debug)]
pub struct Input {
    banks: Vec<Bank>,
}

impl TryFrom<&str> for Input {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value
            .trim()
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| {
                        c.to_digit(10)
                            .context("character isn't a digit")
                            .map(u64::from)
                            .map(|voltage| Battery { voltage })
                    })
                    .try_collect()
                    .map(|batteries| Bank { batteries })
            })
            .try_collect()
            .map(|banks| Input { banks })
    }
}

fn largest_joltage(batteries: &[Battery], depth: usize, base: u64) -> Option<u64> {
    let Some(depth) = depth.checked_sub(1) else {
        return Some(base);
    };

    let prefix = batteries.get(..batteries.len() - depth)?;
    let (index, battery) = prefix
        .iter()
        .enumerate()
        .max_by_key(|&(index, battery)| (battery.voltage, Reverse(index)))?;

    largest_joltage(
        batteries.get(index + 1..)?,
        depth,
        base * 10 + battery.voltage,
    )
}

fn solve(input: &Input, depth: usize) -> Definitely<u64> {
    Ok(input
        .banks
        .iter()
        .filter_map(|bank| largest_joltage(&bank.batteries, depth, 0))
        .sum())
}

pub fn part1(input: Input) -> Definitely<u64> {
    solve(&input, 2)
}

pub fn part2(input: Input) -> Definitely<u64> {
    solve(&input, 12)
}
