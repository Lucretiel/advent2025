use std::collections::HashMap;

use anyhow::Context;
use gridly::prelude::*;
use itertools::Itertools;

use crate::library::IterExt;

#[derive(Debug, Clone, Copy)]
enum Op {
    Plus,
    Times,
}

#[derive(Debug)]
pub struct Input {
    rows: Vec<Vec<i64>>,
    ops: Vec<Op>,
}

impl TryFrom<&str> for Input {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> anyhow::Result<Self> {
        let mut lines = value.trim().lines();

        let ops = lines
            .next_back()
            .context("no set of operations")?
            .trim()
            .split_whitespace()
            .map(|op| match op {
                "+" => Ok(Op::Plus),
                "*" => Ok(Op::Times),
                _ => anyhow::bail!("operator {op:?} wasn't recognized"),
            })
            .try_collect()?;

        let rows = lines
            .map(|line| {
                line.trim()
                    .split_whitespace()
                    .map(|value| value.parse::<i64>().context("non numeric value"))
                    .try_collect()
            })
            .try_collect()?;

        Ok(Input { rows, ops })
    }
}

pub fn part1(input: Input) -> anyhow::Result<i64> {
    input
        .ops
        .iter()
        .copied()
        .enumerate()
        .map(|(i, op)| {
            input
                .rows
                .iter()
                .map(|row| row.get(i).copied().context("length mismatch"))
                .process_results(|values| -> i64 {
                    match op {
                        Op::Plus => values.sum(),
                        Op::Times => values.product(),
                    }
                })
        })
        .process_results(|accumulations| accumulations.sum())
}

#[derive(Debug)]
pub struct Input2 {
    digits: HashMap<Location, i64>,
    ops: Vec<(Column, Op)>,
}

impl TryFrom<&str> for Input2 {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> anyhow::Result<Self> {
        let mut lines = value.trim().lines();

        let ops = lines
            .next_back()
            .context("no ops")?
            .chars()
            .with_columns(Column(0))
            .filter_map(|(column, c)| match c {
                '+' => Some((column, Op::Plus)),
                '*' => Some((column, Op::Times)),
                _ => None,
            })
            .collect();

        let digits = lines
            .map(|line| line.chars().with_columns(Column(0)))
            .with_rows(Row(0))
            .flat_map(|(row, line)| line.map(move |(column, c)| (row.combine(column), c)))
            .filter_map(|(location, c)| c.to_digit(10).map(|d| (location, d as i64)))
            .collect();

        Ok(Self { digits, ops })
    }
}

fn compute_values_from_column(
    column: Column,
    bottom: Row,
    map: &HashMap<Location, i64>,
) -> impl Iterator<Item = i64> {
    (0..)
        .map(Columns)
        .map(move |distance| column + distance)
        .take_while_some(move |column| {
            Row(0)
                .range_to(bottom)
                .map(|row| row.combine(column))
                .filter_map(|location| map.get(&location).copied())
                .reduce(|accum, digit| (accum * 10) + digit)
        })
}

pub fn part2(input: Input2) -> anyhow::Result<i64> {
    let bottom = input
        .digits
        .keys()
        .map(|key| key.row + Rows(1))
        .max()
        .context("no data")?;

    Ok(input
        .ops
        .iter()
        .map(|&(column, op)| -> i64 {
            let operands = compute_values_from_column(column, bottom, &input.digits);
            match op {
                Op::Plus => operands.sum(),
                Op::Times => operands.product(),
            }
        })
        .sum())
}
