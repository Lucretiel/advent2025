use std::{collections::HashMap, convert::Infallible, fmt::Debug, hash::Hash};

use anyhow::Context;
use itertools::Itertools;
use nom::{
    Parser as _,
    character::complete::{alpha1, char, line_ending, space0},
    combinator::{eof, success},
};
use nom_supreme::{
    ParserExt, error::ErrorTree, final_parser::final_parser, multi::collect_separated_terminated,
};

use crate::library::{
    ITResult,
    dynamic::{self, Task},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct DeviceId<'a>(&'a str);

fn parse_device_id(input: &str) -> ITResult<&str, DeviceId<'_>> {
    alpha1.map(DeviceId).parse(input)
}

type Connections<'a> = HashMap<DeviceId<'a>, Vec<DeviceId<'a>>>;

#[derive(Debug)]
pub struct Input<'a> {
    connections: Connections<'a>,
}

fn parse_input(input: &str) -> ITResult<&str, Input<'_>> {
    collect_separated_terminated(
        parse_device_id
            .terminated(char(':'))
            .terminated(space0)
            .and(collect_separated_terminated(
                parse_device_id.terminated(space0),
                success(()),
                line_ending.or(eof),
            )),
        success(()),
        eof,
    )
    .map(|connections| Input { connections })
    .parse(input)
}

impl<'a> TryFrom<&'a str> for Input<'a> {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

impl<'a> DeviceId<'a> {
    const YOU: Self = Self("you");
    const SVR: Self = Self("svr");
    const END: Self = Self("out");
    const FFT: Self = Self("fft");
    const DAC: Self = Self("dac");
}

struct GraphExplorer<'a> {
    connections: Connections<'a>,
}

impl<'a> dynamic::StatelessTask<DeviceId<'a>, u64, Infallible> for GraphExplorer<'a> {
    fn solve<'sub>(
        &self,
        goal: &DeviceId<'a>,
        subtasker: &'sub impl dynamic::Subtask<DeviceId<'a>, u64>,
    ) -> Result<u64, dynamic::TaskInterrupt<'sub, DeviceId<'a>, Infallible>> {
        Ok(match goal {
            &DeviceId::END => 1,
            goal => self
                .connections
                .get(goal)
                .map(|steps| steps.as_slice())
                .unwrap_or(&[])
                .iter()
                .map(|&step| subtasker.solve(step))
                .process_results(|counts| counts.sum())?,
        })
    }
}

fn solve<'a, T>(input: Input<'a>, goal: T) -> anyhow::Result<u64>
where
    GraphExplorer<'a>: Task<T, u64, Infallible>,
    T: Debug + Eq + Hash,
{
    let connections = input.connections;

    dynamic::execute(goal, &GraphExplorer { connections }, HashMap::new())
        .map_err(|err| match err {
            dynamic::DynamicError::CircularDependency(key) => {
                anyhow::anyhow!("circular reference detected for {key:?}")
            }
        })
        .context("failed to solve dynamically")
}

pub fn part1<'a>(input: Input<'a>) -> anyhow::Result<u64> {
    solve(input, DeviceId::YOU)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Part2Goal<'a> {
    device: DeviceId<'a>,
    fft: bool,
    dac: bool,
}

impl<'a> dynamic::StatelessTask<Part2Goal<'a>, u64, Infallible> for GraphExplorer<'a> {
    fn solve<'sub>(
        &self,
        goal: &Part2Goal<'a>,
        subtasker: &'sub impl dynamic::Subtask<Part2Goal<'a>, u64>,
    ) -> Result<u64, dynamic::TaskInterrupt<'sub, Part2Goal<'a>, Infallible>> {
        Ok(match *goal {
            // Reached END and found both nodes
            Part2Goal {
                fft: true,
                dac: true,
                device: DeviceId::END,
            } => 1,

            // Reached END but didn't hit the right nodes
            Part2Goal {
                device: DeviceId::END,
                ..
            } => 0,

            Part2Goal {
                ref device,
                fft,
                dac,
            } => self
                .connections
                .get(device)
                .map(|steps| steps.as_slice())
                .unwrap_or(&[])
                .iter()
                .map(|&step| {
                    subtasker.solve(Part2Goal {
                        device: step,
                        fft: fft || *device == DeviceId::FFT,
                        dac: dac || *device == DeviceId::DAC,
                    })
                })
                .process_results(|counts| counts.sum())?,
        })
    }
}

pub fn part2(input: Input) -> anyhow::Result<u64> {
    solve(
        input,
        Part2Goal {
            device: DeviceId::SVR,
            fft: false,
            dac: false,
        },
    )
}
