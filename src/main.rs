#![feature(array_windows)]
#![feature(iter_array_chunks)]
#![feature(try_trait_v2)]
#![feature(control_flow_into_value)]

include!(concat!(env!("OUT_DIR"), "/generated.rs"));

mod library;

use std::{
    fs::File,
    io::{self, Read},
    num::ParseIntError,
    path::PathBuf,
    str::FromStr,
};

use anyhow::Context;
use debate::{
    FromArgs, Usage,
    help::{ParameterUsage, ParameterValueKind, Repetition, Requirement},
    parameter::ParsedValue,
};
use lazy_format::lazy_format;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum DayError {
    #[error("Failed to parse day")]
    Parse(#[from] ParseIntError),

    #[error("{0} is not an Advent Puzzle Day")]
    BadDay(u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Part {
    Part1,
    Part2,
}

#[derive(Debug, Clone, Error)]
pub enum PartError {
    #[error("Failed to parse part")]
    Parse(#[from] ParseIntError),

    #[error("{0} is not an Advent Puzzle Part; must be 1 or 2")]
    BadPart(u8),
}

impl FromStr for Part {
    type Err = PartError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value: u8 = s.parse()?;

        match value {
            1 => Ok(Part::Part1),
            2 => Ok(Part::Part2),
            value => Err(PartError::BadPart(value)),
        }
    }
}

impl ParsedValue for Part {}

impl ParameterUsage for Part {
    const VALUE: ParameterValueKind = ParameterValueKind::OneOf(&["1", "2"]);
    const REQUIREMENT: Requirement = Requirement::Mandatory;
    const REPETITION: Repetition = Repetition::Single;
}

/// Solve an Advent of Code 2022 problem for the given day and part. Unless
/// --string or --file are given, input is read from standard input. The
/// solution is always written to standard output.
#[derive(FromArgs, Usage)]
#[debate(help)]
struct Advent2025<'a> {
    /// The advent of code day to solve
    #[debate(short, long)]
    day: Day,

    /// Which part of the day to solve
    #[debate(short, long)]
    part: Part,

    /// If given, before the solution is printed, the parsed input for the
    /// problem will be printed to stderr
    #[debate(short = 'v', long)]
    show_input: bool,

    #[debate(flatten)]
    input: Option<Input<'a>>,
}

#[derive(FromArgs, Usage)]
#[debate(long)]
enum Input<'a> {
    /// If given, read input from stdin. This is the default.
    Stdin,

    /// If given, read input from this file
    #[debate(short)]
    File(PathBuf),

    /// If given, use this as the puzzle input directly
    #[debate(short)]
    String(&'a str),
}

#[debate::main]
fn main(args: Advent2025<'_>) -> anyhow::Result<()> {
    let mut buf: String = String::new();

    let input = match args.input.unwrap_or(Input::Stdin) {
        Input::String(s) => s,
        Input::Stdin => {
            io::stdin()
                .read_to_string(&mut buf)
                .context("failed to read puzzle input from stdin")?;

            buf.as_str()
        }
        Input::File(filename) => {
            File::open(&filename)
                .context(lazy_format!(
                    "failed to open file: {:?}",
                    filename.display()
                ))?
                .read_to_string(&mut buf)
                .context("failed to read puzzle input from file")?;

            buf.as_str()
        }
    };

    run_solution(args.day, args.part, &input, args.show_input)
}
