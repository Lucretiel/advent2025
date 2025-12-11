use std::{
    collections::{HashMap, HashSet, VecDeque},
    convert::Infallible,
    num::ParseIntError,
    str::FromStr,
};

use anyhow::Context;
use nom::{
    Parser,
    branch::alt,
    character::complete::{char, digit1, multispace0, multispace1, space0},
    combinator::{eof, success},
    error::{FromExternalError, ParseError},
};
use nom_supreme::{
    ParserExt as _,
    error::ErrorTree,
    final_parser::final_parser,
    multi::{collect_separated_terminated, parse_separated_terminated},
};

use crate::{
    express,
    library::{self, ITResult},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LightId(u8);

impl From<u8> for LightId {
    fn from(value: u8) -> Self {
        LightId(value)
    }
}

#[derive(Debug, Clone, Copy)]
enum Light {
    Off,
    On,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LightBank {
    lights: HashSet<LightId>,
}

impl LightBank {
    pub fn new() -> Self {
        Self {
            lights: HashSet::new(),
        }
    }

    pub fn with_toggled(&self, lights: impl IntoIterator<Item = LightId>) -> Self {
        let mut bank = self.lights.clone();

        for light in lights {
            if bank.contains(&light) {
                bank.remove(&light);
            } else {
                bank.insert(light);
            }
        }

        Self { lights: bank }
    }
}

#[derive(Debug)]
pub struct Spec {
    bank: HashSet<LightId>,
    buttons: Vec<HashSet<LightId>>,
    joltages: Vec<i32>,
}

fn parse_light_bank(input: &str) -> ITResult<&str, HashSet<LightId>> {
    parse_separated_terminated(
        alt((char('.').value(Light::Off), char('#').value(Light::On))),
        success(()),
        char(']'),
        || (0, HashSet::new()),
        |(index, set), light| match light {
            Light::Off => (index + 1, set),
            Light::On => (index + 1, express!(set.insert(LightId(index)))),
        },
    )
    .preceded_by(char('['))
    .map(|(_, set)| set)
    .parse(input)
}

fn braced_number_parser<'a, T, D, C, E>(
    open: char,
    close: char,
    item: impl Fn(D) -> T,
) -> impl Parser<&'a str, C, E>
where
    C: Extend<T> + Default,
    D: FromStr<Err = ParseIntError>,
    E: ParseError<&'a str>,
    E: FromExternalError<&'a str, ParseIntError>,
{
    collect_separated_terminated(
        digit1.parse_from_str_cut().map(item),
        char(','),
        char(close),
    )
    .preceded_by(char(open))
}

fn parse_spec(input: &str) -> ITResult<&str, Spec> {
    let (mut input, bank) = parse_light_bank.terminated(space0).parse(input)?;

    let mut buttons = Vec::new();

    let mut parse_button = braced_number_parser('(', ')', LightId).terminated(space0);
    let mut parse_joltages = braced_number_parser('{', '}', |i: i32| i);

    loop {
        let err = match parse_joltages.parse(input) {
            Ok((tail, joltages)) => {
                break Ok((
                    tail,
                    Spec {
                        bank,
                        buttons,
                        joltages,
                    },
                ));
            }
            Err(nom::Err::Error(err)) => err,
            Err(error) => break Err(error),
        };

        let (tail, button) = match parse_button.parse(input) {
            Ok((tail, button)) => (tail, button),
            Err(nom::Err::Error(err2)) => break Err(nom::Err::Error(err.or(err2))),
            Err(err) => break Err(err),
        };

        buttons.push(button);
        input = tail;
    }
}

#[derive(Debug)]
pub struct Input {
    specs: Vec<Spec>,
}

struct Attempt<'a> {
    bank: LightBank,
    count: u32,
    buttons: &'a [HashSet<LightId>],
}

fn parse_input(input: &str) -> ITResult<&str, Input> {
    collect_separated_terminated(parse_spec, multispace1, multispace0.terminated(eof))
        .map(|specs| Input { specs })
        .parse(input)
}

impl TryFrom<&str> for Input {
    type Error = ErrorTree<nom_supreme::final_parser::Location>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        final_parser(parse_input)(value)
    }
}

pub fn part1(input: Input) -> anyhow::Result<u32> {
    input
        .specs
        .iter()
        .map(|spec| {
            let mut queue = VecDeque::from([Attempt {
                bank: LightBank::new(),
                count: 0,
                buttons: &spec.buttons,
            }]);

            while let Some(attempt) = queue.pop_front() {
                if attempt.bank.lights == spec.bank {
                    return Ok(attempt.count);
                }

                let mut buttons = attempt.buttons;

                while let Some((button, tail)) = buttons.split_first() {
                    queue.push_back(Attempt {
                        bank: attempt.bank.with_toggled(button.iter().copied()),
                        count: attempt.count + 1,
                        buttons: tail,
                    });
                    buttons = tail;
                }
            }

            anyhow::bail!("impossible")
        })
        .enumerate()
        .try_fold(0, |sum, (i, res)| {
            res.map(|count| sum + count)
                .with_context(|| format!("item {i} was unsolvable"))
        })
}

pub fn part2(input: Input) -> anyhow::Result<Infallible> {
    anyhow::bail!("not implemented yet")
}
