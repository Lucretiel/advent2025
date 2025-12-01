use std::{
    env,
    fs::{File, read_dir},
    io::Write,
    path::PathBuf,
};

use lazy_format::lazy_format;
use nom::{IResult, Parser, bytes::complete::tag, character::complete::digit1};
use nom_supreme::ParserExt;

fn parse_day_filename(input: &str) -> IResult<&str, i32, ()> {
    tag("day")
        .precedes(digit1)
        .terminated(tag(".rs"))
        .parse_from_str()
        .all_consuming()
        .parse(input)
}

fn main() {
    let project_root = env::current_dir().expect("couldn't get working directory");
    let source_directory = project_root.join("src");

    println!("cargo:rerun-if-changed={}", source_directory.display());

    let items = read_dir(&source_directory).expect("couldn't open the source directory");

    let days: Vec<i32> = items
        .map(|item| item.expect("failed to read directory entry"))
        .filter(|item| item.file_type().unwrap().is_file())
        .filter_map(|item| {
            parse_day_filename(
                item.path()
                    .file_name()
                    .expect("file has no filename")
                    .to_str()
                    .expect("filename wasn't valid utf8"),
            )
            .ok()
            .map(|(_, day)| day)
        })
        .collect();

    let days = days.as_slice();

    let mods = lazy_format!(
        // HATE HATE HATE HATE
        "#[path = \"../../../../../src/day{day}.rs\"] mod day{day};\n"
        for day in days
    );

    let enum_variants = lazy_format!("Day{day},\n" for day in days);
    let match_arms = lazy_format!("{day} => Ok(Day::Day{day}),\n" for day in days);
    let usage_days = lazy_format!("\"{day}\"," for day in days);
    let solver_match_arms = lazy_format!(
        "#[allow(clippy::unnecessary_fallible_conversions)]
        #[allow(clippy::useless_conversion)]
        (Day::Day{day}, Part::Part{part}) => input
            .try_into()
            .inspect(|input| {{
                if show_input {{
                    eprintln!(\"Parsed input:\n{{input:#?}}\");
                }}
            }})
            .context(\"failed to parse input\")
            .and_then(|input| day{day}::part{part}(input).context(\"failed to compute solution after successful parse\"))
            .context(\"failed to solve day {day}, part {part}\")
            .map(|solution| println!(\"{{solution}}\")),\n"
        for (day, part) in days.iter().flat_map(|&day| [(day, 1), (day, 2)])
    );

    let generated_content = lazy_format!(
        "
        {mods}

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum Day {{
            {enum_variants}
        }}

        impl FromStr for Day {{
            type Err = DayError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {{
                let value: u8 = s.parse()?;

                match value {{
                    {match_arms}
                    value => Err(DayError::BadDay(value)),
                }}
            }}
        }}

        impl ParsedValue for Day {{}}

        impl ParameterUsage for Day {{
            const VALUE: ParameterValueKind = ParameterValueKind::OneOf(&[
                {usage_days}
            ]);
            const REQUIREMENT: Requirement = Requirement::Mandatory;
            const REPETITION: Repetition = Repetition::Single;
        }}

        fn run_solution(day: Day, part: Part, input: &str, show_input: bool) -> anyhow::Result<()> {{
            match (day, part) {{
                {solver_match_arms}
            }}
        }}"
    );

    let generated_content = generated_content.to_string();

    let output_path = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR not set in build.rs"));
    let output_path = output_path.join("generated.rs");

    let mut output = File::create(output_path).expect("failed to create generated.rs");
    output
        .write_all(generated_content.as_bytes())
        .expect("failed to write to generated.rs");

    output.flush().expect("failed to write to generated.rs");
}
