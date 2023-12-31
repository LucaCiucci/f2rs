use std::{io::Write, path::{Path, PathBuf}};

use anyhow::{Result, anyhow};
use f2rs_parser_combinator::prelude::*;

use crate::code_gen::file_2_rs;

mod code_gen;
mod parse;

use clap::Parser;

/// A Fortran to Rust transpiler
#[derive(Debug, Clone, Parser)]
enum Cli {
    /// Transpile a Fortran file to Rust
    Transpile(Transpile),
    /// Project
    Project(Project),
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli {
        Cli::Transpile(convert) => {
            let _out_file = transpile_file(convert)?;
        }
        Cli::Project(project) => {
            transpile_project(project)?;
        }
    }

    Ok(())
}

#[derive(Debug, Clone, Parser)]
struct Transpile {
    /// The Fortran file to transpile
    file: String,

    /// The Rust file to write to
    out: String,

    /// If set, the Rust file will not be formatted
    #[arg(long)]
    no_fmt: bool,

    /// Show output
    #[arg(long)]
    verbose: bool,
}

fn transpile_file(args: Transpile) -> Result<String> {
    let src = load_file(&args.file)?;

    let file = parse::elements().parse(&src[..]);

    // input file relative to output file dir
    let in_abs = std::fs::canonicalize(&args.file)?;
    let out_abs = std::fs::canonicalize(&args.out)?;
    let input_relative = pathdiff::diff_paths(&in_abs, &out_abs).unwrap();

    // write to file
    //let out = std::fs::File::create("parsed.rs").unwrap();
    //let mut out = std::io::BufWriter::new(out);
    //let file = file.0.unwrap();
    //write!(out, "{file:#?}").unwrap();

    let (parsed_file, unparsed) = (file.0, file.1);

    if let Some(parsed_file) = parsed_file {
        {
            let mut rs = file_2_rs(&parsed_file, Some(input_relative.to_str().unwrap()));

            if unparsed.len() > 0 {
                rs = rs + "\n\n// UNPARSED by f2rs:\n\n/*\n" + &unparsed + "\n*/";
            }

            let out = std::fs::File::create(&args.out)?;
            let mut out = std::io::BufWriter::new(out);
            write!(out, "{}", rs)?;
        }

        if !args.no_fmt {
            let out = std::process::Command::new("rustfmt")
                .arg(&args.out)
                .output()?;
            let out = String::from_utf8(out.stdout).unwrap();
            if args.verbose {
                println!("{}", out);
            }
        }

        Ok(args.out)
    } else {
        Err(anyhow!("Failed to parse file"))
    }
}

fn load_file(path: impl AsRef<Path>) -> Result<String> {
    let path = path.as_ref();

    let src = std::fs::read_to_string(path)?.replace("\r", "");
    //.replace("\\", "\\\\")
    //.replace("//", "\\//")
    //.replace("!", "//")
    //.replace("\\//", "!")
    //.replace("\\\\", "\\");

    Ok(src)
}

#[derive(Debug, Clone, Parser)]
struct Project {
    working_dir: String,
}

fn transpile_project(args: Project) -> Result<()> {
    let working_dir = std::env::current_dir()?.join(&args.working_dir);
    let manifest_file = working_dir.join("f2rs.toml");

    if !manifest_file.exists() {
        return Err(anyhow!("No f2rs.toml file found in {}", args.working_dir));
    }

    let manifest = std::fs::read_to_string(manifest_file)?;
    let manifest: toml::Value = toml::from_str(&manifest)?;

    let input_dir = working_dir.join(&manifest["input-dir"].as_str().ok_or(anyhow!("No input_dir in f2rs.toml"))?);
    let output_dir = working_dir.join(&manifest["output-dir"].as_str().ok_or(anyhow!("No output_dir in f2rs.toml"))?);

    let mut modules = vec![];
    transpile_item(&input_dir, &output_dir, &mut modules)?;

    {
        let mut out = std::fs::File::create(output_dir.join("mod.rs"))?;
        for (name, path) in modules {
            let relative_path = path.strip_prefix(&output_dir)?.to_str().unwrap().to_string();
            writeln!(out, "#[path=\"{}\"]", relative_path)?;
            writeln!(out, "mod {}_mod_; pub use {}_mod_::*;", name, name)?;
        }
    }

    Ok(())
}

/// Convert all files in dir and subdirs
fn transpile_item(
    path: impl AsRef<Path>,
    out_dir: &Path,
    modules: &mut Vec<(String, PathBuf)>, // name, path
) -> Result<()> {
    let path = path.as_ref();
    if !out_dir.exists() {
        std::fs::create_dir(&out_dir)?;
    }

    if path.is_dir() {
        for entry in std::fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            let out_dir = if path.is_dir() {
                out_dir.join(path.file_name().unwrap())
            } else {
                out_dir.to_path_buf()
            };
            transpile_item(&path, &out_dir, modules)?;
        }
    } else {
        if let Some(ext) = path.extension() {
            if ext == "f90" || ext == "f" {
                println!("transpiling {}", path.to_str().unwrap());
                let out_file = out_dir.join(path.file_name().unwrap()).with_extension("rs");
                let out_file = out_file.to_str().unwrap().to_string();
                let name = path.file_stem().unwrap().to_str().unwrap().to_string();
                let p = transpile_file(Transpile {
                    file: path.to_str().unwrap().to_string(),
                    out: out_file,
                    no_fmt: false,
                    verbose: false,
                })?;
                modules.push((name, p.into()));
            }
        }
    }

    Ok(())
}