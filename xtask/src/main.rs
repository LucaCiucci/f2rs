
use std::process::Command;

use clap::Parser;


#[derive(Parser, Debug)]
enum Cli {
    ConvertExamples,
}

fn main() {
    let cli = Cli::parse();

    match cli {
        Cli::ConvertExamples => {
            convert_examples();
        }
    }
}

fn convert_examples() {
    let examples_dir = std::env::current_dir().unwrap().join("examples");
    
    // list all f90 files in examples directory
    let mut f90_files = vec![];
    for entry in std::fs::read_dir(examples_dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext == "f90" || ext == "f" || ext == "mod" {
                    f90_files.push(path);
                }
            }
        }
    }

    // convert each f90 file to rs
    for f90_file in f90_files {
        let rs_file = f90_file.with_extension("rs");
        let out = Command::new("cargo")
            //.args(["run", "--", f90_file.to_str().unwrap(), rs_file.to_str().unwrap()])
            .arg("run")
            .arg("--")
            .arg("transpile")
            .arg(f90_file)
            .arg(rs_file)
            .stdout(std::process::Stdio::inherit())
            .stderr(std::process::Stdio::inherit())
            .stdin(std::process::Stdio::inherit())
            .output()
            .expect("failed to execute process");
        let out = String::from_utf8(out.stdout).unwrap();
        println!("{}", out);
    }
}