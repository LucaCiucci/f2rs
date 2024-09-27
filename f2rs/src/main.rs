use clap::Parser;
use f2rs::programs::{interactive_tokenization, InteractiveTokenization};


#[derive(Debug, Parser)]
enum Command {
    InteractiveTokenization(InteractiveTokenization),
}

fn main() {
    env_logger::builder()
        //.filter_level(LevelFilter::Trace)
        .init();

    let command = Command::parse();

    match command {
        Command::InteractiveTokenization(args) => interactive_tokenization(args),
    }
}
