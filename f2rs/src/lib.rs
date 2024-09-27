use clap::ValueEnum;


pub mod programs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[derive(ValueEnum)]
pub enum ColorArg {
    Red,
    Green,
    Blue,
    Yellow,
    Cyan,
    Magenta,
    Black,
    White,
    BrightRed,
    BrightGreen,
    BrightBlue,
    BrightYellow,
    BrightCyan,
    BrightMagenta,
    BrightBlack,
}

impl ColorArg {
    pub fn to_colored(self) -> colored::Color {
        match self {
            Self::Red => colored::Color::Red,
            Self::Green => colored::Color::Green,
            Self::Blue => colored::Color::Blue,
            Self::Yellow => colored::Color::Yellow,
            Self::Cyan => colored::Color::Cyan,
            Self::Magenta => colored::Color::Magenta,
            Self::Black => colored::Color::Black,
            Self::White => colored::Color::White,
            Self::BrightRed => colored::Color::BrightRed,
            Self::BrightGreen => colored::Color::BrightGreen,
            Self::BrightBlue => colored::Color::BrightBlue,
            Self::BrightYellow => colored::Color::BrightYellow,
            Self::BrightCyan => colored::Color::BrightCyan,
            Self::BrightMagenta => colored::Color::BrightMagenta,
            Self::BrightBlack => colored::Color::BrightBlack,
        }
    }
}