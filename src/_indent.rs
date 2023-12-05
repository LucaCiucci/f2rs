use std::fmt::Display;
use std::ops::Add;


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Indent(pub usize);

impl Display for Indent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.0 {
            write!(f, "    ")?;
        }
        Ok(())
    }
}

impl Add<usize> for Indent {
    type Output = Indent;

    fn add(self, rhs: usize) -> Self::Output {
        self + Indent(rhs)
    }
}

impl Add<Indent> for Indent {
    type Output = Indent;

    fn add(self, rhs: Indent) -> Self::Output {
        Indent(self.0 + rhs.0)
    }
}