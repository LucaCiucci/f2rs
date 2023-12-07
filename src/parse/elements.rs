
pub use riddle::provided::text::rusty::identifier;

mod literals; pub use literals::*;
mod comment; pub use comment::*;
mod space; pub use space::*;
mod operator; pub use operator::*;
mod expression; pub use expression::*;
mod keyword; pub use keyword::*;
mod integer; pub use integer::*;
mod statement; pub use statement::*;
mod type_; pub use type_::*;
mod eol; pub use eol::*;
