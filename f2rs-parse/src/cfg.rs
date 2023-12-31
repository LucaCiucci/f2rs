

/// Standard
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Standard{
    Invalid,
    /// Fortran 2018 J3/18-007r1
    ///
    /// <https://j3-fortran.org/doc/year/18/18-007r1.pdf>
    // [Fortran 2018 J3/18-007r1](https://wg5-fortran.org/N2151-N2200/N2184.pdf)
    F18V007r1,
}

impl Standard {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Invalid => "Invalid",
            Self::F18V007r1 => "Fortran 2018 J3/18-007r1",
        }
    }
}

pub struct Cfg {
    pub standard: Standard,
}

impl Cfg {
    pub fn new(standard: Standard) -> Self {
        Self { standard }
    }

    pub fn invalid() -> Self {
        Self::new(Standard::Invalid)
    }

    /// Defaults to Fortran 2018 J3/18-007r1
    pub fn f2018() -> Self {
        Self::new(Standard::F18V007r1)
    }
}

impl From<Standard> for Cfg {
    fn from(standard: Standard) -> Self {
        Self::new(standard)
    }
}

macro_rules! dbg_compatible_with {
    ($cfg:expr, $($v:expr),*$(,)?) => {
        debug_assert!(
            [
                $(
                    $crate::Standard::from($v),
                )*
            ].contains(&$cfg.standard),
            "The current fortran standard \"{}\" ({:?}) is not compatible with this parser",
            $cfg.standard.name(), $cfg.standard
        )
    };
}

//#[allow(unused)]
//macro_rules! panics_for_invalid {
//    ($f:ident) => {
//        paste::paste!{
//            #[test]
//            #[should_panic]
//            pub fn [<$f _invalid_cfg>]() {
//                $f::<&str>(&$crate::Cfg::invalid());
//            }
//        }
//    };
//    ($f:ident($($args:expr),*)) => {
//        paste::paste!{
//            #[test]
//            #[should_panic]
//            pub fn [<test_ $f _invalid_cfg>]() {
//                $f::<&str>(&$crate::Cfg::invalid(), $($args),*);
//            }
//        }
//    };
//}

pub(crate) use dbg_compatible_with;
//#[allow(unused)]
//pub(crate) use panics_for_invalid;