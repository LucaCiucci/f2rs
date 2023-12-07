

macro_rules! test_parse {
    ($p:expr, $s:literal => $check:expr) => {
        use riddle::prelude::*;
        let r = $p.parse($s);
        assert!(r.0.is_some(), "failed to parse");
        assert!($check(r.0.unwrap()));
    };
    ($p:expr, $s:literal) => {
        $crate::testing::test_parse!($p, $s => |_| true);
    }
}

pub(crate) use test_parse;