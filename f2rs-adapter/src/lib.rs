//#![deprecated = "Use `call_or_index` instead"]

pub mod types;

pub mod prelude {
    pub use crate::types::*;
    pub use crate::CallOrIndex;
    pub use crate::call;
}

pub trait CallOrIndex<Args> {
    type Output;
    //#[deprecated = "Use `call_or_index` instead"]
    fn call_or_index(&self, arg: Args) -> Self::Output;
}

macro_rules! impl_call_or_index_for_functions {
    () => {
        impl<F, R> CallOrIndex<()> for F
        where
            F: Fn() -> R,
        {
            type Output = R;
            fn call_or_index(
                &self,
                _: (),
            ) -> Self::Output {
                self()
            }
        }
    };
    ($head:ident $($tail:ident)*) => {
        impl_call_or_index_for_functions!($($tail)*);
        #[allow(non_snake_case)]
        impl<F, R, $head, $($tail),*> CallOrIndex<( $head, $($tail),* )> for F
        where
            F: Fn($head, $($tail),*) -> R,
        {
            type Output = R;
            fn call_or_index(
                &self,
                ($head, $($tail),*): ( $head, $($tail),* ),
            ) -> Self::Output {
                self($head, $($tail),*)
            }
        }
    };
}

impl_call_or_index_for_functions!(
     A1  A2  A3  A4  A5  A6  A7  A8  A9 A10
    A11 A12 A13 A14 A15 A16 A17 A18 A19 A20
    A21 A22 A23 A24 A25 A26 A27 A28 A29 A30
);

#[macro_export]
macro_rules! args_ref {
    (value($v:expr), $($args:tt)*) => { $v, $crate::args_ref!($($args)*) };
    (VALUE($v:expr), $($args:tt)*) => { $v, $crate::args_ref!($($args)*) };
    (value($v:expr)) => { $v };
    (VALUE($v:expr)) => { $v };
    ($v:expr, $($args:tt)*) => { &mut $v, $crate::args_ref!($($args)*) };
    ($v:expr) => { &mut $v };
    () => {
    };
}

//#[deprecated="Remove this macro"]
#[macro_export]
macro_rules! call {
    ($f:ident($($args:tt)*)) => {
        $crate::CallOrIndex::call_or_index(&$f, ($crate::args_ref!($($args)*),))
    };
}