/*!
In this crate, we use macros to implement adapter functions like call, pow, and so on, instead of using functions. The reason for this is to avoid potential issues that could arise if the Fortran code defines local variables or functions that shadow these adapter functions.
*/

//#![deprecated = "Use `call_or_index` instead"]

pub mod types;
pub mod pow_operation;
pub mod common_operations;

pub mod prelude {
    pub use crate::types::*;
    pub use crate::call;
    pub use crate::fortran;
    pub use crate::CallOrIndex;
    pub use crate::pow;
    pub use crate::pow_operation::FortranPow;
    pub use crate::float;
    pub use crate::assign;
    pub use crate::add;
    pub use crate::sub;
    pub use crate::mul;
    pub use crate::div;
    pub use f2rs_adapter_derive::rewrite_fortran_goto;
    pub use f2rs_adapter_derive::fortran_body;
    pub use f2rs_adapter_derive::fortran_label;
    pub use f2rs_adapter_derive::fortran_goto;
}

//pub fn float<T>(x: impl std::borrow::Borrow<T>) -> types::real
//where
//    T: Into<f64> + Clone,
//{
//    x.borrow().clone().into()
//}

#[macro_export]
macro_rules! float {
    ($x:expr) => {
        $x as $crate::types::real
    };
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
macro_rules! arg_ref {
    (value($v:expr)) => { $v };
    (VALUE($v:expr)) => { $v };
    ($v:expr) => { &mut $v };
}

#[macro_export]
macro_rules! args_ref {
    ($($args:expr),*) => { ($($crate::arg_ref!($args)),*,) };
}

//#[deprecated="Remove this macro"]
#[macro_export]
macro_rules! call {
    ($f:ident($($args:tt)*)) => {
        $crate::CallOrIndex::call_or_index(&$f, $crate::args_ref!($($args)*))
    };
}

/// Invokes a fortran special function or statement function.
#[macro_export]
macro_rules! fortran {
    (print $($t:tt)*) => { $crate::fortran_print!($($t)*); };
    (PRINT $($t:tt)*) => { $crate::fortran_print!($($t)*); };
    (float($x:expr)) => { $crate::float!($x) };
    (FLOAT($x:expr)) => { $crate::float!($x) };
    (stop) => { $crate::fortran_stop!(); };
    (STOP) => { $crate::fortran_stop!(); };
}

#[macro_export]
macro_rules! fortran_print {
    (*, $($v:expr),*) => {
        println!(concat!($($crate::position_string!($v), " ",)*), $($v),*);
    };
    ($($t:tt)*) => {
        todo!("unimplemented fortran_print! macro for {:?}", stringify!($($t)*));
    };
}

#[macro_export]
macro_rules! position_string {
    ($($t:tt)*) => { "\t{}" };
}

#[macro_export]
macro_rules! fortran_stop {
    () => {
        std::process::exit(0);
    };
}