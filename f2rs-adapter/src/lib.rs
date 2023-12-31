/*!
In this crate, we use macros to implement adapter functions like call, pow, and so on, instead of using functions. The reason for this is to avoid potential issues that could arise if the Fortran code defines local variables or functions that shadow these adapter functions.
*/

//#![deprecated = "Use `call_or_index` instead"]

pub mod types;
pub mod pow_operation;
pub mod common_operations;
pub mod iso_c_binding;

pub mod prelude {
    pub use crate::types::predefined::*;
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
    pub use crate::eq;
    pub use crate::ne;
    pub use crate::lt;
    pub use crate::le;
    pub use crate::gt;
    pub use crate::ge;
    pub use f2rs_adapter_derive::*;
    pub use crate::iso_c_binding;
    pub use crate::default;
    pub use crate::f2rs_version;
}

#[deprecated="TODO IMPLEMENT CHECKS"]
#[macro_export]
macro_rules! f2rs_version {
    ($($t:tt)*) => {
    };
}

pub const fn type_size<T>() -> usize {
    std::mem::size_of::<T>()
}

pub const fn value_size<T>(_: &T) -> usize {
    type_size::<T>()
}

#[macro_export]
macro_rules! fortran_kind {
    ($e:expr) => {
        $crate::value_size(&$e) as $crate::prelude::integer
    };
}

pub trait TypeProvider {
    type Type;
}

#[macro_export]
macro_rules! default {
    () => {
        std::default::Default::default()
    };
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
        $x as $crate::prelude::real
    };
}

pub trait CallOrIndex<Args> {
    type Output;
    //#[deprecated = "Use `call_or_index` instead"]
    fn call_or_index(&self, arg: Args) -> Self::Output;
}

macro_rules! impl_call_or_index_for_functions {
    () => {
        //impl<F, R> CallOrIndex<()> for F
        //where
        //    F: Fn() -> R,
        //{
        //    type Output = R;
        //    fn call_or_index(
        //        &self,
        //        _: (),
        //    ) -> Self::Output {
        //        self()
        //    }
        //}
    };
    ($head:ident $($tail:ident)*) => {
        impl_call_or_index_for_functions!($($tail)*);
        //#[allow(non_snake_case)]
        //impl<F, R, $head, $($tail),*> CallOrIndex<( $head, $($tail),* )> for F
        //where
        //    F: Fn($head, $($tail),*) -> R,
        //{
        //    type Output = R;
        //    fn call_or_index(
        //        &self,
        //        ($head, $($tail),*): ( $head, $($tail),* ),
        //    ) -> Self::Output {
        //        self($head, $($tail),*)
        //    }
        //}

        #[allow(non_snake_case)]
        impl<R, $head, $($tail),*> CallOrIndex<( $head, $($tail),* )> for fn( $head, $($tail),* ) -> R {
            type Output = R;
            fn call_or_index(
                &self,
                ($head, $($tail),*): ( $head, $($tail),* ),
            ) -> Self::Output {
                unsafe { self($head, $($tail),*) }
            }
        }
    };
}

impl_call_or_index_for_functions!(
     A1  A2  A3  A4  A5  A6  A7  A8  A9 A10
    A11 A12 A13 A14 A15 A16 A17 A18 A19 A20
    A21 A22 A23 A24 A25 A26 A27 A28 A29 A30
);

/// Invokes a fortran special function or statement function.
#[macro_export]
macro_rules! fortran {
    (print $($t:tt)*) => { $crate::fortran_print!($($t)*); };
    (PRINT $($t:tt)*) => { $crate::fortran_print!($($t)*); };
    (float($x:expr)) => { $crate::float!($x) };
    (FLOAT($x:expr)) => { $crate::float!($x) };
    (kind($x:expr)) => { $crate::fortran_kind!($x) };
    (KIND($x:expr)) => { $crate::fortran_kind!($x) };
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