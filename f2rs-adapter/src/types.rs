#![allow(non_camel_case_types)]

pub type integer1 = i8;
pub type integer2 = i16;
pub type integer4 = i32;
pub type integer8 = i64;
pub type integer16 = i128;

cfg_if::cfg_if! {
    if #[cfg(feature="default-integer-precision-8")] {
        pub type integer = i8;
    } else if #[cfg(feature="default-integer-precision-16")] {
        pub type integer = i16;
    } else if #[cfg(feature="default-integer-precision-32")] {
        pub type integer = i32;
    } else if #[cfg(feature="default-integer-precision-64")] {
        pub type integer = i64;
    } else if #[cfg(feature="default-integer-precision-128")] {
        pub type integer = i128;
    } else {
        pub type integer = i32;
    }
}

pub type real4 = f32;
pub type real8 = f64;
#[cfg(feature = "f128")]
pub type real16 = f128;
pub type double_precision = f64;

cfg_if::cfg_if! {
    if #[cfg(feature="default-real-precision-32")] {
        pub type real = f32;
    } else if #[cfg(feature="default-real-precision-64")] {
        pub type real = f64;
    } else if #[cfg(feature="default-real-precision-128")] {
        pub type real = f128;
    } else {
        pub type real = f64;
    }
}

use num::complex::Complex as NumComplex;

pub type complex8 = NumComplex<f32>;
pub type complex16 = NumComplex<f64>;
#[cfg(feature = "f128")]
pub type complex32 = NumComplex<f128>;

pub type complex = NumComplex<real>;
pub type double_complex = NumComplex<f64>;

pub type character = char;
pub type characterN<const N: usize> = [char; N];

pub type logical = bool;
pub type logical1 = bool;
//pub type logical2 = bool;
//pub type logical4 = bool;
//pub type logical8 = bool;
