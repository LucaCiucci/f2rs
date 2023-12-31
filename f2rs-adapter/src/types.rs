#![allow(non_camel_case_types)]

use num::complex::Complex as NumComplex;

use crate::TypeProvider;

macro_rules! type_with_kind {
    ($name:ident (default: $default:expr) => $provider:ident) => {
        pub type $name<const KIND: $crate::types::default_integer = $default> = <$provider<KIND> as $crate::TypeProvider>::Type;
    };
}

pub mod predefined {
    use super::*;

    type_with_kind!(integer (default: DEFAULT_INTEGER_KIND) => IntegerProvider);
    type_with_kind!(real (default: DEFAULT_REAL_KIND) => RealProvider);
    type_with_kind!(complex (default: DEFAULT_COMPLEX_KIND) => ComplexProvider);
    type_with_kind!(character (default: DEFAULT_CHARACTER_KIND) => CharacterProvider);

    pub type double_complex = complex<16>;
    pub type double_precision = real<8>;
    
    pub type logical = bool;
    pub type logical1 = bool;
    //pub type logical2 = bool;
    //pub type logical4 = bool;
    //pub type logical8 = bool;
}

cfg_if::cfg_if! {
    if #[cfg(feature="default-integer-precision-8")] {
        type default_integer = i8;
    } else if #[cfg(feature="default-integer-precision-16")] {
        type default_integer = i16;
    } else if #[cfg(feature="default-integer-precision-32")] {
        type default_integer = i32;
    } else if #[cfg(feature="default-integer-precision-64")] {
        type default_integer = i64;
    } else if #[cfg(feature="default-integer-precision-128")] {
        type default_integer = i128;
    } else {
        type default_integer = i32; // TODO check
    }
}

cfg_if::cfg_if! {
    if #[cfg(feature="default-real-precision-32")] {
        type default_real = f32;
    } else if #[cfg(feature="default-real-precision-64")] {
        type default_real = f64;
    } else if #[cfg(feature="default-real-precision-128")] {
        type default_real = f128;
    } else {
        type default_real = f32; // TODO maybe change this to f64? check what is the default in Fortran
    }
}

macro_rules! default_kind {
    ($name:ident => $rust_type:ty) => {
        pub const $name: default_integer = std::mem::size_of::<$rust_type>() as default_integer;
    };
    ($name:ident = { $v: expr }) => {
        pub const $name: default_integer = $v;
    };
}

default_kind!(DEFAULT_INTEGER_KIND => default_integer);
default_kind!(DEFAULT_REAL_KIND => default_real);
default_kind!(DEFAULT_COMPLEX_KIND = { DEFAULT_REAL_KIND * 2 });
default_kind!(DEFAULT_CHARACTER_KIND => char);

pub struct IntegerProvider<const KIND: default_integer>;
pub struct RealProvider<const KIND: default_integer>;
pub struct ComplexProvider<const KIND: default_integer>;
pub struct CharacterProvider<const KIND: default_integer>;


macro_rules! impl_integer_provider {
    ($s:expr => $t:ty) => {
        impl TypeProvider for IntegerProvider<$s> {
            type Type = $t;
        }
    };
}

impl_integer_provider!(1 => i8);
impl_integer_provider!(2 => i16);
impl_integer_provider!(4 => i32);
impl_integer_provider!(8 => i64);
impl_integer_provider!(16 => i128);

macro_rules! impl_real_provider {
    ($s:expr => $t:ty) => {
        impl TypeProvider for RealProvider<$s> {
            type Type = $t;
        }
    };
}

impl_real_provider!(4 => f32);
impl_real_provider!(8 => f64);
#[cfg(feature = "f128")]
impl_real_provider!(16 => f128);

macro_rules! impl_complex_provider {
    ($s:expr => $t:ty) => {
        impl TypeProvider for ComplexProvider<$s> {
            type Type = NumComplex<$t>;
        }
    };
}

impl_complex_provider!(8 => f32);
impl_complex_provider!(16 => f64);
#[cfg(feature = "f128")]
impl_complex_provider!(32 => f128);

macro_rules! impl_character_provider {
    ($s:expr => $t:ty) => {
        impl TypeProvider for CharacterProvider<$s> {
            type Type = $t;
        }
    };
}

impl_character_provider!(1 => u8);
impl_character_provider!(2 => u16);
impl_character_provider!(4 => char);