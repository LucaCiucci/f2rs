#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]

// https://fortranwiki.org/fortran/show/iso_c_binding

use crate::prelude::*;

macro_rules! binding_type {
    ($name:ident => $rust_type:ty) => {
        pub const $name: integer = std::mem::size_of::<$rust_type>() as integer;
    };
}

binding_type!(c_int => std::ffi::c_int);
binding_type!(c_short => std::ffi::c_short);
binding_type!(c_long => std::ffi::c_long);
binding_type!(c_long_long => std::ffi::c_longlong);
binding_type!(c_signed_char => std::ffi::c_schar);
binding_type!(c_size_t => std::ffi::c_ulong);
binding_type!(c_int8_t => std::os::raw::c_schar);
binding_type!(c_int16_t => std::os::raw::c_short);
binding_type!(c_int32_t => std::os::raw::c_int);
binding_type!(c_int64_t => std::os::raw::c_longlong);
binding_type!(c_int_least8_t => std::os::raw::c_schar);
binding_type!(c_int_least16_t => std::os::raw::c_short);
binding_type!(c_int_least32_t => std::os::raw::c_int);
binding_type!(c_int_least64_t => std::os::raw::c_longlong);
binding_type!(c_int_fast8_t => std::os::raw::c_schar);
binding_type!(c_int_fast16_t => std::os::raw::c_int);
binding_type!(c_int_fast32_t => std::os::raw::c_int);
binding_type!(c_int_fast64_t => std::os::raw::c_longlong);
binding_type!(c_intmax_t => std::os::raw::c_longlong);
binding_type!(c_intptr_t => std::os::raw::c_long);
binding_type!(c_ptrdiff_t => std::os::raw::c_long);
binding_type!(c_float => std::os::raw::c_float);
binding_type!(c_double => std::os::raw::c_double);
binding_type!(c_long_double => std::os::raw::c_double);
binding_type!(c_float_complex => std::os::raw::c_float);
binding_type!(c_double_complex => std::os::raw::c_double);
binding_type!(c_long_double_complex => std::os::raw::c_double);
binding_type!(c_bool => std::os::raw::c_int);
binding_type!(c_char => std::os::raw::c_char);

pub type c_void = std::ffi::c_void;
pub type c_funptr = *mut extern "C" fn(); // TODO ???
//pub type c_funptr = *mut std::ffi::c_void; // TODO ???

// TODO to check
pub const c_null_char: character<c_char> = 0;
pub const c_alert: character<c_char> = 7;
pub const c_backspace: character<c_char> = 8;
pub const c_form_feed: character<c_char> = 12;
pub const c_new_line: character<c_char> = 10;
pub const c_carriage_return: character<c_char> = 13;
pub const c_horizontal_tab: character<c_char> = 9;
pub const c_vertical_tab: character<c_char> = 11;

pub const c_null_ptr: *mut c_void = 0 as *mut c_void;
pub const c_null_funptr: c_funptr = 0 as c_funptr;
