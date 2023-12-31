
pub trait FortranAssign<Rhs = Self> {
    fn fortran_assign(&mut self, rhs: Rhs);
}

#[macro_export]
macro_rules! impl_assign_for_fortran {
    ($a:ty, $b:ty) => {
        impl FortranAssign<$b> for $a {
            fn fortran_assign(&mut self, rhs: $b) {
                *self = rhs as $a;
            }
        }
        impl FortranAssign<&mut $b> for $a {
            fn fortran_assign(&mut self, rhs: &mut $b) {
                *self = *rhs as $a;
            }
        }
        impl FortranAssign<$b> for &mut $a {
            fn fortran_assign(&mut self, rhs: $b) {
                **self = rhs as $a;
            }
        }
        impl FortranAssign<&mut $b> for &mut $a {
            fn fortran_assign(&mut self, rhs: &mut $b) {
                **self = *rhs as $a;
            }
        }
    };
    ($($a:ty, $b:ty);*;) => {
        $(impl_assign_for_fortran!($a, $b);)*
    };
}

#[macro_export]
macro_rules! assign {
    ($lhs:expr, $rhs:expr) => {
        let r = $rhs;
        $crate::common_operations::FortranAssign::fortran_assign(&mut $lhs, r)
    }
}

macro_rules! define_operation {
    ($trait_name:ident $function_name:ident $implementer:ident => $($impl:tt)*) => {
        pub trait $trait_name<Rhs = Self> {
            type Output;
            fn $function_name(self, rhs: Rhs) -> Self::Output;
        }
        macro_rules! $implementer {
            ($a:ty, $b:ty => $r:ty) => {
                impl $trait_name<$b> for $a {
                    type Output = $r;
                    fn $function_name(self, rhs: $b) -> Self::Output {
                        (self as $r) $($impl)* (rhs as $r)
                    }
                }
                impl $trait_name<&$b> for $a {
                    type Output = $r;
                    fn $function_name(self, rhs: &$b) -> Self::Output {
                        (self as $r) $($impl)* (*rhs as $r)
                    }
                }
                impl $trait_name<$b> for &$a {
                    type Output = $r;
                    fn $function_name(self, rhs: $b) -> Self::Output {
                        (*self as $r) $($impl)* (rhs as $r)
                    }
                }
                impl $trait_name<&$b> for &$a {
                    type Output = $r;
                    fn $function_name(self, rhs: &$b) -> Self::Output {
                        (*self as $r) $($impl)* (*rhs as $r)
                    }
                }
            }
        }
    };
}

macro_rules! define_logical_operation {
    ($trait_name:ident $function_name:ident $implementer:ident => $($impl:tt)*) => {
        pub trait $trait_name<Rhs = Self> {
            fn $function_name(self, rhs: Rhs) -> bool;
        }
        macro_rules! $implementer {
            ($a:ty, $b:ty => $r:ty) => {
                impl $trait_name<$b> for $a {
                    fn $function_name(self, rhs: $b) -> bool {
                        (self as $r) $($impl)* (rhs as $r)
                    }
                }
                impl $trait_name<&$b> for $a {
                    fn $function_name(self, rhs: &$b) -> bool {
                        (self as $r) $($impl)* (*rhs as $r)
                    }
                }
                impl $trait_name<$b> for &$a {
                    fn $function_name(self, rhs: $b) -> bool {
                        (*self as $r) $($impl)* (rhs as $r)
                    }
                }
                impl $trait_name<&$b> for &$a {
                    fn $function_name(self, rhs: &$b) -> bool {
                        (*self as $r) $($impl)* (*rhs as $r)
                    }
                }
            }
        }
    };
}

define_operation!(FortranAdd fortran_add impl_fortran_add => +);
define_operation!(FortranSub fortran_sub impl_fortran_sub => -);
define_operation!(FortranMul fortran_mul impl_fortran_mul => *);
define_operation!(FortranDiv fortran_div impl_fortran_div => /);

define_logical_operation!(FortranEq fortran_eq impl_fortran_eq => ==);
define_logical_operation!(FortranNe fortran_ne impl_fortran_ne => !=);
define_logical_operation!(FortranLt fortran_lt impl_fortran_lt => <);
define_logical_operation!(FortranLe fortran_le impl_fortran_le => <=);
define_logical_operation!(FortranGt fortran_gt impl_fortran_gt => >);
define_logical_operation!(FortranGe fortran_ge impl_fortran_ge => >=);

#[macro_export]
macro_rules! add {
    ($lhs:expr, $rhs:expr) => {
        $crate::common_operations::FortranAdd::fortran_add($lhs, $rhs)
    }
}

#[macro_export]
macro_rules! sub {
    ($lhs:expr, $rhs:expr) => {
        $crate::common_operations::FortranSub::fortran_sub($lhs, $rhs)
    }
}

#[macro_export]
macro_rules! mul {
    ($lhs:expr, $rhs:expr) => {
        $crate::common_operations::FortranMul::fortran_mul($lhs, $rhs)
    }
}

#[macro_export]
macro_rules! div {
    ($lhs:expr, $rhs:expr) => {
        $crate::common_operations::FortranDiv::fortran_div($lhs, $rhs)
    }
}

#[macro_export]
macro_rules! eq {
    ($lhs:expr, $rhs:expr) => {
        $crate::common_operations::FortranEq::fortran_eq($lhs, $rhs)
    }
}

#[macro_export]
macro_rules! ne {
    ($lhs:expr, $rhs:expr) => {
        $crate::common_operations::FortranNe::fortran_ne($lhs, $rhs)
    }
}

#[macro_export]
macro_rules! lt {
    ($lhs:expr, $rhs:expr) => {
        $crate::common_operations::FortranLt::fortran_lt($lhs, $rhs)
    }
}

#[macro_export]
macro_rules! le {
    ($lhs:expr, $rhs:expr) => {
        $crate::common_operations::FortranLe::fortran_le($lhs, $rhs)
    }
}

#[macro_export]
macro_rules! gt {
    ($lhs:expr, $rhs:expr) => {
        $crate::common_operations::FortranGt::fortran_gt($lhs, $rhs)
    }
}

#[macro_export]
macro_rules! ge {
    ($lhs:expr, $rhs:expr) => {
        $crate::common_operations::FortranGe::fortran_ge($lhs, $rhs)
    }
}

macro_rules! define_cast_rule {
    ($a:ty, $b:ty => $r:ty) => {
        impl_assign_for_fortran!($a, $b);

        impl_fortran_add!($a, $b => $r);
        impl_fortran_sub!($a, $b => $r);
        impl_fortran_mul!($a, $b => $r);
        impl_fortran_div!($a, $b => $r);

        impl_fortran_eq!($a, $b => $r);
        impl_fortran_ne!($a, $b => $r);
        impl_fortran_lt!($a, $b => $r);
        impl_fortran_le!($a, $b => $r);
        impl_fortran_gt!($a, $b => $r);
        impl_fortran_ge!($a, $b => $r);
    };
    ($($a:ty, $b:ty => $r:ty);*;) => {
        $(define_cast_rule!($a, $b => $r);)*
    };
}

define_cast_rule!(
    u8, u8 => u8;
    u8, u16 => u16;
    u8, u32 => u32;
    u8, u64 => u64;
    u8, u128 => u128;
    u8, usize => usize;
    u8, i8 => i8;
    u8, i16 => i16;
    u8, i32 => i32;
    u8, i64 => i64;
    u8, i128 => i128;
    u8, isize => isize;
    u8, f32 => f32;
    u8, f64 => f64;
    i8, u8 => i8;
    i8, u16 => i16;
    i8, u32 => i32;
    i8, u64 => i64;
    i8, u128 => i128;
    i8, usize => isize;
    i8, i8 => i8;
    i8, i16 => i16;
    i8, i32 => i32;
    i8, i64 => i64;
    i8, i128 => i128;
    i8, isize => isize;
    i8, f32 => f32;
    i8, f64 => f64;
    u16, u8 => u16;
    u16, u16 => u16;
    u16, u32 => u32;
    u16, u64 => u64;
    u16, u128 => u128;
    u16, usize => usize;
    u16, i8 => i16;
    u16, i16 => i16;
    u16, i32 => i32;
    u16, i64 => i64;
    u16, i128 => i128;
    u16, isize => isize;
    u16, f32 => f32;
    u16, f64 => f64;
    i16, u8 => i16;
    i16, u16 => i16;
    i16, u32 => i32;
    i16, u64 => i64;
    i16, u128 => i128;
    i16, usize => isize;
    i16, i8 => i16;
    i16, i16 => i16;
    i16, i32 => i32;
    i16, i64 => i64;
    i16, i128 => i128;
    i16, isize => isize;
    i16, f32 => f32;
    i16, f64 => f64;
    u32, u8 => u32;
    u32, u16 => u32;
    u32, u32 => u32;
    u32, u64 => u64;
    u32, u128 => u128;
    u32, usize => usize;
    u32, i8 => i32;
    u32, i16 => i32;
    u32, i32 => i32;
    u32, i64 => i64;
    u32, i128 => i128;
    u32, isize => isize;
    u32, f32 => f32;
    u32, f64 => f64;
    i32, u8 => i32;
    i32, u16 => i32;
    i32, u32 => i32;
    i32, u64 => i64;
    i32, u128 => i128;
    i32, usize => isize;
    i32, i8 => i32;
    i32, i16 => i32;
    i32, i32 => i32;
    i32, i64 => i64;
    i32, i128 => i128;
    i32, isize => isize;
    i32, f32 => f32;
    i32, f64 => f64;
    u64, u8 => u64;
    u64, u16 => u64;
    u64, u32 => u64;
    u64, u64 => u64;
    u64, u128 => u128;
    u64, usize => usize;
    u64, i8 => i64;
    u64, i16 => i64;
    u64, i32 => i64;
    u64, i64 => i64;
    u64, i128 => i128;
    u64, isize => isize;
    u64, f32 => f64;
    u64, f64 => f64;
    i64, u8 => i64;
    i64, u16 => i64;
    i64, u32 => i64;
    i64, u64 => i64;
    i64, u128 => i128;
    i64, usize => isize;
    i64, i8 => i64;
    i64, i16 => i64;
    i64, i32 => i64;
    i64, i64 => i64;
    i64, i128 => i128;
    i64, isize => isize;
    i64, f32 => f64;
    i64, f64 => f64;
    u128, u8 => u128;
    u128, u16 => u128;
    u128, u32 => u128;
    u128, u64 => u128;
    u128, u128 => u128;
    u128, usize => usize;
    u128, i8 => i128;
    u128, i16 => i128;
    u128, i32 => i128;
    u128, i64 => i128;
    u128, i128 => i128;
    u128, isize => isize;
    u128, f32 => f64;
    u128, f64 => f64;
    i128, u8 => i128;
    i128, u16 => i128;
    i128, u32 => i128;
    i128, u64 => i128;
    i128, u128 => i128;
    i128, usize => isize;
    i128, i8 => i128;
    i128, i16 => i128;
    i128, i32 => i128;
    i128, i64 => i128;
    i128, i128 => i128;
    i128, isize => isize;
    i128, f32 => f64;
    i128, f64 => f64;
    usize, u8 => usize;
    usize, u16 => usize;
    usize, u32 => usize;
    usize, u64 => usize;
    usize, u128 => usize;
    usize, usize => usize;
    usize, i8 => isize;
    usize, i16 => isize;
    usize, i32 => isize;
    usize, i64 => isize;
    usize, i128 => isize;
    usize, isize => isize;
    usize, f32 => f64;
    usize, f64 => f64;
    isize, u8 => isize;
    isize, u16 => isize;
    isize, u32 => isize;
    isize, u64 => isize;
    isize, u128 => isize;
    isize, usize => isize;
    isize, i8 => isize;
    isize, i16 => isize;
    isize, i32 => isize;
    isize, i64 => isize;
    isize, i128 => isize;
    isize, isize => isize;
    isize, f32 => f64;
    isize, f64 => f64;
    f32, u8 => f32;
    f32, u16 => f32;
    f32, u32 => f32;
    f32, u64 => f64;
    f32, u128 => f64;
    f32, usize => f64;
    f32, i8 => f32;
    f32, i16 => f32;
    f32, i32 => f32;
    f32, i64 => f64;
    f32, i128 => f64;
    f32, isize => f64;
    f32, f32 => f32;
    f32, f64 => f64;
    f64, u8 => f64;
    f64, u16 => f64;
    f64, u32 => f64;
    f64, u64 => f64;
    f64, u128 => f64;
    f64, usize => f64;
    f64, i8 => f64;
    f64, i16 => f64;
    f64, i32 => f64;
    f64, i64 => f64;
    f64, i128 => f64;
    f64, isize => f64;
    f64, f32 => f64;
    f64, f64 => f64;
);

#[cfg(feature = "f128")]
define_cast_rule!(
    u8, f128 => f128;
    i8, f128 => f128;
    u16, f128 => f128;
    i16, f128 => f128;
    u32, f128 => f128;
    i32, f128 => f128;
    u64, f128 => f128;
    i64, f128 => f128;
    u128, f128 => f128;
    i128, f128 => f128;
    usize, f128 => f128;
    isize, f128 => f128;
    f128, u8 => f128;
    f128, i8 => f128;
    f128, u16 => f128;
    f128, i16 => f128;
    f128, u32 => f128;
    f128, i32 => f128;
    f128, u64 => f128;
    f128, i64 => f128;
    f128, u128 => f128;
    f128, i128 => f128;
    f128, usize => f128;
    f128, isize => f128;
    f128, f32 => f128;
    f128, f64 => f128;
    f128, f128 => f128;
);