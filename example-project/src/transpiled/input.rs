// Converted with f2rs v0.1.0
// From: ..\..\..\fortran\input.f90
use f2rs_adapter::prelude::*;
f2rs_version!(0_1_0, requires adapter >= 0_1_0);

// comment
// program "my_program"
#[fortran_function]
pub unsafe fn my_program_main() {
    //implicit none
    //use some_module, ONLY : a, b, c
    //use some_module2
    use iso_c_binding::{c_int, c_int as ci};
    use some_module::*;
    // implicit none
    fortran_body!();

    //1 format (i2, 1x, f4.2)
    // TODO format statement

    let mut i: integer = 0;
    let mut a: integer = 0;
    let mut b: integer = 0;
    let mut c: integer = 0;
    let mut d: integer = 0;
    let mut e: integer = 0;
    let mut f: integer = 0;
    let mut flag1: logical = false;
    let mut flag2: logical = false;
    let mut df: double_precision = 0.0;
    let mut cc: integer<c_int> = 0;
    let mut cci: integer<ci> = 0;
    let mut c2: integer<2> = 0;
    let mut c22: integer<2> = 0;
    static mut c3: integer<4> = 42;
    let mut z: double_complex = Default::default();
    #[allow(non_upper_case_globals)]
    const pi: double_precision = 3.14;
    #[allow(non_upper_case_globals)]
    const dkind: integer = fortran!(KIND(1.0d0));
    let mut r: real<dkind> = 0.0;
    //integer :: some_vec(:, 2, 3), some_vec2(2, 3)
    //type(integer) iii
    //character*4 :: str
    //integer dsnjf(1:10)

    assign!(cci, minus!(2));

    add!(sub!(add!(a, mul!(b, c)), div!(d, pow!(e, f))), f);

    for i in 1..=10 {
        if (eq!(i, 5)) {
            fortran!(print *, "i is 5");
        } else {
            fortran!(print *, "i is not 5");
        }
    }

    for k in 1..=5 {
        assign!(a, add!(a, mul!(call!(kmul(i, k)), call!(arg(i)))));
    }

    for q in 1..=5 {
        assign!(a, add!(a, mul!(call!(kmul(i, k)), call!(arg(i)))));
    }

    for l in 1..=5 {
        assign!(a, add!(a, mul!(call!(kmul(i, k)), call!(arg(i)))));
        unclassified_line!(r###"END DO"###);

        call!(some_subroutine(i));

        // comment 2
        unclassified_line!(r###"end program my_program"###);
    }
}
