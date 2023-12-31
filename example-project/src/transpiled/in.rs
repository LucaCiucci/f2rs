use f2rs_adapter::prelude::*;

// comment
use super::*;
use crate::*;
// program "my_program"
#[rewrite_fortran_goto]
pub unsafe fn my_program_main() {
    //implicit none
    use some_module::{a, b, c};
    use some_module2::*;
    // implicit none
    fortran_body!();

    //1 format (i2, 1x, f4.2)
    // TODO format statement

    let mut i: integer = 0;

    let mut flag1: logical = false;
    let mut flag2: logical = false;

    let mut d: double_precision = 0.0;

    let mut c: TODO = TODO;

    let mut c2: integer2 = 0;

    let mut c3: integer4 = 0;

    let mut z: double_complex = 0.0;

    let mut some_vec: TODO = TODO;
    let mut some_vec2: TODO = TODO;

    let mut iii: TODO = TODO;

    let mut str: TODO = TODO;

    let mut dsnjf: TODO = TODO;

    -2;

    add!(sub!(add!(a, mul!(b, c)), div!(d, pow!(e, f))), call!(f(2)));

    for i in 1..=10 {
        if (i == 5) {
            fortran!(print *, "i is 5");
        } else {
            fortran!(print *, "i is not 5");
        }
    }

    call!(some_subroutine(a, b, c, 3));

    // comment 2
}
