// Converted with f2rs v0.1.0
// From: ..\..\..\fortran\goto.f90
use f2rs_adapter::prelude::*;
f2rs_version!(0_1_0, requires adapter >= 0_1_0);

// program "example_goto"
#[fortran_function]
pub unsafe fn example_goto_main() {
    // implicit none
    let mut i: integer = 0;
    fortran_body!();

    // initialize i
    assign!(i, 0);

    // increment i and print it
    fortran_label!(10);
    assign!(i, add!(i, 1));
    fortran!(print *, i);

    if (lt!(i, 10)) {
        fortran_goto!(10); // if i < 10, repeat
    }
}
