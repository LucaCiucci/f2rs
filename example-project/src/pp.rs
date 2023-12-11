use f2rs_adapter::prelude::*;



// program "example_goto"
#[rewrite_fortran_goto]
pub unsafe fn main() {
    // implicit none
    let mut i: integer = 0;

    fortran_body!();

    // initialize i
    assign!(i, 0);

    // increment i and print it
    fortran_label!(10);
    assign!(i, add!(i, 1));
    fortran!(print *, i);

    if i < 10 {
        fortran_goto!(10); // if i < 10, repeat
    }
}
