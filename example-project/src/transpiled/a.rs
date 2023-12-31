// Converted with f2rs v0.1.0
// From: ..\..\..\fortran\a.f90
use f2rs_adapter::prelude::*;
f2rs_version!(0_1_0, requires adapter >= 0_1_0);

// comment
// program "my_program"
#[fortran_function]
pub unsafe fn my_program_main() {
fortran_body!();

for k in 1..=5 {
assign!(a, add!(a, mul!(call!(kmul(i, k)), call!(arg(i)))));
fortran!(print *, a);
}
}


pub struct my_type {
pub i: integer;
pub arg: TODO;
pub kmul: TODO;
}
