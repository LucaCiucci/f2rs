// Converted with f2rs v0.1.0
// From: ..\..\..\fortran\some_module.f90
use f2rs_adapter::prelude::*;
f2rs_version!(0_1_0, requires adapter >= 0_1_0);

pub mod some_module {
    use super::*;
    pub use contains::some_subroutine;
    // contains
    mod contains {
        use super::*;
        #[fortran_function]
        pub unsafe fn some_subroutine(i: &mut integer) {
            // implicit none
            //integer, intent(in) :: i
            let mut i: integer = 0;
            fortran_body!();

            fortran!(print *, "Hello from some_sub", i);
        }

        #[fortran_function]
        pub unsafe fn some_function(x: &mut double_precision) {
            // implicit none
            let mut x: double_precision = 0.0;
            fortran_body!();

            assign!(some_function, pow!(x, 2));
        }
    }
}
