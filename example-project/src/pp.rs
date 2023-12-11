use f2rs_adapter::prelude::*;

//==============================================================
use super::*;
use crate::*;
// program "congruent"
pub unsafe fn main() {
    //==============================================================
    // example of linear congruential generator
    // X_(k+1) = mod(a*X_k + c,m) -> x_k = X_k)/m
    // M. D'Elia - 09/2018
    //==============================================================

    // declaration of variable types

    // implicit none
    let mut m: integer16 = 0 /*WARNING: initialization was missing*/; // we require quite large integers for the generator
    let mut a: integer16 = 0 /*WARNING: initialization was missing*/; // we require quite large integers for the generator
    let mut c: integer16 = 0 /*WARNING: initialization was missing*/; // we require quite large integers for the generator
    let mut xk: integer16 = 0 /*WARNING: initialization was missing*/; // we require quite large integers for the generator
    let mut xkp1: integer16 = 0 /*WARNING: initialization was missing*/; // we require quite large integers for the generator
    let mut xtemp: integer16 = 0 /*WARNING: initialization was missing*/; // we require quite large integers for the generator
    let mut seed: integer16 = 0 /*WARNING: initialization was missing*/; // we require quite large integers for the generator

    let mut iloop: integer = 0 /*WARNING: initialization was missing*/;

    let mut x: real8 = 0.0 /*WARNING: initialization was missing*/;
    let mut y: real8 = 0.0 /*WARNING: initialization was missing*/;

    // definition of the "modulus"
    assign!(m, add!(pow!(10, 8), 1)); // original Lehmer implementation 1949 working on ENIAC
                                      // which was indeed a 8-decimal digit number machine
                                      //      m = 2147483647 ! 2^31 - 1  ! Park-Miller 1988, this is a Mersenne prime

    // definition of the "multiplier"
    assign!(a, 23); // original Lehmer implementation 1949 workin on ENIAC
                    //      a = 16807  ! Park-Miller 1988
                    //      a = 48271  ! Park-Miller 1993

    // definition of the "increment"
    // when c = 0 the generator is called "multiplicative congruential generator"
    assign!(c, 0); // Both Lehmer and Park-Miller implementations

    assign!(seed, 2); // define the starting values of pseudo-random sequence

    assign!(xk, seed);

    for iloop in 1..=10 {
        assign!(xtemp, add!(mul!(xk, a), c)); // the generator in three lines: linear transformation
        assign!(xkp1, sub!(xtemp, mul!(m, (div!(xtemp, m))))); // this is the mod(xtemp,m) operation. Notice that going
        assign!(xk, xkp1); // through xkp1 is useless, could save one line,
                           // just for the sake of clarity ..
        assign!(x, div!(fortran!(float(xk)), fortran!(float(m)))); // x in [0,1), the actual random number

        assign!(xtemp, add!(mul!(xk, a), c)); // we repeat twice to draw a pair of consecutive
        assign!(xkp1, sub!(xtemp, mul!(m, (div!(xtemp, m))))); // random numbers in the sequence
        assign!(xk, xkp1); //
                           //
        assign!(y, div!(fortran!(float(xk)), fortran!(float(m)))); //

        fortran!(print *, x, y);
        // follow some regular structure. Lehmer implementation
        // is ugly, Park-Miller looks much better
        //      if (x.lt.0.001.and.y.lt.0.001) write (*,*) x,y  ! but if you try to zoom
        // also Park-Miller shows regular
        // structures, even if at much smaller
        // scales
    }

    fortran!(stop);
}
