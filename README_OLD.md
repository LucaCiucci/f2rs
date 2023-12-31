# f2rs
 Fortran to Rust transpiler

Welcome to f2rs, a Fortran to Rust transpiler. This tool is designed to help developers transition their Fortran codebase to Rust, a modern, safe, and high-performance programming language. By automatically converting Fortran code to Rust, f2rs aims to reduce the time and effort required for code migration, while preserving the original logic and structure of the programs.

Whether you're maintaining a legacy Fortran system or looking to leverage the benefits of Rust, f2rs is here to facilitate your journey. In this README, we will guide you through the usage of f2rs, showcasing an example of how Fortran code is transpiled to Rust.

Please note that while f2rs strives to provide accurate transpilation, manual review and adjustments may be necessary to ensure optimal performance and idiomatic Rust code.

## Example
The following Fortran code:
```fortran
program example_goto
    implicit none
    integer :: i

    ! initialize i
    i = 0

    ! increment i and print it
    10 i = i + 1
    print *, i

    if (i < 10) goto 10 ! if i < 10, repeat
end
```

is transpiled to the following Rust code:

```rust
use f2rs_adapter::prelude::*;

// program "example_goto"
#[rewrite_fortran_goto]
pub fn example_goto_main() {
    // implicit none
    let mut i: integer = 0;
    fortran_body!();

    // initialize i
    assign!(i, 0);

    // increment i and print it
    fortran_label!(10);
    assign!(i, add!(i, 1));
    fortran!(print *, i);

    if lt!(i, 10) {
        fortran_goto!(10); // if i < 10, repeat
    }
}
```

using the command:
```sh
cargo run transpile goto.f90 example-project/src/pp.rs; rustfmt example-project/src/pp.rs; cargo fix -p example-project --allow-dirty
```

## Goals

1. **Automated Conversion**: The primary goal of `f2rs` is to automate the conversion of Fortran code to Rust, reducing the manual effort required in code migration.
1. **Preservation of Logic**: `f2rs` aims to preserve the original logic and structure of the Fortran code in the generated Rust code.
1. **Ease of Use**
1. **Support for Common Fortran Constructs**: While it's impossible to cover all Fortran features, `f2rs` strives to support the most commonly used constructs.

### Non-Goals

1. **100% Coverage of Fortran Features**: Due to the complexity and age of the Fortran language, it's not feasible for `f2rs` to support all Fortran features.
1. **Optimized Rust Code**: While `f2rs` aims to generate valid Rust code, it does not guarantee that the generated code will be optimized for performance. Manual optimization may be necessary.
1. **Idiomatic Rust Code**: `f2rs` focuses on preserving the original Fortran logic, which may result in Rust code that is not idiomatic. Manual adjustments may be required to make the code more idiomatic.
1. **Replacement for Manual Review**: `f2rs` is a tool to aid in code migration, but it is not a replacement for manual review. It's important to review and test the generated Rust code to ensure correctness.

## Development

Development is quite difficult since the syntax is already messed up and unnecessarily complicated and standard mixes syntax rules with semantic.

https://docs.nvidia.com/hpc-sdk/pgi-compilers/18.1/pdf/pgi18fortref.pdf
https://j3-fortran.org/doc/year/18/18-007r1.pdf