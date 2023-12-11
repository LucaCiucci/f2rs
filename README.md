# f2rs
 Fortran to Rust transpiler

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
```

using the command:
```sh
cargo run goto.f90 example-project/src/pp.rs; rustfmt example-project/src/pp.rs; cargo fix -p example-project --allow-dirty
```