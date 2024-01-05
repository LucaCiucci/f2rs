# f2rs
A WIP Fortran to Rust transpiler

`f2rs` will transpile modern Fortran code to human-friendly Rust code and will provide a [`f2rs-adapter`](https://github.com/LucaCiucci/f2rs/tree/main/f2rs-adapter) crate that mimics the Fortran standard library.

At the moment, only `Fortran -> Rust` is planned since this is the most straightforward, but other languages may be supported in the future as the project evolves.

## Status of the project

This project is in a very early stage of development. It is not usable yet.

A proof of concept is available in the [`f2rs`](https://github.com/LucaCiucci/f2rs/tree/main/f2rs) directory and some examples of converted code is available in the [`transpiled`](https://github.com/LucaCiucci/f2rs/tree/main/example-project/src/transpiled) directory.  
This proof of concept is currently stale as a proper parser is being developed in the [`f2rs-parse`](https://github.com/LucaCiucci/f2rs/tree/main/f2rs-parse) crate.

### Implementation progress
- [ ] parsing
  - [ ] syntax (see [#1](https://github.com/LucaCiucci/f2rs/issues/1))
  - [ ] semantics
  - [ ] ...
- [ ] [`f2rs-adapter`](https://github.com/LucaCiucci/f2rs/tree/main/f2rs-adapter) crate
- [ ] code generation