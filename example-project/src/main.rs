use f2rs_adapter::prelude::*;

fn main() {
    let _v: integer = 3;
    let _v: real = 3.0;
    let _v = complex::new(3.0, 4.0);
    //let c: CharacterN<5> = "hello";

    fn f(_a: &i32) {}

    let a = 1;
    let b = 2;
    let c = 3;
    let _ = a + (b * c);

    call!(f(3));

    f.call_or_index((&mut 3,));

    println!("Hello, world!");
}
