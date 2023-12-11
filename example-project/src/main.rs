use f2rs_adapter::prelude::*;

mod pp;

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

    foo();
    foo2();

    unsafe {
        pp::main();
    }
}

use forward_goto::rewrite_forward_goto;

#[rewrite_forward_goto]
fn foo() {
    //forward_goto!('a);
    forward_goto!('turing_complete);
    println!("Hello, world!");

    if false {
        println!("Seems you were lucky");

        forward_label!('turing_complete);

        println!("Message decoded!");
    } else {
        println!("No luck today...");
    }
}

#[rewrite_fortran_goto]
fn foo2() {
    let p = |i: i32| println!("Hello, world! {}", i);

    let mut counter = 0;

    fortran_body!();

    fortran_label!(10);
    counter += 1;
    p(counter);
    if counter < 10 {
        fortran_goto!(10);
    }
}

/*
fn foo2() {
    enum State {
        Start,
        Label1,
        End,
    }

    impl State {
        fn next(&self) -> Self {
            match self {
                State::Start => State::Label1,
                State::Label1 => State::End,
                State::End => State::Start,
            }
        }
    }

    rewrite_fortran_goto! {
        println!("Hello, world! 1");

        fortran_label!(123);
        fortran_label!(124);

        let mut state = State::Start;

        'outher: loop {
            //macro_rules! _goto_label_1 {
            //    () => {
            //        break 'set_outher_step State::Label1;
            //    };
            //}

            state = 'set_outher_step: {
                match state {
                    State::Start => {
                        // TODO something
                        println!("Hello, world! 1");
                        break 'set_outher_step State::Label1;
                        println!("Hello, world! 2");
                        break 'set_outher_step State::Start.next();
                    }
                    State::Label1 => {
                        // TODO something
                        println!("Hello, world! 3");
                        break 'set_outher_step State::Label1.next();
                    }
                    State::End => {
                        break 'outher;
                    }
                }
            };
        }
    }
}
*/
