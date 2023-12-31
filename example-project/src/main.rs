
mod transpiled;

// cargo run -- project example-project
// cargo fix -p example-project --allow-dirty
// cargo run -p example-project

const N: usize = 1;

fn main() {
    unsafe {
        transpiled::my_program_main();
        transpiled::example_goto_main();
        transpiled::congruent_main();
    }

    let _b: B<1> = 0;
    let _b: B<N> = 0;
}

struct A<const N: usize>;

trait T {
    type Item;
}

impl T for A<1> {
    type Item = i32;
}

type B<const N: usize> = <A<N> as T>::Item;