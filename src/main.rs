#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

use std::env;
use std::fs;
use std::time::Instant;

use cubiml_demo::State;

fn main() {
    let mut state = State::new();
    for fname in env::args().skip(1) {
        println!("Processing {}", fname);
        let data = fs::read_to_string(fname).unwrap();
        // println!(">> {}", data);

        let t0 = Instant::now();
        let res = if state.process(&data) {
            state.get_output().unwrap()
        } else {
            state.get_err().unwrap()
        };
        dbg!(t0.elapsed());

        println!("{}", res);
    }
}
