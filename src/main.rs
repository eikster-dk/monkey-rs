use std::{io, io::BufReader};

use monkey_rs::repl;

fn main() {
    let reader = BufReader::new(io::stdin());
    repl::start(reader, io::stdout()).unwrap();
}
