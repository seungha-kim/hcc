extern crate hcc;

use std::error;
use std::io;
use hcc::basic::Interpreter;
use std::io::Write;

fn main() {
    loop {
        print!("calc> ");
        // https://github.com/rust-lang/rust/issues/23818
        stdout_flush();
        match interprete() {
            Ok(result) => println!("{}", result),
            Err(err) => println!("error: {}", err),
        }
    }
}

fn interprete() -> Result<String, Box<error::Error>> {
    let mut input = String::new();
    try!(io::stdin().read_line(&mut input));
    let result = try!(Interpreter::new(input.trim()).expr());
    Ok(result.to_string())
}

fn stdout_flush() {
    io::stdout().flush().ok().expect("Could not flush stdout");
}
