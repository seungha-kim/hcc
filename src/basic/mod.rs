use std::error;
use std::fmt;
use lexer::{self, Token};

pub struct Interpreter<'a> {
    source: &'a str,
}

enum Operator {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    Syntax(lexer::Error),
    Semantic,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Syntax(_) => write!(f, "Syntax Error"),
            Error::Semantic => write!(f, "Semantic Error"),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        "interpreter error"
    }
    fn cause(&self) -> Option<&error::Error> {
        None
    }
}

impl From<lexer::Error> for Error {
    fn from(err: lexer::Error) -> Error {
        Error::Syntax(err)
    }
}

impl<'a> Interpreter<'a> {
    pub fn new(source: &'a str) -> Self {
        Interpreter { source: source }
    }
    pub fn expr(&self) -> Result<u32, Error> {
        let mut l = lexer::Lexer::new(self.source.chars());
        let mut operand_stack = Vec::new();
        let mut operator_stack = Vec::new();
        loop {
            let token = try!(l.next_token());
            match token {
                Token::Integer(i) => operand_stack.push(i),
                Token::Plus => operator_stack.push(Operator::Plus),
                Token::Minus => operator_stack.push(Operator::Minus),
                Token::Eof => break,
            }
        }
        loop {
            let op = operator_stack.pop();
            match op {
                Some(Operator::Plus) => {
                    let right = try!(operand_stack.pop().ok_or(Error::Semantic));
                    let left = try!(operand_stack.pop().ok_or(Error::Semantic));
                    operand_stack.push(left + right);
                },
                Some(Operator::Minus) => {
                    let right = try!(operand_stack.pop().ok_or(Error::Semantic));
                    let left = try!(operand_stack.pop().ok_or(Error::Semantic));
                    operand_stack.push(left - right);
                },
                None => break,
            }
        }
        if operand_stack.len() == 1 && operator_stack.len() == 0 {
            Ok(operand_stack[0])
        } else {
            Err(Error::Semantic)
        }
    }
}

#[test]
fn test_interpreter_works() {
    let ip = Interpreter::new("3+2");
    assert_eq!(ip.expr().unwrap(), 5);

    let ip = Interpreter::new("3-2");
    assert_eq!(ip.expr().unwrap(), 1);

    // let ip = Interpreter::new("3-4");
    // assert_eq!(ip.expr().unwrap(), -1);
}

#[test]
fn test_interpreter_two_digit() {
    let ip = Interpreter::new("32");
    assert_eq!(ip.expr(), Err(Error::Semantic));
}

#[test]
fn test_interpreter_two_plus() {
    let ip = Interpreter::new("3++");
    assert_eq!(ip.expr(), Err(Error::Semantic));
}
