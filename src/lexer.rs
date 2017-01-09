use std::str::Chars;
use std::iter::Peekable;
use std::error;
use std::fmt;
use std::num;

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Integer(u32),
    Plus,
    Minus,
    Eof,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    Syntax,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Syntax => write!(f, "Syntax Error"),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::Syntax => "syntax error",
        }
    }
    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::Syntax => None
        }
    }
}

pub struct Lexer<'a>
{
    chars: Peekable<Chars<'a>>,
}

impl From<num::ParseIntError> for Error {
    fn from(_: num::ParseIntError) -> Error {
        Error::Syntax
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer { chars: source.chars().peekable() }
    }
    pub fn next_token(&mut self) -> Result<Token, Error> {
        loop {
            return match self.chars.next() {
                Some(c) if c.is_digit(10) => {
                    let mut digits = vec![c];
                    while let Some(&d) = self.chars.peek() {
                        if d.is_digit(10) {
                            digits.push(d);
                            self.chars.next();
                        } else {
                            break
                        }
                    }
                    let i = try!(digits.iter().cloned().collect::<String>().parse());
                    Ok(Token::Integer(i))
                },
                Some(c) if c == '+' => Ok(Token::Plus),
                Some(c) if c == '-' => Ok(Token::Minus),
                Some(c) if c.is_whitespace() => continue,
                None => Ok(Token::Eof),
                _ => Err(Error::Syntax),
            };
        }
    }
}

#[test]
fn test_lexer() {
    let source = "33+2-";
    let mut lexer = Lexer::new(source);
    assert_eq!(lexer.next_token(), Ok(Token::Integer(33)));
    assert_eq!(lexer.next_token(), Ok(Token::Plus));
    assert_eq!(lexer.next_token(), Ok(Token::Integer(2)));
    assert_eq!(lexer.next_token(), Ok(Token::Minus));
    assert_eq!(lexer.next_token(), Ok(Token::Eof));
}
