use std::str::Chars;
use std::error;
use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Integer(u32),
    Plus,
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
    source_chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(chars: Chars<'a>) -> Lexer<'a> {
        Lexer { source_chars: chars }
    }
    pub fn next_token(&mut self) -> Result<Token, Error> {
        let taken = self.source_chars.next();
        match taken {
            Some(c) if c.is_digit(10) => Ok(Token::Integer(c.to_digit(10).unwrap())),
            Some(c) if c == '+' => Ok(Token::Plus),
            None => Ok(Token::Eof),
            _ => Err(Error::Syntax),
        }
    }
}

#[test]
fn test_lexer() {
    let source = "3+2";
    let mut lexer = Lexer {source_chars: source.chars()};
    assert_eq!(lexer.next_token(), Ok(Token::Integer(3)));
    assert_eq!(lexer.next_token(), Ok(Token::Plus));
    assert_eq!(lexer.next_token(), Ok(Token::Integer(2)));
    assert_eq!(lexer.next_token(), Ok(Token::Eof));
}
