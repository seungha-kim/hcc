use std::str::Chars;
use std::iter::Peekable;
use std::error;
use std::fmt;
use std::num;

type Pos = usize;

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Integer(i32),
    Ident(String),
    OpenParen,
    CloseParen,
    Plus,
    Minus,
    Eof,
    Ws,
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
            Error::Syntax => None,
        }
    }
}

pub struct Lexer {
    chars: Vec<char>,
    pos: usize,
}

impl From<num::ParseIntError> for Error {
    fn from(_: num::ParseIntError) -> Error {
        Error::Syntax
    }
}

macro_rules! extract_in_sequence {
    ( $s:ident , $( $i:path ) , + ) => {{
        $(
            #[cfg(debug_assertions)] println!("{} : try {}", $s.pos, stringify!($i));
            match $i(&mut $s.chars, $s.pos) {
                Ok((t, pos)) => {
                    $s.pos = pos;
                    return Ok(t)
                }
                Err(ExtractorError::NoMoreChars) => return Ok(Token::Eof),
                Err(ExtractorError::NotMatched) => (),
                e => panic!("{:?}", e),
            }
        )*
        return Err(Error::Syntax)
    }};
    ( $s:ident , $( $i:path , ) + ) => (extract_in_sequence!($s, $($i),+));
}

impl Lexer {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            chars: source.chars().collect(),
            pos: 0,
        }
    }
    pub fn next_token(&mut self) -> Result<Token, Error> {
        loop {
            match self.extract() {
                Ok(Token::Ws) => (),
                r => return r,
            }
        }
    }
    fn extract(&mut self) -> Result<Token, Error> {
        extract_in_sequence!(self,
            Extractor::ident,
            Extractor::integer,
            Extractor::plus,
            Extractor::minus,
            Extractor::open_paren,
            Extractor::close_paren,
            Extractor::ws,
        )
    }
}

#[test]
fn test_lexer() {
    let source = "(33) + (2 -";
    let mut lexer = Lexer::new(source);
    assert_eq!(lexer.next_token(), Ok(Token::OpenParen));
    assert_eq!(lexer.next_token(), Ok(Token::Integer(33)));
    assert_eq!(lexer.next_token(), Ok(Token::CloseParen));
    assert_eq!(lexer.next_token(), Ok(Token::Plus));
    assert_eq!(lexer.next_token(), Ok(Token::OpenParen));
    assert_eq!(lexer.next_token(), Ok(Token::Integer(2)));
    assert_eq!(lexer.next_token(), Ok(Token::Minus));
    assert_eq!(lexer.next_token(), Ok(Token::Eof));
}

struct Extractor;

#[derive(Eq, PartialEq, Debug)]
enum ExtractorError {
    NotMatched,
    NoMoreChars,
    ParseInt,
}

impl From<num::ParseIntError> for ExtractorError {
    fn from(_: num::ParseIntError) -> ExtractorError {
        ExtractorError::ParseInt
    }
}

impl Extractor {
    fn integer(chars: &Vec<char>, pos: Pos) -> Result<(Token, Pos), ExtractorError> {
        let c = *try!(chars.get(pos).ok_or(ExtractorError::NoMoreChars));
        if c.is_digit(10) {
            let mut step = pos;
            loop {
                match chars.get(step) {
                    Some(&c) if c.is_digit(10) => {
                        step += 1;
                    }
                    _ => break,
                }
            }
            let i = try!(chars[pos..step].iter().cloned().collect::<String>().parse());
            Ok((Token::Integer(i), step))
        } else {
            Err(ExtractorError::NotMatched)
        }
    }
    fn ident(chars: &Vec<char>, pos: Pos) -> Result<(Token, Pos), ExtractorError> {
        let c = *try!(chars.get(pos).ok_or(ExtractorError::NoMoreChars));
        if c.is_alphabetic() {
            let mut step = pos;
            loop {
                match chars.get(step) {
                    Some(&c) if c.is_alphanumeric() => {
                        step += 1;
                    }
                    _ => break,
                }
            }
            let s = chars[pos..step].iter().cloned().collect::<String>();
            Ok((Token::Ident(s), step))
        } else {
            Err(ExtractorError::NotMatched)
        }
    }
    fn plus(chars: &Vec<char>, pos: Pos) -> Result<(Token, Pos), ExtractorError> {
        match chars.get(pos) {
            None => Err(ExtractorError::NoMoreChars),
            Some(&c) if c == '+' => Ok((Token::Plus, pos + 1)),
            _ => Err(ExtractorError::NotMatched),
        }
    }
    fn minus(chars: &Vec<char>, pos: Pos) -> Result<(Token, Pos), ExtractorError> {
        match chars.get(pos) {
            None => Err(ExtractorError::NoMoreChars),
            Some(&c) if c == '-' => Ok((Token::Minus, pos + 1)),
            _ => Err(ExtractorError::NotMatched),
        }
    }
    fn open_paren(chars: &Vec<char>, pos: Pos) -> Result<(Token, Pos), ExtractorError> {
        match chars.get(pos) {
            None => Err(ExtractorError::NoMoreChars),
            Some(&c) if c == '(' => Ok((Token::OpenParen, pos + 1)),
            _ => Err(ExtractorError::NotMatched),
        }
    }
    fn close_paren(chars: &Vec<char>, pos: Pos) -> Result<(Token, Pos), ExtractorError> {
        match chars.get(pos) {
            None => Err(ExtractorError::NoMoreChars),
            Some(&c) if c == ')' => Ok((Token::CloseParen, pos + 1)),
            _ => Err(ExtractorError::NotMatched),
        }
    }
    fn ws(chars: &Vec<char>, pos: Pos) -> Result<(Token, Pos), ExtractorError> {
        let c = *try!(chars.get(pos).ok_or(ExtractorError::NoMoreChars));
        if c.is_whitespace() {
            let mut step = pos;
            loop {
                match chars.get(step) {
                    Some(&c) if c.is_whitespace() => {
                        step += 1;
                    }
                    _ => break,
                }
            }
            Ok((Token::Ws, step))
        } else {
            Err(ExtractorError::NotMatched)
        }
    }
}

#[test]
fn test_extractor_integer() {
    let chars = "1".chars().collect();
    assert_eq!(Ok((Token::Integer(1), 1)), Extractor::integer(&chars, 0));
    let chars = "123".chars().collect();
    assert_eq!(Ok((Token::Integer(123), 3)), Extractor::integer(&chars, 0));
    let chars = "a".chars().collect();
    assert_eq!(Err(ExtractorError::NotMatched),
               Extractor::integer(&chars, 0));
    let chars = "".chars().collect();
    assert_eq!(Err(ExtractorError::NoMoreChars),
               Extractor::integer(&chars, 0));
}

#[test]
fn test_extractor_ident() {
    let chars = "a".chars().collect();
    assert_eq!(Ok((Token::Ident("a".into()), 1)),
               Extractor::ident(&chars, 0));
    let chars = "abc".chars().collect();
    assert_eq!(Ok((Token::Ident("abc".into()), 3)),
               Extractor::ident(&chars, 0));
    let chars = "한글123후후aa".chars().collect();
    assert_eq!(Ok((Token::Ident("한글123후후aa".into()), 9)),
               Extractor::ident(&chars, 0));
    let chars = "1".chars().collect();
    assert_eq!(Err(ExtractorError::NotMatched), Extractor::ident(&chars, 0));
    let chars = "1한글후후".chars().collect();
    assert_eq!(Err(ExtractorError::NotMatched), Extractor::ident(&chars, 0));
}

#[test]
fn test_extractor_plus() {
    let chars = "".chars().collect();
    assert_eq!(Err(ExtractorError::NoMoreChars), Extractor::plus(&chars, 0));
    let chars = "+".chars().collect();
    assert_eq!(Ok((Token::Plus, 1)), Extractor::plus(&chars, 0));
    let chars = "1".chars().collect();
    assert_eq!(Err(ExtractorError::NotMatched), Extractor::plus(&chars, 0));
}

#[test]
fn test_extractor_mius() {
    let chars = "".chars().collect();
    assert_eq!(Err(ExtractorError::NoMoreChars),
               Extractor::minus(&chars, 0));
    let chars = "-".chars().collect();
    assert_eq!(Ok((Token::Minus, 1)), Extractor::minus(&chars, 0));
    let chars = "1".chars().collect();
    assert_eq!(Err(ExtractorError::NotMatched), Extractor::minus(&chars, 0));
}

#[test]
fn test_extractor_open_paren() {
    let chars = "".chars().collect();
    assert_eq!(Err(ExtractorError::NoMoreChars),
               Extractor::open_paren(&chars, 0));
    let chars = "(".chars().collect();
    assert_eq!(Ok((Token::OpenParen, 1)), Extractor::open_paren(&chars, 0));
    let chars = "1".chars().collect();
    assert_eq!(Err(ExtractorError::NotMatched),
               Extractor::open_paren(&chars, 0));
}

#[test]
fn test_extractor_close_paren() {
    let chars = "".chars().collect();
    assert_eq!(Err(ExtractorError::NoMoreChars),
               Extractor::close_paren(&chars, 0));
    let chars = ")".chars().collect();
    assert_eq!(Ok((Token::CloseParen, 1)),
               Extractor::close_paren(&chars, 0));
    let chars = "1".chars().collect();
    assert_eq!(Err(ExtractorError::NotMatched),
               Extractor::close_paren(&chars, 0));
}
