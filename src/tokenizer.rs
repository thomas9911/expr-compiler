pub use logos::Logos;
use logos::Span;
use std::num::IntErrorKind;

pub type Lexer<'a> = logos::Lexer<'a, Token>;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct LexingError {
    error: LexingErrorKind,
    span: Span,
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexingErrorKind {
    InvalidInteger(String),
    UnknownCharacter(char),
    #[default]
    Other,
}

fn parse_integer(lex: &mut Lexer<'_>) -> Result<i64, LexingError> {
    match lex.slice().parse() {
        Ok(x) => Ok(x),
        Err(err) => {
            let kind = match err.kind() {
                IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                    LexingErrorKind::InvalidInteger("overflow error".to_owned())
                }
                _ => LexingErrorKind::InvalidInteger("other error".to_owned()),
            };
            Err(LexingError {
                error: kind,
                span: lex.span(),
            })
        }
    }
}

impl LexingError {
    fn from_lexer(lex: &mut Lexer<'_>) -> Self {
        LexingError {
            error: LexingErrorKind::UnknownCharacter(lex.slice().chars().next().unwrap()),
            span: lex.span(),
        }
    }
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error(LexingError, LexingError::from_lexer))]
// #[logos(skip r" ")]
pub enum Token {
    #[token("    ")]
    Indent,
    #[regex(r"[\n]+")]
    Newline,
    #[regex(r"[0-9]+", parse_integer)]
    Integer(i64),
    #[token("+")]
    Add,
    #[token("-")]
    Subtract,
    #[token("(")]
    OpenBracket,
    #[token(")")]
    CloseBracket,
    #[token("fn")]
    DefineFunction,
    #[regex("do|:", priority = 5)]
    StartBlock,
    #[token("end")]
    EndBlock,
    #[regex(r"[a-zA-Z_]+", |lexer| lexer.slice().to_string())]
    Symbol(String),
    #[regex(" ", logos::skip, priority = 3)]
    Ignored,
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        match self {
            Token::Indent => TokenKind::Space,
            Token::Newline => TokenKind::Newline,
            Token::Integer(_) => TokenKind::Integer,
            Token::Add => TokenKind::InfixOperator,
            Token::Subtract => TokenKind::InfixOperator,
            Token::OpenBracket => TokenKind::OpenBracket,
            Token::CloseBracket => TokenKind::CloseBracket,
            Token::DefineFunction => TokenKind::DefineFunction,
            Token::StartBlock => TokenKind::StartBlock,
            Token::EndBlock => TokenKind::EndBlock,
            Token::Symbol(_) => TokenKind::Symbol,
            Token::Ignored => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Space,
    Newline,
    Integer,
    InfixOperator,
    OpenBracket,
    CloseBracket,
    DefineFunction,
    StartBlock,
    EndBlock,
    Symbol,
}

#[test]
fn tokenize_function() {
    use Token::*;

    let text = r#"

fn main() do
    1 + 2 - 3
end

    "#;

    let result: Result<Vec<_>, _> = Token::lexer(text).collect();

    let expected = vec![
        Newline,
        DefineFunction,
        Symbol("main".to_string()),
        OpenBracket,
        CloseBracket,
        StartBlock,
        Newline,
        Indent,
        Integer(1),
        Add,
        Integer(2),
        Subtract,
        Integer(3),
        Newline,
        EndBlock,
        Newline,
        Indent,
    ];

    assert_eq!(result.unwrap(), expected);

    let text = r#"

fn main():
    1 + 2 - 3

    "#;

    let result: Result<Vec<_>, _> = Token::lexer(text).collect();

    let expected = vec![
        Newline,
        DefineFunction,
        Symbol("main".to_string()),
        OpenBracket,
        CloseBracket,
        StartBlock,
        Newline,
        Indent,
        Integer(1),
        Add,
        Integer(2),
        Subtract,
        Integer(3),
        Newline,
        Indent,
    ];

    assert_eq!(result.unwrap(), expected);

    // todo!("determine if do end (like elixir) or use spaces and : (like python)")
}
