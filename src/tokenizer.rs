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
    #[token(",")]
    Comma,
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
            Token::Comma => TokenKind::Comma,
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
    Comma,
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

#[test]
fn tokenize_function_with_params() {
    use Token::*;

    let text = "fn add(x, y) do\n    x + y\nend";
    let result: Result<Vec<_>, _> = Token::lexer(text).collect();

    assert_eq!(
        result.unwrap(),
        vec![
            DefineFunction,
            Symbol("add".to_string()),
            OpenBracket,
            Symbol("x".to_string()),
            Comma,
            Symbol("y".to_string()),
            CloseBracket,
            StartBlock,
            Newline,
            Indent,
            Symbol("x".to_string()),
            Add,
            Symbol("y".to_string()),
            Newline,
            EndBlock,
        ]
    );

    let text = r#"

fn add(x, y):
    x + y

    "#;
    let result: Result<Vec<_>, _> = Token::lexer(text).collect();

    assert_eq!(
        result.unwrap(),
        vec![
            Newline,
            DefineFunction,
            Symbol("add".to_string()),
            OpenBracket,
            Symbol("x".to_string()),
            Comma,
            Symbol("y".to_string()),
            CloseBracket,
            StartBlock,
            Newline,
            Indent,
            Symbol("x".to_string()),
            Add,
            Symbol("y".to_string()),
            Newline,
            Indent,
        ]
    );
}

#[test]
fn tokenize_comma() {
    use Token::*;

    let result: Result<Vec<_>, _> = Token::lexer("a, b, c").collect();
    assert_eq!(
        result.unwrap(),
        vec![
            Symbol("a".to_string()),
            Comma,
            Symbol("b".to_string()),
            Comma,
            Symbol("c".to_string()),
        ]
    );
}
