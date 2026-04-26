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
    #[regex(r"[\r\n]+")]
    Newline,
    #[regex(r"[0-9]+", parse_integer)]
    Integer(i64),
    #[token("+")]
    Add,
    #[token("-")]
    Subtract,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("%")]
    Modulo,
    #[token(">=")]
    GreaterThanOrEqual,
    #[token("<=")]
    LessThanOrEqual,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,
    #[token(">")]
    GreaterThan,
    #[token("<")]
    LessThan,
    #[token("(")]
    OpenBracket,
    #[token(")")]
    CloseBracket,
    #[token(",")]
    Comma,
    #[token("=")]
    Assign,
    #[token("fn")]
    DefineFunction,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[regex(r"#[^\r\n]*", logos::skip, allow_greedy = true)]
    Comment,
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
            Token::Multiply => TokenKind::InfixOperator,
            Token::Divide => TokenKind::InfixOperator,
            Token::Modulo => TokenKind::InfixOperator,
            Token::GreaterThan => TokenKind::InfixOperator,
            Token::LessThan => TokenKind::InfixOperator,
            Token::GreaterThanOrEqual => TokenKind::InfixOperator,
            Token::LessThanOrEqual => TokenKind::InfixOperator,
            Token::EqualEqual => TokenKind::InfixOperator,
            Token::NotEqual => TokenKind::InfixOperator,
            Token::OpenBracket => TokenKind::OpenBracket,
            Token::CloseBracket => TokenKind::CloseBracket,
            Token::Comma => TokenKind::Comma,
            Token::Assign => TokenKind::Assign,
            Token::DefineFunction => TokenKind::DefineFunction,
            Token::If => TokenKind::If,
            Token::Else => TokenKind::Else,
            Token::Comment => unreachable!(),
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
    Assign,
    DefineFunction,
    If,
    Else,
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

    // Both block styles are currently accepted: `do ... end` and `: + indentation`.
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
fn tokenize_function_call() {
    use Token::*;

    let result: Result<Vec<_>, _> = Token::lexer("double(x)").collect();
    assert_eq!(
        result.unwrap(),
        vec![
            Symbol("double".to_string()),
            OpenBracket,
            Symbol("x".to_string()),
            CloseBracket,
        ]
    );

    let result: Result<Vec<_>, _> = Token::lexer("add(x, y)").collect();
    assert_eq!(
        result.unwrap(),
        vec![
            Symbol("add".to_string()),
            OpenBracket,
            Symbol("x".to_string()),
            Comma,
            Symbol("y".to_string()),
            CloseBracket,
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

#[test]
fn tokenize_comparison_operators() {
    use Token::*;

    let result: Result<Vec<_>, _> = Token::lexer("a > b").collect();
    assert_eq!(
        result.unwrap(),
        vec![
            Symbol("a".to_string()),
            GreaterThan,
            Symbol("b".to_string())
        ]
    );

    let result: Result<Vec<_>, _> = Token::lexer("a < b").collect();
    assert_eq!(
        result.unwrap(),
        vec![Symbol("a".to_string()), LessThan, Symbol("b".to_string())]
    );

    let result: Result<Vec<_>, _> = Token::lexer("a >= b").collect();
    assert_eq!(
        result.unwrap(),
        vec![
            Symbol("a".to_string()),
            GreaterThanOrEqual,
            Symbol("b".to_string())
        ]
    );

    let result: Result<Vec<_>, _> = Token::lexer("a <= b").collect();
    assert_eq!(
        result.unwrap(),
        vec![
            Symbol("a".to_string()),
            LessThanOrEqual,
            Symbol("b".to_string())
        ]
    );

    let result: Result<Vec<_>, _> = Token::lexer("a == b").collect();
    assert_eq!(
        result.unwrap(),
        vec![Symbol("a".to_string()), EqualEqual, Symbol("b".to_string())]
    );

    let result: Result<Vec<_>, _> = Token::lexer("a != b").collect();
    assert_eq!(
        result.unwrap(),
        vec![Symbol("a".to_string()), NotEqual, Symbol("b".to_string())]
    );
}

#[test]
fn tokenize_if_else_keywords() {
    use Token::*;

    let result: Result<Vec<_>, _> = Token::lexer("if x > 0 do\n    x\nelse\n    0\nend").collect();
    assert_eq!(
        result.unwrap(),
        vec![
            If,
            Symbol("x".to_string()),
            GreaterThan,
            Integer(0),
            StartBlock,
            Newline,
            Indent,
            Symbol("x".to_string()),
            Newline,
            Else,
            Newline,
            Indent,
            Integer(0),
            Newline,
            EndBlock,
        ]
    );
}

#[test]
fn tokenize_if_not_symbol() {
    use Token::*;

    // 'if' and 'else' must not lex as Symbol
    let result: Result<Vec<_>, _> = Token::lexer("if else").collect();
    assert_eq!(result.unwrap(), vec![If, Else]);
}

#[test]
fn tokenize_skips_line_comments() {
    use Token::*;

    let text = "fn main():\n    # comment only line\n    x = 1 # trailing comment\n    x\n";
    let result: Result<Vec<_>, _> = Token::lexer(text).collect();

    assert_eq!(
        result.unwrap(),
        vec![
            DefineFunction,
            Symbol("main".to_string()),
            OpenBracket,
            CloseBracket,
            StartBlock,
            Newline,
            Indent,
            Newline,
            Indent,
            Symbol("x".to_string()),
            Assign,
            Integer(1),
            Newline,
            Indent,
            Symbol("x".to_string()),
            Newline,
        ]
    );
}

#[test]
fn tokenize_skips_line_comments_crlf() {
    use Token::*;

    let text = "fn main():\r\n    x = 1 # comment\r\n";
    let result: Result<Vec<_>, _> = Token::lexer(text).collect();

    assert_eq!(
        result.unwrap(),
        vec![
            DefineFunction,
            Symbol("main".to_string()),
            OpenBracket,
            CloseBracket,
            StartBlock,
            Newline,
            Indent,
            Symbol("x".to_string()),
            Assign,
            Integer(1),
            Newline,
        ]
    );
}
