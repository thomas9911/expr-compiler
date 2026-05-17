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
    InvalidString(String),
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

fn parse_bigint_literal(lex: &mut Lexer<'_>) -> String {
    let slice = lex.slice();
    slice[..slice.len() - 1].to_string()
}

fn parse_string_literal(lex: &mut Lexer<'_>) -> Result<String, LexingError> {
    let slice = lex.slice();
    let inner = &slice[1..slice.len() - 1];
    let mut out = String::with_capacity(inner.len());
    let mut chars = inner.chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }
        let escaped = chars.next().ok_or_else(|| LexingError {
            error: LexingErrorKind::InvalidString("unterminated escape".to_string()),
            span: lex.span(),
        })?;
        match escaped {
            '"' => out.push('"'),
            '\\' => out.push('\\'),
            'n' => out.push('\n'),
            'r' => out.push('\r'),
            't' => out.push('\t'),
            other => {
                return Err(LexingError {
                    error: LexingErrorKind::InvalidString(format!("unsupported escape: \\{other}")),
                    span: lex.span(),
                });
            }
        }
    }
    Ok(out)
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
    #[regex(r"[0-9]+n", parse_bigint_literal)]
    BigIntLiteral(String),
    #[regex(r#""([^"\\]|\\["\\nrt])*""#, parse_string_literal)]
    StringLiteral(String),
    #[regex(r"[0-9]+", parse_integer)]
    Integer(i64),
    #[token("+")]
    Add,
    #[token("->")]
    Arrow,
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
    #[token("[")]
    OpenSquareBracket,
    #[token("]")]
    CloseSquareBracket,
    #[token(",")]
    Comma,
    #[token("=")]
    Assign,
    #[token("fn")]
    DefineFunction,
    #[token("if")]
    If,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("elif")]
    Elif,
    #[token("else")]
    Else,
    #[regex(r"#[^\r\n]*", logos::skip, allow_greedy = true)]
    Comment,
    #[token("do")]
    DoBlock,
    #[token(":")]
    ColonBlock,
    #[token("end")]
    EndBlock,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lexer| lexer.slice().to_string())]
    Symbol(String),
    #[regex(" ", logos::skip, priority = 3)]
    Ignored,
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        match self {
            Token::Indent => TokenKind::Space,
            Token::Newline => TokenKind::Newline,
            Token::BigIntLiteral(_) => TokenKind::Integer,
            Token::StringLiteral(_) => TokenKind::Integer,
            Token::Integer(_) => TokenKind::Integer,
            Token::Add => TokenKind::InfixOperator,
            Token::Arrow => TokenKind::Arrow,
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
            Token::OpenSquareBracket => TokenKind::OpenSquareBracket,
            Token::CloseSquareBracket => TokenKind::CloseSquareBracket,
            Token::Comma => TokenKind::Comma,
            Token::Assign => TokenKind::Assign,
            Token::DefineFunction => TokenKind::DefineFunction,
            Token::If => TokenKind::If,
            Token::And | Token::Or => TokenKind::InfixOperator,
            Token::Elif => TokenKind::Else,
            Token::Else => TokenKind::Else,
            Token::Comment => unreachable!(),
            Token::DoBlock | Token::ColonBlock => TokenKind::StartBlock,
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
    Arrow,
    OpenBracket,
    CloseBracket,
    OpenSquareBracket,
    CloseSquareBracket,
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
        DoBlock,
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
        ColonBlock,
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
fn tokenize_lambda_arrow() {
    use Token::*;

    let result: Result<Vec<_>, _> = Token::lexer("fn item -> item * 2 end").collect();
    assert_eq!(
        result.unwrap(),
        vec![
            DefineFunction,
            Symbol("item".to_string()),
            Arrow,
            Symbol("item".to_string()),
            Multiply,
            Integer(2),
            EndBlock,
        ]
    );
}

#[test]
fn tokenize_bigint_literal() {
    use Token::*;

    let result: Result<Vec<_>, _> = Token::lexer("123n + 4").collect();
    assert_eq!(
        result.unwrap(),
        vec![BigIntLiteral("123".to_string()), Add, Integer(4)]
    );
}

#[test]
fn tokenize_string_literal() {
    use Token::*;

    let result: Result<Vec<_>, _> = Token::lexer("\"hello\\nworld\"").collect();
    assert_eq!(
        result.unwrap(),
        vec![StringLiteral("hello\nworld".to_string())]
    );
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
            DoBlock,
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
            ColonBlock,
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
            DoBlock,
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
fn tokenize_logical_keywords() {
    use Token::*;

    let result: Result<Vec<_>, _> = Token::lexer("a and b or c").collect();
    assert_eq!(
        result.unwrap(),
        vec![
            Symbol("a".to_string()),
            And,
            Symbol("b".to_string()),
            Or,
            Symbol("c".to_string()),
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
fn tokenize_identifier_with_digits() {
    use Token::*;

    let result: Result<Vec<_>, _> = Token::lexer("utf8_width x1 _tmp2").collect();
    assert_eq!(
        result.unwrap(),
        vec![
            Symbol("utf8_width".to_string()),
            Symbol("x1".to_string()),
            Symbol("_tmp2".to_string()),
        ]
    );
}

#[test]
fn tokenize_list_literal() {
    use Token::*;

    let result: Result<Vec<_>, _> = Token::lexer("[1, 2, 3]").collect();
    assert_eq!(
        result.unwrap(),
        vec![
            OpenSquareBracket,
            Integer(1),
            Comma,
            Integer(2),
            Comma,
            Integer(3),
            CloseSquareBracket,
        ]
    );
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
            ColonBlock,
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
            ColonBlock,
            Newline,
            Indent,
            Symbol("x".to_string()),
            Assign,
            Integer(1),
            Newline,
        ]
    );
}
