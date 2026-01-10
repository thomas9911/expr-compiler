use logos::Logos;
use logos::Span;
use std::{iter::Peekable, num::IntErrorKind};

type Lexer<'a> = logos::Lexer<'a, Token>;

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

#[derive(Debug)]
pub struct ParseError<'a> {
    span: Span,
    text: &'a str,
}

impl<'a> ParseError<'a> {
    fn unexpected(lex: &mut ParseLexer<'a>) -> ParseError<'a> {
        ParseError {
            span: lex.lexer.span(),
            text: lex.lexer.slice(),
        }
        // ParseError { span, text }
    }
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error(LexingError, LexingError::from_lexer))]
#[logos(skip r"\s")]
pub enum Token {
    #[token(r"\n")]
    Newline,
    #[regex(r"[0-9]+", parse_integer)]
    Integer(i64),
    #[token("+")]
    Add,
    #[token("-")]
    Subtract,
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        match self {
            Token::Newline => TokenKind::Newline,
            Token::Integer(_) => TokenKind::Integer,
            Token::Add => TokenKind::InfixOperator,
            Token::Subtract => TokenKind::InfixOperator,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Newline,
    Integer,
    InfixOperator,
}

// pub struct Ast {}

#[derive(Debug)]
pub struct ParseLexer<'a> {
    lexer: Lexer<'a>,
    peeked: Option<Result<Token, LexingError>>,
}

impl<'a> ParseLexer<'a> {
    fn new(lex: Lexer<'a>) -> Self {
        ParseLexer {
            lexer: lex,
            peeked: None,
        }
    }

    fn peek(&mut self) -> Option<Result<Token, LexingError>> {
        if let Some(ref x) = self.peeked {
            return Some(x.clone());
        }

        let token = self.lexer.next()?;
        self.peeked = Some(token);
        self.peek()
    }
}

impl<'a> Iterator for ParseLexer<'a> {
    type Item = Result<Token, LexingError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(x) = self.peeked.take() {
            return Some(x);
        }

        self.lexer.next()
    }
}

#[derive(Debug, Clone)]
pub enum Ast {
    Expression(ExpressionAst),
    Literal(LiteralAst),
}

impl Ast {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        todo!()
    }
}

#[derive(Debug, Default, Clone)]
pub struct ExpressionAst {
    function: String,
    args: Vec<Ast>,
}

impl ExpressionAst {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        let mut expr = ExpressionAst::default();
        let mut in_prefix = false;
        let mut in_infix = false;
        loop {
            match lex.peek() {
                Some(Ok(Token::Newline)) => break,
                Some(Ok(x)) if [TokenKind::Integer].contains(&x.kind()) && in_infix => {
                    todo!("error")
                }
                Some(Ok(x)) if x.kind() == TokenKind::InfixOperator && in_infix => {
                    match x {
                        Token::Add => {expr.function = "add".to_string()}
                        Token::Subtract => {expr.function = "subtract".to_string()}
                        _ => unreachable!()
                    }
                    lex.next();
                    in_infix = false;
                }
                Some(Ok(x)) if [TokenKind::Integer].contains(&x.kind()) => {
                    let literal = LiteralAst::from_lexer(lex)?;
                    if !in_prefix {
                        in_infix = true;
                    }
                    expr.args.push(Ast::Literal(literal.clone()));
                }
                None if !expr.function.is_empty() && expr.args.len() == 2 => {
                    break;
                }
                x => {
                    dbg!(x);
                    unimplemented!()
                },
            };
        }

        Ok(expr)
    }

}

#[derive(Debug, Clone)]
pub enum LiteralAst {
    Integer(i64),
}

impl LiteralAst {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        match lex.next() {
            Some(Ok(Token::Integer(int))) => Ok(LiteralAst::Integer(int)),
            _ => Err(ParseError::unexpected(lex)),
        }
    }
}

#[test]
fn xd() {
    let lex = Token::lexer("3 + 2");

    let mut lexer = ParseLexer::new(lex);

    dbg!(&ExpressionAst::from_lexer(&mut lexer));

    // let tokens: Result<Vec<_>, _> = lex.collect();

    // dbg!(tokens);

    panic!();
}
