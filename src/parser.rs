use crate::tokenizer::{Lexer, LexingError, Token, TokenKind};
use logos::Span;

#[derive(Debug)]
pub struct ParseLexer<'a> {
    lexer: Lexer<'a>,
    peeked: Option<Result<Token, LexingError>>,
}

impl<'a> ParseLexer<'a> {
    pub fn new(lex: Lexer<'a>) -> Self {
        ParseLexer {
            lexer: lex,
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token, LexingError>> {
        if let Some(ref x) = self.peeked {
            return Some(x);
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

#[derive(Debug, Clone, PartialEq)]
pub enum Ast {
    Expression(ExpressionAst),
    Literal(LiteralAst),
}

impl Ast {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        Ok(Ast::Expression(ExpressionAst::from_lexer(lex)?))
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ExpressionAst {
    pub function: String,
    pub args: Vec<Ast>,
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
                Some(Ok(x)) if [TokenKind::Integer].contains(&x.kind()) => {
                    let literal = LiteralAst::from_lexer(lex)?;
                    if !in_prefix {
                        if expr.args.len() < 1 {
                            in_infix = true;
                        }
                    }
                    expr.args.push(Ast::Literal(literal.clone()));
                }
                Some(Ok(x)) if x.kind() == TokenKind::InfixOperator && in_infix => {
                    set_function(&mut expr, x);
                    lex.next();
                    in_infix = false;
                }
                Some(Ok(x)) if x.kind() == TokenKind::InfixOperator => {
                    let first_arg = Ast::Expression(expr);
                    expr = ExpressionAst::default();
                    set_function(&mut expr, x);
                    expr.args.push(first_arg);
                    lex.next();
                }
                None if !expr.function.is_empty() && expr.args.len() == 2 => {
                    break;
                }
                x => {
                    dbg!(x, expr);
                    unimplemented!()
                }
            };
        }

        Ok(expr)
    }
}

fn set_function(expr: &mut ExpressionAst, token: &Token) {
    match token {
        Token::Add => expr.function = "add".to_string(),
        Token::Subtract => expr.function = "subtract".to_string(),
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
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

// 1 + 2 * 4
