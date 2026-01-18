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
    Block(BlockAst),
    FunctionDef(FunctionDefAst),
    Expression(ExpressionAst),
    Literal(LiteralAst),
}

fn trim_newlines<'a>(lex: &mut ParseLexer<'a>) {
    while lex.peek() == Some(&Ok(Token::Newline)) {
        lex.next();
    }
}

impl Ast {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        trim_newlines(lex);

        match lex.peek() {
            Some(&Ok(Token::DefineFunction)) => {
                Ok(Ast::FunctionDef(FunctionDefAst::from_lexer(lex)?))
            }
            _ => Ok(Ast::Expression(ExpressionAst::from_lexer(lex)?)),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct BlockAst {
    lines: Vec<Ast>,
}

impl BlockAst {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        let mut block = BlockAst::default();
        let mut current_indent = 0;
        assert!(lex.next() == Some(Ok(Token::StartBlock)));

        trim_newlines(lex);
        loop {
            match dbg!(lex.peek()) {
                Some(Ok(Token::Indent)) => {
                    lex.next();
                    current_indent += 1;
                }
                Some(Ok(Token::EndBlock)) => {
                    lex.next();
                    current_indent = 0;
                    break;
                }
                Some(Ok(Token::Newline)) => {
                    lex.next();
                    current_indent = 0;
                }
                None => {
                    break;
                }
                x => {
                    block.lines.push(Ast::from_lexer(lex)?);
                    current_indent = 0;
                }
            }
        }

        Ok(block)
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct FunctionDefAst {
    name: String,
    inputs: Vec<String>,
    output: Option<String>,
    block: BlockAst,
}

impl FunctionDefAst {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        let mut function_def = FunctionDefAst::default();
        assert!(lex.next() == Some(Ok(Token::DefineFunction)));
        let mut first = true;
        let mut bracket_counter = 0;

        loop {
            match lex.peek() {
                Some(Ok(Token::Symbol(_))) if first => {
                    let Token::Symbol(name) = lex.next().unwrap().unwrap() else {
                        unreachable!()
                    };
                    function_def.name = name;
                    first = false;
                }
                Some(Ok(Token::OpenBracket)) if bracket_counter == 0 => {
                    lex.next();
                    bracket_counter = 1;
                }
                Some(Ok(Token::CloseBracket)) if bracket_counter == 1 => {
                    lex.next();
                    bracket_counter = 0;
                }
                Some(Ok(Token::StartBlock)) => {
                    function_def.block = BlockAst::from_lexer(lex)?;
                }
                None => {
                    break;
                }
                Some(Ok(Token::Newline)) => {
                    break;
                }

                x => {
                    unimplemented!("{:?}", x)
                }
            }
        }

        Ok(function_def)
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

#[cfg(test)]
use crate::tokenizer::{self, Logos};

#[test]
fn parse_test_elixir_style() {
    use Ast::*;

    let text = r#"

fn main() do
    1 + 2 - 3
end

    "#;

    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function: "subtract".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function: "add".to_string(),
                        args: vec![
                            Literal(LiteralAst::Integer(1)),
                            Literal(LiteralAst::Integer(2)),
                        ],
                    }),
                    Literal(LiteralAst::Integer(3)),
                ],
            })],
        },
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_test_python_style() {
    use Ast::*;

    let text = r#"

fn main():
    1 + 2 - 3

    "#;

    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function: "subtract".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function: "add".to_string(),
                        args: vec![
                            Literal(LiteralAst::Integer(1)),
                            Literal(LiteralAst::Integer(2)),
                        ],
                    }),
                    Literal(LiteralAst::Integer(3)),
                ],
            })],
        },
    });

    assert_eq!(ast, expected);
}
