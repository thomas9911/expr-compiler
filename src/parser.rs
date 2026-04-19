use crate::tokenizer::{Lexer, LexingError, Token, TokenKind};
use logos::Span;
use std::collections::VecDeque;

#[derive(Debug)]
pub struct ParseLexer<'a> {
    lexer: Lexer<'a>,
    buf: VecDeque<Result<Token, LexingError>>,
}

impl<'a> ParseLexer<'a> {
    pub fn new(lex: Lexer<'a>) -> Self {
        ParseLexer {
            lexer: lex,
            buf: VecDeque::new(),
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token, LexingError>> {
        if self.buf.is_empty() {
            let token = self.lexer.next()?;
            self.buf.push_back(token);
        }
        self.buf.front()
    }

    pub fn push_back(&mut self, token: Result<Token, LexingError>) {
        self.buf.push_front(token);
    }
}

impl<'a> Iterator for ParseLexer<'a> {
    type Item = Result<Token, LexingError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(tok) = self.buf.pop_front() {
            return Some(tok);
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
    Variable(String),
    Assign {
        name: String,
        value: Box<Ast>,
    },
    If {
        condition: Box<Ast>,
        then: BlockAst,
        else_: Option<BlockAst>,
    },
}

fn trim_newlines<'a>(lex: &mut ParseLexer<'a>) {
    while lex.peek() == Some(&Ok(Token::Newline)) {
        lex.next();
    }
}

fn parse_block_body_until_end<'a>(lex: &mut ParseLexer<'a>) -> Result<BlockAst, ParseError<'a>> {
    let mut block = BlockAst::default();
    loop {
        match lex.peek() {
            Some(Ok(Token::Indent)) | Some(Ok(Token::Newline)) => {
                lex.next();
            }
            Some(Ok(Token::EndBlock)) => {
                lex.next();
                break;
            }
            Some(Ok(Token::DefineFunction)) | None => break,
            _ => block.lines.push(Ast::from_lexer(lex)?),
        }
    }
    Ok(block)
}

impl Ast {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        trim_newlines(lex);

        if matches!(lex.peek(), Some(Ok(Token::Symbol(_)))) {
            let tok = lex.next().unwrap();
            if lex.peek() == Some(&Ok(Token::Assign)) {
                lex.next();
                let Token::Symbol(name) = tok.unwrap() else {
                    unreachable!()
                };
                let value = parse_expr(lex, 0)?;
                return Ok(Ast::Assign {
                    name,
                    value: Box::new(value),
                });
            }
            lex.push_back(tok);
        }

        match lex.peek() {
            Some(&Ok(Token::DefineFunction)) => {
                Ok(Ast::FunctionDef(FunctionDefAst::from_lexer(lex)?))
            }
            Some(&Ok(Token::If)) => {
                lex.next();
                let condition = parse_expr(lex, 0)?;
                let then = BlockAst::from_lexer(lex)?;
                trim_newlines(lex);
                let else_ = if lex.peek() == Some(&Ok(Token::Else)) {
                    lex.next();
                    trim_newlines(lex);
                    if matches!(lex.peek(), Some(Ok(Token::StartBlock))) {
                        Some(BlockAst::from_lexer(lex)?)
                    } else {
                        Some(parse_block_body_until_end(lex)?)
                    }
                } else {
                    None
                };
                Ok(Ast::If {
                    condition: Box::new(condition),
                    then,
                    else_,
                })
            }
            _ => parse_expr(lex, 0),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct BlockAst {
    pub lines: Vec<Ast>,
}

impl BlockAst {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        let mut block = BlockAst::default();
        assert!(lex.next() == Some(Ok(Token::StartBlock)));

        trim_newlines(lex);
        loop {
            match lex.peek() {
                Some(Ok(Token::Indent)) => {
                    lex.next();
                }
                Some(Ok(Token::EndBlock)) => {
                    lex.next();
                    break;
                }
                Some(Ok(Token::Newline)) => {
                    lex.next();
                }
                Some(Ok(Token::DefineFunction)) | Some(Ok(Token::Else)) | None => {
                    break;
                }
                _ => {
                    block.lines.push(Ast::from_lexer(lex)?);
                }
            }
        }

        Ok(block)
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct FunctionDefAst {
    pub name: String,
    pub inputs: Vec<String>,
    pub output: Option<String>,
    pub block: BlockAst,
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
                    loop {
                        match lex.peek() {
                            Some(Ok(Token::Symbol(_))) => {
                                let Token::Symbol(name) = lex.next().unwrap().unwrap() else {
                                    unreachable!()
                                };
                                function_def.inputs.push(name);
                            }
                            Some(Ok(Token::Comma)) => {
                                lex.next();
                            }
                            _ => break,
                        }
                    }
                }
                Some(Ok(Token::CloseBracket)) if bracket_counter == 1 => {
                    lex.next();
                    bracket_counter = 0;
                }
                Some(Ok(Token::StartBlock)) => {
                    function_def.block = BlockAst::from_lexer(lex)?;
                    break;
                }
                None | Some(Ok(Token::DefineFunction)) => {
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
        match parse_expr(lex, 0)? {
            Ast::Expression(e) => Ok(e),
            single => Ok(ExpressionAst {
                function: String::new(),
                args: vec![single],
            }),
        }
    }
}

fn infix_precedence(token: &Token) -> Option<u8> {
    match token {
        Token::GreaterThan
        | Token::LessThan
        | Token::GreaterThanOrEqual
        | Token::LessThanOrEqual
        | Token::EqualEqual
        | Token::NotEqual => Some(1),
        Token::Add | Token::Subtract => Some(2),
        Token::Multiply | Token::Divide | Token::Modulo => Some(3),
        _ => None,
    }
}

fn infix_name(token: &Token) -> &'static str {
    match token {
        Token::Add => "add",
        Token::Subtract => "subtract",
        Token::Multiply => "multiply",
        Token::Divide => "divide",
        Token::Modulo => "modulo",
        Token::GreaterThan => "gt",
        Token::LessThan => "lt",
        Token::GreaterThanOrEqual => "gte",
        Token::LessThanOrEqual => "lte",
        Token::EqualEqual => "eq",
        Token::NotEqual => "ne",
        _ => unreachable!(),
    }
}

fn parse_expr<'a>(lex: &mut ParseLexer<'a>, min_prec: u8) -> Result<Ast, ParseError<'a>> {
    let mut lhs = parse_primary(lex)?;

    loop {
        let prec = match lex.peek() {
            Some(Ok(t)) => match infix_precedence(t) {
                Some(p) if p >= min_prec => p,
                _ => break,
            },
            _ => break,
        };

        let op = lex.next().unwrap().unwrap();
        let rhs = parse_expr(lex, prec + 1)?;

        lhs = Ast::Expression(ExpressionAst {
            function: infix_name(&op).to_string(),
            args: vec![lhs, rhs],
        });
    }

    Ok(lhs)
}

fn parse_primary<'a>(lex: &mut ParseLexer<'a>) -> Result<Ast, ParseError<'a>> {
    match lex.peek() {
        Some(Ok(x)) if x.kind() == TokenKind::Integer => {
            Ok(Ast::Literal(LiteralAst::from_lexer(lex)?))
        }
        Some(Ok(x)) if x.kind() == TokenKind::Symbol => {
            let Token::Symbol(name) = lex.next().unwrap().unwrap() else {
                unreachable!()
            };
            if lex.peek() == Some(&Ok(Token::OpenBracket)) {
                lex.next();
                let mut args = vec![];
                loop {
                    match lex.peek() {
                        Some(Ok(Token::CloseBracket)) => {
                            lex.next();
                            break;
                        }
                        Some(Ok(Token::Comma)) => {
                            lex.next();
                        }
                        _ => args.push(parse_expr(lex, 0)?),
                    }
                }
                Ok(Ast::Expression(ExpressionAst {
                    function: name,
                    args,
                }))
            } else {
                Ok(Ast::Variable(name))
            }
        }
        _ => Err(ParseError::unexpected(lex)),
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

#[test]
fn parse_operator_precedence() {
    use Ast::*;

    // 2 + 3 * 4 must parse as add(2, multiply(3, 4)), not multiply(add(2,3), 4)
    let text = "fn main() do\n    2 + 3 * 4\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function: "add".to_string(),
                args: vec![
                    Literal(LiteralAst::Integer(2)),
                    Expression(ExpressionAst {
                        function: "multiply".to_string(),
                        args: vec![
                            Literal(LiteralAst::Integer(3)),
                            Literal(LiteralAst::Integer(4)),
                        ],
                    }),
                ],
            })],
        },
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_function_call_single_arg() {
    use Ast::*;

    let text = "fn main() do\n    double(21)\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function: "double".to_string(),
                args: vec![Literal(LiteralAst::Integer(21))],
            })],
        },
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_function_call_multi_args() {
    use Ast::*;

    let text = "fn main() do\n    add(1, 2)\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function: "add".to_string(),
                args: vec![
                    Literal(LiteralAst::Integer(1)),
                    Literal(LiteralAst::Integer(2)),
                ],
            })],
        },
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_function_call_in_expression() {
    use Ast::*;

    // double(3) + 1 — call result used in infix, no passthrough wrapper
    let text = "fn main() do\n    double(3) + 1\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function: "add".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function: "double".to_string(),
                        args: vec![Literal(LiteralAst::Integer(3))],
                    }),
                    Literal(LiteralAst::Integer(1)),
                ],
            })],
        },
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_function_calling_other_with_params() {
    use Ast::*;

    let text = "fn double(x) do\n    add(x, x)\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "double".to_string(),
        inputs: vec!["x".to_string()],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function: "add".to_string(),
                args: vec![Variable("x".to_string()), Variable("x".to_string())],
            })],
        },
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_if_without_else_elixir() {
    use Ast::*;

    let text = "fn main() do\n    if x > 5 do\n        x\n    end\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![If {
                condition: Box::new(Expression(ExpressionAst {
                    function: "gt".to_string(),
                    args: vec![Variable("x".to_string()), Literal(LiteralAst::Integer(5))],
                })),
                then: BlockAst {
                    lines: vec![Variable("x".to_string())],
                },
                else_: None,
            }],
        },
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_if_with_else_elixir() {
    use Ast::*;

    let text = "fn main() do\n    if x > 5 do\n        1\n    else\n        2\n    end\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![If {
                condition: Box::new(Expression(ExpressionAst {
                    function: "gt".to_string(),
                    args: vec![Variable("x".to_string()), Literal(LiteralAst::Integer(5))],
                })),
                then: BlockAst {
                    lines: vec![Literal(LiteralAst::Integer(1))],
                },
                else_: Some(BlockAst {
                    lines: vec![Literal(LiteralAst::Integer(2))],
                }),
            }],
        },
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_if_with_else_python() {
    use Ast::*;

    let text = "fn main():\n    if x > 5:\n        1\n    else:\n        2\n";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![If {
                condition: Box::new(Expression(ExpressionAst {
                    function: "gt".to_string(),
                    args: vec![Variable("x".to_string()), Literal(LiteralAst::Integer(5))],
                })),
                then: BlockAst {
                    lines: vec![Literal(LiteralAst::Integer(1))],
                },
                else_: Some(BlockAst {
                    lines: vec![Literal(LiteralAst::Integer(2))],
                }),
            }],
        },
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_comparison_precedence() {
    use Ast::*;

    // 1 + 2 > 3 - 1 must parse as gt(add(1,2), subtract(3,1)), not add(1, gt(2,...))
    let text = "fn main() do\n    1 + 2 > 3 - 1\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function: "gt".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function: "add".to_string(),
                        args: vec![
                            Literal(LiteralAst::Integer(1)),
                            Literal(LiteralAst::Integer(2)),
                        ],
                    }),
                    Expression(ExpressionAst {
                        function: "subtract".to_string(),
                        args: vec![
                            Literal(LiteralAst::Integer(3)),
                            Literal(LiteralAst::Integer(1)),
                        ],
                    }),
                ],
            })],
        },
    });

    assert_eq!(ast, expected);
}
