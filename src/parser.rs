use crate::source::Span;
use crate::tokenizer::{Lexer, LexingError, Token};
use std::collections::VecDeque;
use std::ops::Deref;

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Option<Span>,
}

impl Ident {
    pub fn synthetic(name: String) -> Self {
        Self { name, span: None }
    }

    pub fn spanned(name: String, span: Span) -> Self {
        Self { name, span: Some(span) }
    }

    pub fn as_str(&self) -> &str {
        &self.name
    }
}

impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.name
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.name
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Ident {}

#[derive(Debug)]
pub struct ParseLexer<'a> {
    lexer: Lexer<'a>,
    buf: VecDeque<Result<Token, LexingError>>,
    spans: VecDeque<Span>,
    last_span: Option<Span>,
}

impl<'a> ParseLexer<'a> {
    pub fn new(lex: Lexer<'a>) -> Self {
        ParseLexer { lexer: lex, buf: VecDeque::new(), spans: VecDeque::new(), last_span: None }
    }

    pub fn peek(&mut self) -> Option<&Result<Token, LexingError>> {
        if self.buf.is_empty() {
            let token = self.lexer.next()?;
            self.buf.push_back(token);
            self.spans
                .push_back(Span { start: self.lexer.span().start, end: self.lexer.span().end });
        }
        self.buf.front()
    }

    pub fn peek_n(&mut self, index: usize) -> Option<&Result<Token, LexingError>> {
        while self.buf.len() <= index {
            let token = self.lexer.next()?;
            self.buf.push_back(token);
            self.spans
                .push_back(Span { start: self.lexer.span().start, end: self.lexer.span().end });
        }
        self.buf.get(index)
    }

    pub fn push_back(&mut self, token: Result<Token, LexingError>, span: Span) {
        self.buf.push_front(token);
        self.spans.push_front(span);
    }

    pub fn last_span(&self) -> Option<Span> {
        self.last_span.clone()
    }

    pub fn peek_span(&mut self) -> Option<Span> {
        self.peek()?;
        self.spans.front().cloned()
    }
}

impl<'a> Iterator for ParseLexer<'a> {
    type Item = Result<Token, LexingError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(tok) = self.buf.pop_front() {
            self.last_span = self.spans.pop_front();
            return Some(tok);
        }
        let token = self.lexer.next()?;
        self.last_span = Some(Span { start: self.lexer.span().start, end: self.lexer.span().end });
        Some(token)
    }
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub span: Span,
    pub text: &'a str,
}

impl<'a> ParseError<'a> {
    fn unexpected(lex: &mut ParseLexer<'a>) -> ParseError<'a> {
        let span =
            lex.peek_span().unwrap_or_else(|| lex.last_span().unwrap_or(Span { start: 0, end: 0 }));
        ParseError { span, text: lex.lexer.slice() }
        // ParseError { span, text }
    }
}

impl std::fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unexpected token {:?}", self.text)
    }
}

#[derive(Debug, Clone)]
pub enum Ast {
    Block(BlockAst),
    FunctionDef(FunctionDefAst),
    Lambda { inputs: Vec<String>, body: Box<Ast> },
    FunctionRef(Ident),
    Expression(ExpressionAst),
    MultiValue(Vec<Ast>),
    Literal(LiteralAst),
    ListLiteral(Vec<Ast>),
    Index { collection: Box<Ast>, index: Box<Ast>, span: Option<Span> },
    IndexAssign { collection: Box<Ast>, index: Box<Ast>, value: Box<Ast>, span: Option<Span> },
    Variable(Ident),
    Assign { name: String, value: Box<Ast>, span: Option<Span> },
    MultiAssign { names: Vec<String>, value: Box<Ast>, span: Option<Span> },
    If { condition: Box<Ast>, then: BlockAst, else_: Option<BlockAst>, span: Option<Span> },
}

impl PartialEq for Ast {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ast::Block(a), Ast::Block(b)) => a == b,
            (Ast::FunctionDef(a), Ast::FunctionDef(b)) => a == b,
            (
                Ast::Lambda { inputs: a_inputs, body: a_body },
                Ast::Lambda { inputs: b_inputs, body: b_body },
            ) => a_inputs == b_inputs && a_body == b_body,
            (Ast::FunctionRef(a), Ast::FunctionRef(b)) => a == b,
            (Ast::Expression(a), Ast::Expression(b)) => a == b,
            (Ast::MultiValue(a), Ast::MultiValue(b)) => a == b,
            (Ast::Literal(a), Ast::Literal(b)) => a == b,
            (Ast::ListLiteral(a), Ast::ListLiteral(b)) => a == b,
            (
                Ast::Index { collection: a_collection, index: a_index, .. },
                Ast::Index { collection: b_collection, index: b_index, .. },
            ) => a_collection == b_collection && a_index == b_index,
            (
                Ast::IndexAssign {
                    collection: a_collection, index: a_index, value: a_value, ..
                },
                Ast::IndexAssign {
                    collection: b_collection, index: b_index, value: b_value, ..
                },
            ) => a_collection == b_collection && a_index == b_index && a_value == b_value,
            (Ast::Variable(a), Ast::Variable(b)) => a == b,
            (
                Ast::Assign { name: a_name, value: a_value, .. },
                Ast::Assign { name: b_name, value: b_value, .. },
            ) => a_name == b_name && a_value == b_value,
            (
                Ast::MultiAssign { names: a_names, value: a_value, .. },
                Ast::MultiAssign { names: b_names, value: b_value, .. },
            ) => a_names == b_names && a_value == b_value,
            (
                Ast::If { condition: a_condition, then: a_then, else_: a_else, .. },
                Ast::If { condition: b_condition, then: b_then, else_: b_else, .. },
            ) => a_condition == b_condition && a_then == b_then && a_else == b_else,
            _ => false,
        }
    }
}

fn trim_newlines<'a>(lex: &mut ParseLexer<'a>) {
    while lex.peek() == Some(&Ok(Token::Newline)) {
        lex.next();
    }
}

fn peek_line_indent<'a>(lex: &mut ParseLexer<'a>) -> usize {
    let mut count = 0;
    while matches!(lex.peek_n(count), Some(Ok(Token::Indent))) {
        count += 1;
    }
    count
}

fn consume_indents<'a>(lex: &mut ParseLexer<'a>, count: usize) {
    for _ in 0..count {
        assert!(lex.next() == Some(Ok(Token::Indent)));
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
            Some(Ok(Token::Else)) | Some(Ok(Token::Elif)) => break,
            _ => block.lines.push(Ast::from_lexer(lex)?),
        }
    }
    Ok(block)
}

fn parse_block_after_colon<'a>(lex: &mut ParseLexer<'a>) -> Result<BlockAst, ParseError<'a>> {
    let mut block = BlockAst::default();
    assert!(lex.next() == Some(Ok(Token::ColonBlock)));

    trim_newlines(lex);
    let block_indent = peek_line_indent(lex);
    if block_indent == 0 {
        return Err(ParseError::unexpected(lex));
    }

    loop {
        trim_newlines(lex);
        match lex.peek() {
            Some(Ok(Token::DefineFunction)) | Some(Ok(Token::EndBlock)) | None => break,
            _ => {
                let line_indent = peek_line_indent(lex);
                if line_indent < block_indent {
                    break;
                }
                if line_indent > block_indent {
                    return Err(ParseError::unexpected(lex));
                }
                consume_indents(lex, line_indent);
                if lex.peek().is_none() {
                    break;
                }
                if matches!(lex.peek(), Some(Ok(Token::Newline))) {
                    lex.next();
                    continue;
                }
                if matches!(lex.peek(), Some(Ok(Token::Else)) | Some(Ok(Token::Elif))) {
                    break;
                }
                block.lines.push(Ast::from_lexer_with_indent(lex, block_indent)?);
            }
        }
    }

    Ok(block)
}

fn parse_if_after_keyword<'a>(
    lex: &mut ParseLexer<'a>,
    current_indent: usize,
) -> Result<Ast, ParseError<'a>> {
    let if_span = lex.last_span();
    let condition = parse_expr(lex, 0)?;
    let then = match lex.peek() {
        Some(Ok(Token::DoBlock)) => BlockAst::from_lexer(lex)?,
        Some(Ok(Token::ColonBlock)) => parse_block_after_colon(lex)?,
        _ => return Err(ParseError::unexpected(lex)),
    };
    trim_newlines(lex);
    let branch_indent = peek_line_indent(lex);
    let else_ = if branch_indent == current_indent {
        match lex.peek_n(branch_indent) {
            Some(Ok(Token::Else)) => {
                consume_indents(lex, branch_indent);
                lex.next();
                trim_newlines(lex);
                match lex.peek() {
                    Some(Ok(Token::DoBlock)) => Some(BlockAst::from_lexer(lex)?),
                    Some(Ok(Token::ColonBlock)) => Some(parse_block_after_colon(lex)?),
                    _ => Some(parse_block_body_until_end(lex)?),
                }
            }
            Some(Ok(Token::Elif)) => {
                consume_indents(lex, branch_indent);
                lex.next();
                Some(BlockAst { lines: vec![parse_if_after_keyword(lex, current_indent)?] })
            }
            _ => None,
        }
    } else {
        None
    };

    Ok(Ast::If {
        condition: Box::new(condition),
        then,
        else_,
        span: if_span.zip(lex.last_span()).map(|(start, end)| Span::cover(start, end)),
    })
}

impl Ast {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        Self::from_lexer_with_indent(lex, 0)
    }

    fn from_lexer_with_indent<'a>(
        lex: &mut ParseLexer<'a>,
        current_indent: usize,
    ) -> Result<Self, ParseError<'a>> {
        trim_newlines(lex);

        if matches!(lex.peek(), Some(Ok(Token::Symbol(_)))) {
            if matches!(lex.peek_n(1), Some(Ok(Token::Comma))) {
                let mut names = vec![];
                let start_span = lex.peek_span();
                loop {
                    let Token::Symbol(name) = lex.next().unwrap().unwrap() else { unreachable!() };
                    names.push(name);
                    match lex.peek() {
                        Some(Ok(Token::Comma)) => {
                            lex.next();
                        }
                        Some(Ok(Token::Assign)) if names.len() >= 2 => {
                            lex.next();
                            let value = parse_statement_expr(lex)?;
                            return Ok(Ast::MultiAssign {
                                names,
                                value: Box::new(value),
                                span: start_span
                                    .zip(lex.last_span())
                                    .map(|(start, end)| Span::cover(start, end)),
                            });
                        }
                        _ => break,
                    }
                }
                return Err(ParseError::unexpected(lex));
            }
            let Token::Symbol(name) = lex.next().unwrap().unwrap() else { unreachable!() };
            let lhs = parse_postfix(
                lex,
                Ast::Variable(Ident::spanned(
                    name,
                    lex.last_span().expect("consumed symbol should have a span"),
                )),
            )?;
            if lex.peek() == Some(&Ok(Token::Assign)) {
                lex.next();
                let assign_span = lex.last_span();
                let value = parse_statement_expr(lex)?;
                return match lhs {
                    Ast::Variable(name) => Ok(Ast::Assign {
                        name: name.name,
                        value: Box::new(value),
                        span: name
                            .span
                            .clone()
                            .or(assign_span)
                            .zip(lex.last_span())
                            .map(|(start, end)| Span::cover(start, end)),
                    }),
                    Ast::Index { collection, index, span } => Ok(Ast::IndexAssign {
                        collection,
                        index,
                        value: Box::new(value),
                        span: span
                            .or(assign_span)
                            .zip(lex.last_span())
                            .map(|(start, end)| Span::cover(start, end)),
                    }),
                    _ => Err(ParseError::unexpected(lex)),
                };
            }
            return parse_expr_with_lhs(lex, 0, lhs);
        }

        match lex.peek() {
            Some(&Ok(Token::DefineFunction)) => {
                if is_lambda_start(lex) {
                    parse_expr(lex, 0)
                } else {
                    Ok(Ast::FunctionDef(FunctionDefAst::from_lexer(lex)?))
                }
            }
            Some(&Ok(Token::If)) => {
                lex.next();
                parse_if_after_keyword(lex, current_indent)
            }
            _ => parse_statement_expr(lex),
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
        assert!(lex.next() == Some(Ok(Token::DoBlock)));

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
                Some(Ok(Token::Else)) | Some(Ok(Token::Elif)) => break,
                _ => block.lines.push(Ast::from_lexer(lex)?),
            }
        }

        Ok(block)
    }
}

#[derive(Debug, Default, Clone)]
pub struct FunctionDefAst {
    pub name: String,
    pub inputs: Vec<String>,
    pub output: Option<String>,
    pub block: BlockAst,
    pub span: Option<Span>,
}

impl PartialEq for FunctionDefAst {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.inputs == other.inputs
            && self.output == other.output
            && self.block == other.block
    }
}

impl FunctionDefAst {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        let mut function_def = FunctionDefAst::default();
        assert!(lex.next() == Some(Ok(Token::DefineFunction)));
        let start_span = lex.last_span();
        let mut first = true;
        let mut bracket_counter = 0;

        loop {
            match lex.peek() {
                Some(Ok(Token::Symbol(_))) if first => {
                    let Token::Symbol(name) = lex.next().unwrap().unwrap() else { unreachable!() };
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
                Some(Ok(Token::DoBlock)) => {
                    function_def.block = BlockAst::from_lexer(lex)?;
                    function_def.span =
                        start_span.zip(lex.last_span()).map(|(start, end)| Span::cover(start, end));
                    break;
                }
                Some(Ok(Token::ColonBlock)) => {
                    function_def.block = parse_block_after_colon(lex)?;
                    function_def.span =
                        start_span.zip(lex.last_span()).map(|(start, end)| Span::cover(start, end));
                    break;
                }
                None | Some(Ok(Token::DefineFunction)) => {
                    break;
                }
                Some(Ok(Token::Newline)) => {
                    break;
                }

                _ => return Err(ParseError::unexpected(lex)),
            }
        }

        Ok(function_def)
    }
}

#[derive(Debug, Default, Clone)]
pub struct ExpressionAst {
    pub function: String,
    pub args: Vec<Ast>,
    pub function_span: Option<Span>,
}

impl PartialEq for ExpressionAst {
    fn eq(&self, other: &Self) -> bool {
        self.function == other.function && self.args == other.args
    }
}

impl ExpressionAst {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        match parse_statement_expr(lex)? {
            Ast::Expression(e) => Ok(e),
            single => Ok(ExpressionAst {
                function_span: None,
                function: String::new(),
                args: vec![single],
            }),
        }
    }
}

fn infix_precedence(token: &Token) -> Option<u8> {
    match token {
        Token::Or => Some(0),
        Token::And => Some(1),
        Token::GreaterThan
        | Token::LessThan
        | Token::GreaterThanOrEqual
        | Token::LessThanOrEqual
        | Token::EqualEqual
        | Token::NotEqual => Some(2),
        Token::Add | Token::Subtract => Some(3),
        Token::Multiply | Token::Divide | Token::Modulo => Some(4),
        _ => None,
    }
}

fn infix_name(token: &Token) -> &'static str {
    match token {
        Token::And => "and",
        Token::Or => "or",
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
            function_span: lex.last_span(),
            function: infix_name(&op).to_string(),
            args: vec![lhs, rhs],
        });
    }

    Ok(lhs)
}

fn parse_statement_expr<'a>(lex: &mut ParseLexer<'a>) -> Result<Ast, ParseError<'a>> {
    let first = parse_expr(lex, 0)?;
    if lex.peek() != Some(&Ok(Token::Comma)) {
        return Ok(first);
    }

    let mut values = vec![first];
    while lex.peek() == Some(&Ok(Token::Comma)) {
        lex.next();
        values.push(parse_expr(lex, 0)?);
    }
    Ok(Ast::MultiValue(values))
}

fn is_lambda_start<'a>(lex: &mut ParseLexer<'a>) -> bool {
    if lex.peek() != Some(&Ok(Token::DefineFunction)) {
        return false;
    }

    let mut index = 1;
    let mut expect_symbol = true;
    loop {
        match lex.peek_n(index) {
            Some(Ok(Token::Symbol(_))) if expect_symbol => {
                expect_symbol = false;
                index += 1;
            }
            Some(Ok(Token::Comma)) if !expect_symbol => {
                expect_symbol = true;
                index += 1;
            }
            Some(Ok(Token::Arrow)) if !expect_symbol => return true,
            _ => return false,
        }
    }
}

fn parse_lambda<'a>(lex: &mut ParseLexer<'a>) -> Result<Ast, ParseError<'a>> {
    assert!(lex.next() == Some(Ok(Token::DefineFunction)));
    let mut inputs = vec![];

    loop {
        match lex.next() {
            Some(Ok(Token::Symbol(name))) => inputs.push(name),
            _ => return Err(ParseError::unexpected(lex)),
        }

        match lex.peek() {
            Some(Ok(Token::Comma)) => {
                lex.next();
            }
            Some(Ok(Token::Arrow)) => {
                lex.next();
                break;
            }
            _ => return Err(ParseError::unexpected(lex)),
        }
    }

    let body = if lex.peek() == Some(&Ok(Token::Newline)) {
        Ast::Block(parse_block_body_until_end(lex)?)
    } else {
        let body = parse_expr(lex, 0)?;
        trim_newlines(lex);
        if lex.next() != Some(Ok(Token::EndBlock)) {
            return Err(ParseError::unexpected(lex));
        }
        body
    };

    Ok(Ast::Lambda { inputs, body: Box::new(body) })
}

fn parse_primary<'a>(lex: &mut ParseLexer<'a>) -> Result<Ast, ParseError<'a>> {
    if is_lambda_start(lex) {
        return parse_lambda(lex);
    }

    if lex.peek() == Some(&Ok(Token::Not)) {
        lex.next();
        let rhs = parse_expr(lex, 2)?;
        return Ok(Ast::Expression(ExpressionAst {
            function_span: lex.last_span(),
            function: "not".to_string(),
            args: vec![rhs],
        }));
    }

    let lhs = match lex.peek() {
        Some(Ok(Token::True)) => {
            lex.next();
            Ast::Literal(LiteralAst::Integer(1))
        }
        Some(Ok(Token::False)) => {
            lex.next();
            Ast::Literal(LiteralAst::Integer(0))
        }
        Some(Ok(Token::Integer(_) | Token::BigIntLiteral(_) | Token::StringLiteral(_))) => {
            Ast::Literal(LiteralAst::from_lexer(lex)?)
        }
        Some(Ok(Token::Symbol(_))) => {
            let Token::Symbol(name) = lex.next().unwrap().unwrap() else { unreachable!() };
            Ast::Variable(Ident::spanned(
                name,
                lex.last_span().expect("consumed symbol should have a span"),
            ))
        }
        Some(Ok(Token::OpenBracket)) => {
            lex.next();
            let expr = parse_expr(lex, 0)?;
            if lex.next() != Some(Ok(Token::CloseBracket)) {
                return Err(ParseError::unexpected(lex));
            }
            expr
        }
        Some(Ok(Token::OpenSquareBracket)) => {
            lex.next();
            let mut items = vec![];
            loop {
                match lex.peek() {
                    Some(Ok(Token::CloseSquareBracket)) => {
                        lex.next();
                        break;
                    }
                    Some(Ok(Token::Comma)) => {
                        lex.next();
                    }
                    _ => items.push(parse_expr(lex, 0)?),
                }
            }
            Ast::ListLiteral(items)
        }
        _ => return Err(ParseError::unexpected(lex)),
    };

    parse_postfix(lex, lhs)
}

fn parse_postfix<'a>(lex: &mut ParseLexer<'a>, mut lhs: Ast) -> Result<Ast, ParseError<'a>> {
    loop {
        match lex.peek() {
            Some(Ok(Token::OpenBracket)) => {
                let Ast::Variable(function_name) = lhs else {
                    return Err(ParseError::unexpected(lex));
                };
                let function_span = function_name.span.clone();
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
                lhs = Ast::Expression(ExpressionAst {
                    function_span,
                    function: function_name.name,
                    args,
                });
            }
            Some(Ok(Token::OpenSquareBracket)) => {
                let start_span = lex.peek_span();
                lex.next();
                let index = parse_expr(lex, 0)?;
                if lex.next() != Some(Ok(Token::CloseSquareBracket)) {
                    return Err(ParseError::unexpected(lex));
                }
                lhs = Ast::Index {
                    collection: Box::new(lhs),
                    index: Box::new(index),
                    span: start_span
                        .zip(lex.last_span())
                        .map(|(start, end)| Span::cover(start, end)),
                };
            }
            _ => break,
        }
    }

    Ok(lhs)
}

fn parse_expr_with_lhs<'a>(
    lex: &mut ParseLexer<'a>,
    min_prec: u8,
    mut lhs: Ast,
) -> Result<Ast, ParseError<'a>> {
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
            function_span: lex.last_span(),
            function: infix_name(&op).to_string(),
            args: vec![lhs, rhs],
        });
    }

    Ok(lhs)
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralAst {
    Integer(i64),
    BigInt(String),
    String(String),
}

impl LiteralAst {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        match lex.next() {
            Some(Ok(Token::Integer(int))) => Ok(LiteralAst::Integer(int)),
            Some(Ok(Token::BigIntLiteral(value))) => Ok(LiteralAst::BigInt(value)),
            Some(Ok(Token::StringLiteral(value))) => Ok(LiteralAst::String(value)),
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
                function_span: None,
                function: "subtract".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function_span: None,
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
        span: None,
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
                function_span: None,
                function: "subtract".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function_span: None,
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
        span: None,
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
                function_span: None,
                function: "add".to_string(),
                args: vec![
                    Literal(LiteralAst::Integer(2)),
                    Expression(ExpressionAst {
                        function_span: None,
                        function: "multiply".to_string(),
                        args: vec![
                            Literal(LiteralAst::Integer(3)),
                            Literal(LiteralAst::Integer(4)),
                        ],
                    }),
                ],
            })],
        },
        span: None,
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
                function_span: None,
                function: "double".to_string(),
                args: vec![Literal(LiteralAst::Integer(21))],
            })],
        },
        span: None,
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
                function_span: None,
                function: "add".to_string(),
                args: vec![Literal(LiteralAst::Integer(1)), Literal(LiteralAst::Integer(2))],
            })],
        },
        span: None,
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
                function_span: None,
                function: "add".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function_span: None,
                        function: "double".to_string(),
                        args: vec![Literal(LiteralAst::Integer(3))],
                    }),
                    Literal(LiteralAst::Integer(1)),
                ],
            })],
        },
        span: None,
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
                function_span: None,
                function: "add".to_string(),
                args: vec![
                    Variable(Ident::synthetic("x".to_string())),
                    Variable(Ident::synthetic("x".to_string())),
                ],
            })],
        },
        span: None,
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
                    function_span: None,
                    function: "gt".to_string(),
                    args: vec![
                        Variable(Ident::synthetic("x".to_string())),
                        Literal(LiteralAst::Integer(5)),
                    ],
                })),
                then: BlockAst { lines: vec![Variable(Ident::synthetic("x".to_string()))] },
                else_: None,
                span: None,
            }],
        },
        span: None,
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
                    function_span: None,
                    function: "gt".to_string(),
                    args: vec![
                        Variable(Ident::synthetic("x".to_string())),
                        Literal(LiteralAst::Integer(5)),
                    ],
                })),
                then: BlockAst { lines: vec![Literal(LiteralAst::Integer(1))] },
                else_: Some(BlockAst { lines: vec![Literal(LiteralAst::Integer(2))] }),
                span: None,
            }],
        },
        span: None,
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
                    function_span: None,
                    function: "gt".to_string(),
                    args: vec![
                        Variable(Ident::synthetic("x".to_string())),
                        Literal(LiteralAst::Integer(5)),
                    ],
                })),
                then: BlockAst { lines: vec![Literal(LiteralAst::Integer(1))] },
                else_: Some(BlockAst { lines: vec![Literal(LiteralAst::Integer(2))] }),
                span: None,
            }],
        },
        span: None,
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
                function_span: None,
                function: "gt".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function_span: None,
                        function: "add".to_string(),
                        args: vec![
                            Literal(LiteralAst::Integer(1)),
                            Literal(LiteralAst::Integer(2)),
                        ],
                    }),
                    Expression(ExpressionAst {
                        function_span: None,
                        function: "subtract".to_string(),
                        args: vec![
                            Literal(LiteralAst::Integer(3)),
                            Literal(LiteralAst::Integer(1)),
                        ],
                    }),
                ],
            })],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_test_python_style_crlf() {
    use Ast::*;

    let text = "fn main():\r\n    1 + 2 - 3\r\n";

    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function_span: None,
                function: "subtract".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function_span: None,
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
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_function_def_invalid_token_returns_error() {
    let text = "fn main()) do\n    1\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);

    let ast = Ast::from_lexer(&mut lexer);
    assert!(ast.is_err());
}

#[test]
fn parse_list_literal() {
    use Ast::*;

    let text = "fn main() do\n    [1, 2, 3]\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![ListLiteral(vec![
                Literal(LiteralAst::Integer(1)),
                Literal(LiteralAst::Integer(2)),
                Literal(LiteralAst::Integer(3)),
            ])],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_index_expression() {
    use Ast::*;

    let text = "fn main() do\n    xs = [1, 2, 3]\n    xs[1]\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![
                Assign {
                    name: "xs".to_string(),
                    value: Box::new(ListLiteral(vec![
                        Literal(LiteralAst::Integer(1)),
                        Literal(LiteralAst::Integer(2)),
                        Literal(LiteralAst::Integer(3)),
                    ])),
                    span: None,
                },
                Index {
                    collection: Box::new(Variable(Ident::synthetic("xs".to_string()))),
                    index: Box::new(Literal(LiteralAst::Integer(1))),
                    span: None,
                },
            ],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_index_assignment_expression() {
    use Ast::*;

    let text = "fn main() do\n    xs = [1, 2, 3]\n    xs[1] = 9\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![
                Assign {
                    name: "xs".to_string(),
                    value: Box::new(ListLiteral(vec![
                        Literal(LiteralAst::Integer(1)),
                        Literal(LiteralAst::Integer(2)),
                        Literal(LiteralAst::Integer(3)),
                    ])),
                    span: None,
                },
                IndexAssign {
                    collection: Box::new(Variable(Ident::synthetic("xs".to_string()))),
                    index: Box::new(Literal(LiteralAst::Integer(1))),
                    value: Box::new(Literal(LiteralAst::Integer(9))),
                    span: None,
                },
            ],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_lambda_expression() {
    use Ast::*;

    let text = "fn main() do\n    list_map(xs, fn item -> item * 2 end)\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function_span: None,
                function: "list_map".to_string(),
                args: vec![
                    Variable(Ident::synthetic("xs".to_string())),
                    Lambda {
                        inputs: vec!["item".to_string()],
                        body: Box::new(Expression(ExpressionAst {
                            function_span: None,
                            function: "multiply".to_string(),
                            args: vec![
                                Variable(Ident::synthetic("item".to_string())),
                                Literal(LiteralAst::Integer(2)),
                            ],
                        })),
                    },
                ],
            })],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_multiline_lambda_expression() {
    use Ast::*;

    let text = "fn main() do\n    list_map(xs, fn item ->\n        item * 2\n    end)\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function_span: None,
                function: "list_map".to_string(),
                args: vec![
                    Variable(Ident::synthetic("xs".to_string())),
                    Lambda {
                        inputs: vec!["item".to_string()],
                        body: Box::new(Block(BlockAst {
                            lines: vec![Expression(ExpressionAst {
                                function_span: None,
                                function: "multiply".to_string(),
                                args: vec![
                                    Variable(Ident::synthetic("item".to_string())),
                                    Literal(LiteralAst::Integer(2)),
                                ],
                            })],
                        })),
                    },
                ],
            })],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_bigint_literal_expression() {
    use Ast::*;

    let text = "fn main() do\n    123n\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst { lines: vec![Literal(LiteralAst::BigInt("123".to_string()))] },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_string_literal_expression() {
    use Ast::*;

    let text = r#"fn main() do
    "hello\tworld"
end"#;
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst { lines: vec![Literal(LiteralAst::String("hello\tworld".to_string()))] },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_parenthesized_expression() {
    use Ast::*;

    let text = "fn main() do\n    (2 + 3) * 4\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function_span: None,
                function: "multiply".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function_span: None,
                        function: "add".to_string(),
                        args: vec![
                            Literal(LiteralAst::Integer(2)),
                            Literal(LiteralAst::Integer(3)),
                        ],
                    }),
                    Literal(LiteralAst::Integer(4)),
                ],
            })],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_python_style_nested_if_with_inner_else() {
    use Ast::*;

    let text = "fn main():\n    if 1 < 2:\n        if 1 != 1:\n            15\n        else:\n            34\n    else:\n        67\n";
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
                    function_span: None,
                    function: "lt".to_string(),
                    args: vec![Literal(LiteralAst::Integer(1)), Literal(LiteralAst::Integer(2))],
                })),
                then: BlockAst {
                    lines: vec![If {
                        condition: Box::new(Expression(ExpressionAst {
                            function_span: None,
                            function: "ne".to_string(),
                            args: vec![
                                Literal(LiteralAst::Integer(1)),
                                Literal(LiteralAst::Integer(1)),
                            ],
                        })),
                        then: BlockAst { lines: vec![Literal(LiteralAst::Integer(15))] },
                        else_: Some(BlockAst { lines: vec![Literal(LiteralAst::Integer(34))] }),
                        span: None,
                    }],
                },
                else_: Some(BlockAst { lines: vec![Literal(LiteralAst::Integer(67))] }),
                span: None,
            }],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_python_style_nested_if_without_inner_else() {
    use Ast::*;

    let text =
        "fn main():\n    if 1 < 2:\n        if 1 == 2:\n            15\n    else:\n        67\n";
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
                    function_span: None,
                    function: "lt".to_string(),
                    args: vec![Literal(LiteralAst::Integer(1)), Literal(LiteralAst::Integer(2))],
                })),
                then: BlockAst {
                    lines: vec![If {
                        condition: Box::new(Expression(ExpressionAst {
                            function_span: None,
                            function: "eq".to_string(),
                            args: vec![
                                Literal(LiteralAst::Integer(1)),
                                Literal(LiteralAst::Integer(2)),
                            ],
                        })),
                        then: BlockAst { lines: vec![Literal(LiteralAst::Integer(15))] },
                        else_: None,
                        span: None,
                    }],
                },
                else_: Some(BlockAst { lines: vec![Literal(LiteralAst::Integer(67))] }),
                span: None,
            }],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_python_style_elif() {
    use Ast::*;

    let text = "fn main():\n    if 1 == 2:\n        10\n    elif 1 == 1:\n        20\n    else:\n        30\n";
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
                    function_span: None,
                    function: "eq".to_string(),
                    args: vec![Literal(LiteralAst::Integer(1)), Literal(LiteralAst::Integer(2))],
                })),
                then: BlockAst { lines: vec![Literal(LiteralAst::Integer(10))] },
                else_: Some(BlockAst {
                    lines: vec![If {
                        condition: Box::new(Expression(ExpressionAst {
                            function_span: None,
                            function: "eq".to_string(),
                            args: vec![
                                Literal(LiteralAst::Integer(1)),
                                Literal(LiteralAst::Integer(1)),
                            ],
                        })),
                        then: BlockAst { lines: vec![Literal(LiteralAst::Integer(20))] },
                        else_: Some(BlockAst { lines: vec![Literal(LiteralAst::Integer(30))] }),
                        span: None,
                    }],
                }),
                span: None,
            }],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_python_style_nested_elif() {
    use Ast::*;

    let text = "fn main():\n    if 1 == 2:\n        10\n    elif 2 == 3:\n        20\n    elif 3 == 3:\n        30\n    else:\n        40\n";
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
                    function_span: None,
                    function: "eq".to_string(),
                    args: vec![Literal(LiteralAst::Integer(1)), Literal(LiteralAst::Integer(2))],
                })),
                then: BlockAst { lines: vec![Literal(LiteralAst::Integer(10))] },
                else_: Some(BlockAst {
                    lines: vec![If {
                        condition: Box::new(Expression(ExpressionAst {
                            function_span: None,
                            function: "eq".to_string(),
                            args: vec![
                                Literal(LiteralAst::Integer(2)),
                                Literal(LiteralAst::Integer(3)),
                            ],
                        })),
                        then: BlockAst { lines: vec![Literal(LiteralAst::Integer(20))] },
                        else_: Some(BlockAst {
                            lines: vec![If {
                                condition: Box::new(Expression(ExpressionAst {
                                    function_span: None,
                                    function: "eq".to_string(),
                                    args: vec![
                                        Literal(LiteralAst::Integer(3)),
                                        Literal(LiteralAst::Integer(3)),
                                    ],
                                })),
                                then: BlockAst { lines: vec![Literal(LiteralAst::Integer(30))] },
                                else_: Some(BlockAst {
                                    lines: vec![Literal(LiteralAst::Integer(40))],
                                }),
                                span: None,
                            }],
                        }),
                        span: None,
                    }],
                }),
                span: None,
            }],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_identifier_with_digits() {
    use crate::parser::Ast::*;
    use crate::parser::LiteralAst;

    let mut lexer = ParseLexer::new(Token::lexer("fn utf8_width(x1) do\n    x1 + 1\nend"));
    let ast = Ast::from_lexer(&mut lexer).unwrap();
    assert_eq!(
        ast,
        FunctionDef(FunctionDefAst {
            name: "utf8_width".to_string(),
            inputs: vec!["x1".to_string()],
            output: None,
            block: BlockAst {
                lines: vec![Expression(ExpressionAst {
                    function_span: None,
                    function: "add".to_string(),
                    args: vec![
                        Variable(Ident::synthetic("x1".to_string())),
                        Literal(LiteralAst::Integer(1))
                    ],
                })],
            },
            span: None,
        })
    );
}

#[test]
fn parse_logical_operator_precedence() {
    use Ast::*;

    let text = "fn main() do\n    1 == 1 and 2 == 2 or 0\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function_span: None,
                function: "or".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function_span: None,
                        function: "and".to_string(),
                        args: vec![
                            Expression(ExpressionAst {
                                function_span: None,
                                function: "eq".to_string(),
                                args: vec![
                                    Literal(LiteralAst::Integer(1)),
                                    Literal(LiteralAst::Integer(1)),
                                ],
                            }),
                            Expression(ExpressionAst {
                                function_span: None,
                                function: "eq".to_string(),
                                args: vec![
                                    Literal(LiteralAst::Integer(2)),
                                    Literal(LiteralAst::Integer(2)),
                                ],
                            }),
                        ],
                    }),
                    Literal(LiteralAst::Integer(0)),
                ],
            })],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_not_operator_precedence() {
    use Ast::*;

    let text = "fn main() do\n    not 1 == 0 and 2\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();
    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function_span: None,
                function: "and".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function_span: None,
                        function: "not".to_string(),
                        args: vec![Expression(ExpressionAst {
                            function_span: None,
                            function: "eq".to_string(),
                            args: vec![
                                Literal(LiteralAst::Integer(1)),
                                Literal(LiteralAst::Integer(0)),
                            ],
                        })],
                    }),
                    Literal(LiteralAst::Integer(2)),
                ],
            })],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_boolean_alias_literals() {
    use Ast::*;

    let text = "fn main() do\n    true and not false\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![Expression(ExpressionAst {
                function_span: None,
                function: "and".to_string(),
                args: vec![
                    Literal(LiteralAst::Integer(1)),
                    Expression(ExpressionAst {
                        function_span: None,
                        function: "not".to_string(),
                        args: vec![Literal(LiteralAst::Integer(0))],
                    }),
                ],
            })],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_multi_value_expression() {
    use Ast::*;

    let text = "fn pair() do\n    1, 2\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "pair".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![MultiValue(vec![
                Literal(LiteralAst::Integer(1)),
                Literal(LiteralAst::Integer(2)),
            ])],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn parse_multi_assign_statement() {
    use Ast::*;

    let text = "fn main() do\n    a, b = pair()\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let expected = FunctionDef(FunctionDefAst {
        name: "main".to_string(),
        inputs: vec![],
        output: None,
        block: BlockAst {
            lines: vec![MultiAssign {
                names: vec!["a".to_string(), "b".to_string()],
                value: Box::new(Expression(ExpressionAst {
                    function_span: None,
                    function: "pair".to_string(),
                    args: vec![],
                })),
                span: None,
            }],
        },
        span: None,
    });

    assert_eq!(ast, expected);
}

#[test]
fn expression_from_lexer_wraps_non_call_expressions() {
    let text = "1";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let expr = ExpressionAst::from_lexer(&mut lexer).unwrap();
    assert_eq!(
        expr,
        ExpressionAst {
            function_span: None,
            function: String::new(),
            args: vec![Ast::Literal(LiteralAst::Integer(1))],
        }
    );
}
