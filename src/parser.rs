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
pub enum MapKeyAst {
    Static(String),
    Dynamic(Box<Ast>),
}

impl PartialEq for MapKeyAst {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Static(a), Self::Static(b)) => a == b,
            (Self::Dynamic(a), Self::Dynamic(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for MapKeyAst {}

#[derive(Debug, Clone, PartialEq)]
pub struct MapEntryAst {
    pub key: MapKeyAst,
    pub value: Ast,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDefAst {
    pub name: String,
    pub fields: Vec<String>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldValueAst {
    pub name: String,
    pub value: Ast,
}

#[derive(Debug, Clone)]
pub enum Ast {
    Block(BlockAst),
    FunctionDef(FunctionDefAst),
    StructDef(StructDefAst),
    Lambda { inputs: Vec<String>, body: Box<Ast> },
    FunctionRef(Ident),
    MethodCall { receiver: Box<Ast>, method: Ident, args: Vec<Ast>, span: Option<Span> },
    FieldAccess { base: Box<Ast>, field: Ident, span: Option<Span> },
    Expression(ExpressionAst),
    MultiValue(Vec<Ast>),
    Literal(LiteralAst),
    ListLiteral(Vec<Ast>),
    MapLiteral(Vec<MapEntryAst>),
    StructLiteral { type_name: Ident, fields: Vec<StructFieldValueAst>, span: Option<Span> },
    Index { collection: Box<Ast>, index: Box<Ast>, span: Option<Span> },
    IndexAssign { collection: Box<Ast>, index: Box<Ast>, value: Box<Ast>, span: Option<Span> },
    FieldAssign { base: Box<Ast>, field: Ident, value: Box<Ast>, span: Option<Span> },
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
            (Ast::StructDef(a), Ast::StructDef(b)) => a == b,
            (
                Ast::Lambda { inputs: a_inputs, body: a_body },
                Ast::Lambda { inputs: b_inputs, body: b_body },
            ) => ast_eq_lambda(a_inputs, a_body, b_inputs, b_body),
            (Ast::FunctionRef(a), Ast::FunctionRef(b)) => a == b,
            (
                Ast::MethodCall { receiver: a_receiver, method: a_method, args: a_args, .. },
                Ast::MethodCall { receiver: b_receiver, method: b_method, args: b_args, .. },
            ) => ast_eq_method_call(a_receiver, a_method, a_args, b_receiver, b_method, b_args),
            (
                Ast::FieldAccess { base: a_base, field: a_field, .. },
                Ast::FieldAccess { base: b_base, field: b_field, .. },
            ) => ast_eq_field_access(a_base, a_field, b_base, b_field),
            (Ast::Expression(a), Ast::Expression(b)) => a == b,
            (Ast::MultiValue(a), Ast::MultiValue(b)) => a == b,
            (Ast::Literal(a), Ast::Literal(b)) => a == b,
            (Ast::ListLiteral(a), Ast::ListLiteral(b)) => a == b,
            (Ast::MapLiteral(a), Ast::MapLiteral(b)) => a == b,
            (
                Ast::StructLiteral { type_name: a_type, fields: a_fields, .. },
                Ast::StructLiteral { type_name: b_type, fields: b_fields, .. },
            ) => a_type == b_type && a_fields == b_fields,
            (
                Ast::Index { collection: a_collection, index: a_index, .. },
                Ast::Index { collection: b_collection, index: b_index, .. },
            ) => ast_eq_index(a_collection, a_index, b_collection, b_index),
            (
                Ast::IndexAssign {
                    collection: a_collection, index: a_index, value: a_value, ..
                },
                Ast::IndexAssign {
                    collection: b_collection, index: b_index, value: b_value, ..
                },
            ) => {
                ast_eq_index_assign(a_collection, a_index, a_value, b_collection, b_index, b_value)
            }
            (
                Ast::FieldAssign { base: a_base, field: a_field, value: a_value, .. },
                Ast::FieldAssign { base: b_base, field: b_field, value: b_value, .. },
            ) => a_base == b_base && a_field == b_field && a_value == b_value,
            (Ast::Variable(a), Ast::Variable(b)) => a == b,
            (
                Ast::Assign { name: a_name, value: a_value, .. },
                Ast::Assign { name: b_name, value: b_value, .. },
            ) => ast_eq_assign(a_name, a_value, b_name, b_value),
            (
                Ast::MultiAssign { names: a_names, value: a_value, .. },
                Ast::MultiAssign { names: b_names, value: b_value, .. },
            ) => ast_eq_multi_assign(a_names, a_value, b_names, b_value),
            (
                Ast::If { condition: a_condition, then: a_then, else_: a_else, .. },
                Ast::If { condition: b_condition, then: b_then, else_: b_else, .. },
            ) => ast_eq_if(
                a_condition,
                a_then,
                a_else.as_ref(),
                b_condition,
                b_then,
                b_else.as_ref(),
            ),
            _ => false,
        }
    }
}

fn ast_eq_lambda(a_inputs: &[String], a_body: &Ast, b_inputs: &[String], b_body: &Ast) -> bool {
    a_inputs == b_inputs && a_body == b_body
}

fn ast_eq_method_call(
    a_receiver: &Ast,
    a_method: &Ident,
    a_args: &[Ast],
    b_receiver: &Ast,
    b_method: &Ident,
    b_args: &[Ast],
) -> bool {
    a_receiver == b_receiver && a_method == b_method && a_args == b_args
}

fn ast_eq_field_access(a_base: &Ast, a_field: &Ident, b_base: &Ast, b_field: &Ident) -> bool {
    a_base == b_base && a_field == b_field
}

fn ast_eq_index(a_collection: &Ast, a_index: &Ast, b_collection: &Ast, b_index: &Ast) -> bool {
    a_collection == b_collection && a_index == b_index
}

fn ast_eq_index_assign(
    a_collection: &Ast,
    a_index: &Ast,
    a_value: &Ast,
    b_collection: &Ast,
    b_index: &Ast,
    b_value: &Ast,
) -> bool {
    a_collection == b_collection && a_index == b_index && a_value == b_value
}

fn ast_eq_assign(a_name: &str, a_value: &Ast, b_name: &str, b_value: &Ast) -> bool {
    a_name == b_name && a_value == b_value
}

fn ast_eq_multi_assign(
    a_names: &[String],
    a_value: &Ast,
    b_names: &[String],
    b_value: &Ast,
) -> bool {
    a_names == b_names && a_value == b_value
}

fn ast_eq_if(
    a_condition: &Ast,
    a_then: &BlockAst,
    a_else: Option<&BlockAst>,
    b_condition: &Ast,
    b_then: &BlockAst,
    b_else: Option<&BlockAst>,
) -> bool {
    a_condition == b_condition && a_then == b_then && a_else == b_else
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
                    Ast::FieldAccess { base, field, span } => Ok(Ast::FieldAssign {
                        base,
                        field,
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
            Some(&Ok(Token::DefineStruct)) => Ok(Ast::StructDef(StructDefAst::from_lexer(lex)?)),
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

impl StructDefAst {
    pub fn from_lexer<'a>(lex: &mut ParseLexer<'a>) -> Result<Self, ParseError<'a>> {
        assert!(lex.next() == Some(Ok(Token::DefineStruct)));
        let start_span = lex.last_span();
        let Some(Ok(Token::Symbol(name))) = lex.next() else {
            return Err(ParseError::unexpected(lex));
        };
        if lex.next() != Some(Ok(Token::Assign)) {
            return Err(ParseError::unexpected(lex));
        }
        if lex.next() != Some(Ok(Token::OpenBrace)) {
            return Err(ParseError::unexpected(lex));
        }
        let mut fields = vec![];
        loop {
            match lex.peek() {
                Some(Ok(Token::CloseBrace)) => {
                    lex.next();
                    break;
                }
                Some(Ok(Token::Comma)) | Some(Ok(Token::Newline)) | Some(Ok(Token::Indent)) => {
                    lex.next();
                }
                Some(Ok(Token::Symbol(_))) => {
                    let Some(Ok(Token::Symbol(field))) = lex.next() else { unreachable!() };
                    fields.push(field);
                }
                _ => return Err(ParseError::unexpected(lex)),
            }
        }
        Ok(Self {
            name,
            fields,
            span: start_span.zip(lex.last_span()).map(|(start, end)| Span::cover(start, end)),
        })
    }
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
        Token::BitOr => Some(3),
        Token::BitXor => Some(4),
        Token::BitAnd => Some(5),
        Token::ShiftLeft | Token::ShiftRight => Some(6),
        Token::Add | Token::Subtract => Some(7),
        Token::Multiply | Token::Divide | Token::Modulo => Some(8),
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
        Token::BitAnd => "bitand",
        Token::BitOr => "bitor",
        Token::BitXor => "bitxor",
        Token::ShiftLeft => "shl",
        Token::ShiftRight => "shr",
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

fn parse_not_expression<'a>(lex: &mut ParseLexer<'a>) -> Result<Ast, ParseError<'a>> {
    lex.next();
    let rhs = parse_expr(lex, 2)?;
    Ok(Ast::Expression(ExpressionAst {
        function_span: lex.last_span(),
        function: "not".to_string(),
        args: vec![rhs],
    }))
}

fn parse_symbol_variable<'a>(lex: &mut ParseLexer<'a>) -> Result<Ast, ParseError<'a>> {
    let Token::Symbol(name) = lex.next().unwrap().unwrap() else { unreachable!() };
    let ident = Ident::spanned(name, lex.last_span().expect("consumed symbol should have a span"));
    if lex.peek() == Some(&Ok(Token::OpenBrace)) {
        let start_span = ident.span.clone();
        let fields = parse_struct_literal_fields(lex)?;
        return Ok(Ast::StructLiteral {
            type_name: ident,
            fields,
            span: start_span.zip(lex.last_span()).map(|(start, end)| Span::cover(start, end)),
        });
    }
    Ok(Ast::Variable(ident))
}

fn parse_grouped_expression<'a>(lex: &mut ParseLexer<'a>) -> Result<Ast, ParseError<'a>> {
    lex.next();
    let expr = parse_expr(lex, 0)?;
    if lex.next() != Some(Ok(Token::CloseBracket)) {
        return Err(ParseError::unexpected(lex));
    }
    Ok(expr)
}

fn parse_list_literal_primary<'a>(lex: &mut ParseLexer<'a>) -> Result<Ast, ParseError<'a>> {
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
    Ok(Ast::ListLiteral(items))
}

fn parse_struct_literal_fields<'a>(
    lex: &mut ParseLexer<'a>,
) -> Result<Vec<StructFieldValueAst>, ParseError<'a>> {
    assert!(lex.next() == Some(Ok(Token::OpenBrace)));
    let mut fields = vec![];
    loop {
        let next = lex.peek().cloned();
        let next_next = lex.peek_n(1).cloned();
        match next {
            Some(Ok(Token::CloseBrace)) => {
                lex.next();
                break;
            }
            Some(Ok(Token::Comma)) | Some(Ok(Token::Newline)) | Some(Ok(Token::Indent)) => {
                lex.next();
            }
            Some(Ok(Token::Symbol(_))) if next_next == Some(Ok(Token::ColonBlock)) => {
                let Some(Ok(Token::Symbol(name))) = lex.next() else { unreachable!() };
                lex.next();
                let value = parse_expr(lex, 0)?;
                fields.push(StructFieldValueAst { name, value });
            }
            _ => return Err(ParseError::unexpected(lex)),
        }
    }
    Ok(fields)
}

fn parse_map_literal_primary<'a>(lex: &mut ParseLexer<'a>) -> Result<Ast, ParseError<'a>> {
    lex.next();
    let mut entries = vec![];
    loop {
        let next = lex.peek().cloned();
        let next_next = lex.peek_n(1).cloned();
        match next {
            Some(Ok(Token::CloseBrace)) => {
                lex.next();
                break;
            }
            Some(Ok(Token::Comma)) | Some(Ok(Token::Newline)) | Some(Ok(Token::Indent)) => {
                lex.next();
            }
            Some(Ok(Token::Symbol(_))) if next_next == Some(Ok(Token::ColonBlock)) => {
                let Token::Symbol(key) = lex.next().unwrap().unwrap() else { unreachable!() };
                lex.next();
                let value = parse_expr(lex, 0)?;
                entries.push(MapEntryAst { key: MapKeyAst::Static(key), value });
            }
            Some(Ok(Token::StringLiteral(_))) if next_next == Some(Ok(Token::ColonBlock)) => {
                let Token::StringLiteral(key) = lex.next().unwrap().unwrap() else {
                    unreachable!()
                };
                lex.next();
                let value = parse_expr(lex, 0)?;
                entries.push(MapEntryAst { key: MapKeyAst::Static(key), value });
            }
            _ => {
                let key = parse_expr(lex, 0)?;
                if lex.next() != Some(Ok(Token::FatArrow)) {
                    return Err(ParseError::unexpected(lex));
                }
                let value = parse_expr(lex, 0)?;
                entries.push(MapEntryAst { key: MapKeyAst::Dynamic(Box::new(key)), value });
            }
        }
    }
    Ok(Ast::MapLiteral(entries))
}

fn parse_primary<'a>(lex: &mut ParseLexer<'a>) -> Result<Ast, ParseError<'a>> {
    if is_lambda_start(lex) {
        return parse_lambda(lex);
    }

    if lex.peek() == Some(&Ok(Token::Not)) {
        return parse_not_expression(lex);
    }

    let lhs = match lex.peek() {
        Some(Ok(Token::True)) => {
            lex.next();
            Ast::Literal(LiteralAst::Bool(true))
        }
        Some(Ok(Token::False)) => {
            lex.next();
            Ast::Literal(LiteralAst::Bool(false))
        }
        Some(Ok(Token::Integer(_) | Token::BigIntLiteral(_) | Token::StringLiteral(_))) => {
            Ast::Literal(LiteralAst::from_lexer(lex)?)
        }
        Some(Ok(Token::Symbol(_))) => parse_symbol_variable(lex)?,
        Some(Ok(Token::OpenBracket)) => parse_grouped_expression(lex)?,
        Some(Ok(Token::OpenSquareBracket)) => parse_list_literal_primary(lex)?,
        Some(Ok(Token::OpenBrace)) => parse_map_literal_primary(lex)?,
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
            Some(Ok(Token::OpenBrace)) => {
                let Ast::Variable(type_name) = lhs else {
                    break;
                };
                let start_span = type_name.span.clone();
                let fields = parse_struct_literal_fields(lex)?;
                lhs = Ast::StructLiteral {
                    type_name,
                    fields,
                    span: start_span
                        .zip(lex.last_span())
                        .map(|(start, end)| Span::cover(start, end)),
                };
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
            Some(Ok(Token::Dot)) => {
                let start_span = span_of_ast(&lhs).or_else(|| lex.peek_span());
                lex.next();
                let Some(Ok(Token::Symbol(method_name))) = lex.next() else {
                    return Err(ParseError::unexpected(lex));
                };
                let field_span =
                    lex.last_span().expect("consumed field or method symbol should have a span");
                if lex.peek() != Some(&Ok(Token::OpenBracket)) {
                    lhs = Ast::FieldAccess {
                        base: Box::new(lhs),
                        field: Ident::spanned(method_name, field_span.clone()),
                        span: start_span
                            .zip(Some(field_span))
                            .map(|(start, end)| Span::cover(start, end)),
                    };
                    continue;
                }
                lex.next();
                let method_ident = Ident::spanned(method_name, field_span);
                if lex.last_span().is_none() {
                    return Err(ParseError::unexpected(lex));
                }
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
                lhs = Ast::MethodCall {
                    receiver: Box::new(lhs),
                    method: method_ident,
                    args,
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
    Bool(bool),
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

fn span_of_ast_slice(values: &[Ast]) -> Option<Span> {
    values.first().and_then(span_of_ast)
}

fn span_of_map_entry(entry: &MapEntryAst) -> Option<Span> {
    match &entry.key {
        MapKeyAst::Dynamic(key) => span_of_ast(key).or_else(|| span_of_ast(&entry.value)),
        MapKeyAst::Static(_) => span_of_ast(&entry.value),
    }
}

fn span_of_map_entries(entries: &[MapEntryAst]) -> Option<Span> {
    entries.first().and_then(span_of_map_entry)
}

fn span_of_ast(ast: &Ast) -> Option<Span> {
    match ast {
        Ast::FunctionDef(func) => func.span.clone(),
        Ast::StructDef(def) => def.span.clone(),
        Ast::MethodCall { span, .. }
        | Ast::FieldAccess { span, .. }
        | Ast::Index { span, .. }
        | Ast::IndexAssign { span, .. }
        | Ast::FieldAssign { span, .. }
        | Ast::Assign { span, .. }
        | Ast::MultiAssign { span, .. }
        | Ast::If { span, .. } => span.clone(),
        Ast::Variable(name) | Ast::FunctionRef(name) => name.span.clone(),
        Ast::Expression(ExpressionAst { function_span, .. }) => function_span.clone(),
        Ast::Block(block) => span_of_ast_slice(&block.lines),
        Ast::Lambda { body, .. } => span_of_ast(body),
        Ast::MultiValue(values) | Ast::ListLiteral(values) => span_of_ast_slice(values),
        Ast::MapLiteral(entries) => span_of_map_entries(entries),
        Ast::StructLiteral { fields, .. } => {
            fields.first().and_then(|field| span_of_ast(&field.value))
        }
        Ast::Literal(_) => None,
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
fn parse_map_literal() {
    use Ast::*;

    let text = "fn main() do\n    a = {name: 1, other => 2}\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let result = Ast::from_lexer(&mut lexer);

    assert_eq!(
        result.unwrap(),
        FunctionDef(FunctionDefAst {
            name: "main".to_string(),
            inputs: vec![],
            output: None,
            block: BlockAst {
                lines: vec![Assign {
                    name: "a".to_string(),
                    value: Box::new(MapLiteral(vec![
                        MapEntryAst {
                            key: MapKeyAst::Static("name".to_string()),
                            value: Literal(LiteralAst::Integer(1)),
                        },
                        MapEntryAst {
                            key: MapKeyAst::Dynamic(Box::new(Variable(Ident::synthetic(
                                "other".to_string(),
                            )))),
                            value: Literal(LiteralAst::Integer(2)),
                        },
                    ])),
                    span: None,
                }],
            },
            span: None,
        })
    );
}

#[test]
fn parse_multiline_map_literal() {
    let text = "fn main() do\n    a = {\n        normal_key: 3,\n        \"key with spaces\": 1,\n        dyn_key => 2,\n    }\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let result = Ast::from_lexer(&mut lexer).expect("map literal should parse");
    let Ast::FunctionDef(func) = &result else { panic!("expected function") };
    let Ast::Assign { value, .. } = &func.block.lines[0] else { panic!("expected assign") };
    let Ast::MapLiteral(entries) = &**value else { panic!("expected map literal") };
    assert_eq!(entries.len(), 3);
}

#[test]
fn parse_method_call() {
    let text = "fn main() do\n    \"1234\".is_integer()\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let result = Ast::from_lexer(&mut lexer).expect("method call should parse");
    let Ast::FunctionDef(func) = result else { panic!("expected function") };
    let Ast::MethodCall { receiver, method, args, .. } = &func.block.lines[0] else {
        panic!("expected method call")
    };
    assert_eq!(receiver.as_ref(), &Ast::Literal(LiteralAst::String("1234".to_string())));
    assert_eq!(method.as_str(), "is_integer");
    assert!(args.is_empty());
}

#[test]
fn span_of_ast_covers_non_literal_variants() {
    let text = "fn main() do\n    a, b = thing()\n    value = { dynamic_key => [1, 2, 3] }\n    picked = value[0]\n    value[0] = picked\n    callback = fn x -> x end\n    callback\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let result = Ast::from_lexer(&mut lexer).expect("fixture should parse");
    let Ast::FunctionDef(func) = result else { panic!("expected function") };

    assert!(span_of_ast(&Ast::FunctionDef(func.clone())).is_some());
    assert!(span_of_ast(&Ast::Block(func.block.clone())).is_some());
    for line in &func.block.lines {
        assert!(span_of_ast(line).is_some());
    }

    let Ast::Assign { value: map_value, .. } = &func.block.lines[1] else {
        panic!("expected map assignment");
    };
    assert!(span_of_ast(map_value).is_some());

    let Ast::Assign { value: lambda_value, .. } = &func.block.lines[4] else {
        panic!("expected lambda assignment");
    };
    assert!(span_of_ast(lambda_value).is_some());
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
fn parse_struct_declaration() {
    let text = "struct Person = {name, age}";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let Ast::StructDef(def) = ast else {
        panic!("expected struct definition");
    };
    assert_eq!(def.name, "Person");
    assert_eq!(def.fields, vec!["name".to_string(), "age".to_string()]);
}

#[test]
fn parse_struct_literal() {
    let text = "fn main() do\n    Person { name: \"Ada\", age: 42 }\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let Ast::FunctionDef(function) = ast else {
        panic!("expected function definition");
    };
    let [Ast::StructLiteral { type_name, fields, .. }] = function.block.lines.as_slice() else {
        panic!("expected struct literal statement");
    };
    assert_eq!(type_name.as_str(), "Person");
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name, "name");
    assert_eq!(fields[0].value, Ast::Literal(LiteralAst::String("Ada".to_string())));
    assert_eq!(fields[1].name, "age");
    assert_eq!(fields[1].value, Ast::Literal(LiteralAst::Integer(42)));
}

#[test]
fn parse_field_access() {
    let text = "fn main() do\n    person.name\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let Ast::FunctionDef(function) = ast else {
        panic!("expected function definition");
    };
    let [Ast::FieldAccess { base, field, .. }] = function.block.lines.as_slice() else {
        panic!("expected field access statement");
    };
    assert_eq!(field.as_str(), "name");
    assert_eq!(base.as_ref(), &Ast::Variable(Ident::synthetic("person".to_string())));
}

#[test]
fn parse_field_assign() {
    let text = "fn main() do\n    person.name = \"Bob\"\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let Ast::FunctionDef(function) = ast else {
        panic!("expected function definition");
    };
    let [Ast::FieldAssign { base, field, value, .. }] = function.block.lines.as_slice() else {
        panic!("expected field assignment statement");
    };
    assert_eq!(field.as_str(), "name");
    assert_eq!(base.as_ref(), &Ast::Variable(Ident::synthetic("person".to_string())));
    assert_eq!(value.as_ref(), &Ast::Literal(LiteralAst::String("Bob".to_string())));
}

#[test]
fn parse_method_call_and_field_access_are_distinct() {
    let text = "fn main() do\n    value.name\n    value.name()\nend";
    let lex = tokenizer::Token::lexer(text);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let Ast::FunctionDef(function) = ast else {
        panic!("expected function definition");
    };
    assert!(matches!(function.block.lines[0], Ast::FieldAccess { .. }));
    assert!(matches!(function.block.lines[1], Ast::MethodCall { .. }));
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
fn parse_bitwise_operator_precedence() {
    use Ast::*;

    let text = "fn main() do\n    1 + 2 << 3 ^ 4 & 5 | 6\nend";
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
                function: "bitor".to_string(),
                args: vec![
                    Expression(ExpressionAst {
                        function_span: None,
                        function: "bitxor".to_string(),
                        args: vec![
                            Expression(ExpressionAst {
                                function_span: None,
                                function: "shl".to_string(),
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
                            }),
                            Expression(ExpressionAst {
                                function_span: None,
                                function: "bitand".to_string(),
                                args: vec![
                                    Literal(LiteralAst::Integer(4)),
                                    Literal(LiteralAst::Integer(5)),
                                ],
                            }),
                        ],
                    }),
                    Literal(LiteralAst::Integer(6)),
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
                    Literal(LiteralAst::Bool(true)),
                    Expression(ExpressionAst {
                        function_span: None,
                        function: "not".to_string(),
                        args: vec![Literal(LiteralAst::Bool(false))],
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
