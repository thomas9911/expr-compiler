use crate::module::CompileError;
use crate::parser::{
    Ast, BlockAst, ExpressionAst, FunctionDefAst, LiteralAst, MapEntryAst, MapKeyAst, ParseLexer,
    StructDefAst, StructFieldValueAst,
};
use crate::source::{Span, offset_to_line_col};
use crate::tokenizer::{Logos, Token};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockStyle {
    DoEnd,
    Python,
}

#[derive(Debug, Clone)]
pub struct FormatConfig {
    pub indent: &'static str,
    pub final_newline: bool,
    pub block_style: BlockStyle,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self { indent: "    ", final_newline: true, block_style: BlockStyle::DoEnd }
    }
}

impl FormatConfig {
    pub fn inferred_from_source(source: &str) -> Self {
        for token in Token::lexer(source).flatten() {
            match token {
                Token::ColonBlock => {
                    return Self { block_style: BlockStyle::Python, ..Self::default() };
                }
                Token::DoBlock => {
                    return Self { block_style: BlockStyle::DoEnd, ..Self::default() };
                }
                _ => {}
            }
        }
        Self::default()
    }
}

pub struct AstFormatter<'a> {
    config: &'a FormatConfig,
    source: &'a str,
    comments: Vec<LineComment>,
    next_comment: usize,
    expression_indent: usize,
}

struct CommentBlock {
    lines: Vec<String>,
    last_line: Option<usize>,
}

impl<'a> AstFormatter<'a> {
    pub fn new(config: &'a FormatConfig, source: &'a str) -> Self {
        Self {
            config,
            source,
            comments: extract_line_comments(source),
            next_comment: 0,
            expression_indent: 0,
        }
    }

    fn indent(&self, level: usize) -> String {
        self.config.indent.repeat(level)
    }

    fn escape_string(&self, value: &str) -> String {
        let mut out = String::new();
        for ch in value.chars() {
            match ch {
                '\\' => out.push_str("\\\\"),
                '"' => out.push_str("\\\""),
                '\n' => out.push_str("\\n"),
                '\r' => out.push_str("\\r"),
                '\t' => out.push_str("\\t"),
                _ => out.push(ch),
            }
        }
        out
    }

    fn infix_operator(&self, function: &str) -> Option<(&'static str, u8)> {
        match function {
            "or" => Some(("or", 0)),
            "and" => Some(("and", 1)),
            "gt" => Some((">", 2)),
            "lt" => Some(("<", 2)),
            "gte" => Some((">=", 2)),
            "lte" => Some(("<=", 2)),
            "eq" => Some(("==", 2)),
            "ne" => Some(("!=", 2)),
            "bitor" => Some(("|", 3)),
            "bitxor" => Some(("^", 4)),
            "bitand" => Some(("&", 5)),
            "shl" => Some(("<<", 6)),
            "shr" => Some((">>", 6)),
            "add" => Some(("+", 7)),
            "subtract" => Some(("-", 7)),
            "multiply" => Some(("*", 8)),
            "divide" => Some(("/", 8)),
            "modulo" => Some(("%", 8)),
            _ => None,
        }
    }

    fn needs_readability_parens(&self, parent_function: &str, child: &Ast) -> bool {
        matches!(
            (parent_function, child),
            ("or", Ast::Expression(ExpressionAst { function, .. })) if function == "and"
        ) || self.needs_bitwise_readability_parens(parent_function, child)
    }

    fn needs_bitwise_readability_parens(&self, parent_function: &str, child: &Ast) -> bool {
        let Ast::Expression(ExpressionAst { function, .. }) = child else {
            return false;
        };
        let Some((_, parent_prec)) = self.infix_operator(parent_function) else {
            return false;
        };
        let Some((_, child_prec)) = self.infix_operator(function) else {
            return false;
        };
        matches!(parent_function, "bitor" | "bitxor" | "bitand")
            && matches!(function.as_str(), "bitor" | "bitxor" | "bitand" | "shl" | "shr")
            && function != parent_function
            && child_prec > parent_prec
    }

    fn line_of_span(&self, span: &Span) -> usize {
        offset_to_line_col(self.source, span.start).line
    }

    fn end_line_of_span(&self, span: &Span) -> usize {
        let end_offset = if span.end > span.start { span.end - 1 } else { span.end };
        offset_to_line_col(self.source, end_offset).line
    }

    fn line_of_ast(&self, ast: &Ast) -> Option<usize> {
        match ast {
            Ast::FunctionDef(func) => func.span.as_ref().map(|span| self.line_of_span(span)),
            Ast::StructDef(def) => def.span.as_ref().map(|span| self.line_of_span(span)),
            Ast::MethodCall { span, .. } | Ast::FieldAccess { span, .. } => {
                span.as_ref().map(|span| self.line_of_span(span))
            }
            Ast::If { span, .. }
            | Ast::Assign { span, .. }
            | Ast::MultiAssign { span, .. }
            | Ast::Index { span, .. }
            | Ast::IndexAssign { span, .. } => span.as_ref().map(|span| self.line_of_span(span)),
            Ast::Variable(name) | Ast::FunctionRef(name) => {
                name.span.as_ref().map(|span| self.line_of_span(span))
            }
            Ast::Expression(ExpressionAst { function_span, .. }) => {
                function_span.as_ref().map(|span| self.line_of_span(span))
            }
            Ast::Block(block) => block.lines.first().and_then(|line| self.line_of_ast(line)),
            Ast::Lambda { body, .. } => self.line_of_ast(body),
            Ast::MultiValue(values) | Ast::ListLiteral(values) => {
                values.first().and_then(|value| self.line_of_ast(value))
            }
            Ast::MapLiteral(entries) => entries.first().and_then(|entry| match &entry.key {
                MapKeyAst::Dynamic(key) => {
                    self.line_of_ast(key).or_else(|| self.line_of_ast(&entry.value))
                }
                MapKeyAst::Static(_) => self.line_of_ast(&entry.value),
            }),
            Ast::StructLiteral { fields, .. } => {
                fields.first().and_then(|field| self.line_of_ast(&field.value))
            }
            Ast::Literal(_) => None,
        }
    }

    fn end_line_of_ast(&self, ast: &Ast) -> Option<usize> {
        match ast {
            Ast::FunctionDef(func) => func.span.as_ref().map(|span| self.end_line_of_span(span)),
            Ast::StructDef(def) => def.span.as_ref().map(|span| self.end_line_of_span(span)),
            Ast::MethodCall { receiver, args, span, .. } => args
                .last()
                .and_then(|arg| self.end_line_of_ast(arg))
                .or_else(|| self.end_line_of_ast(receiver))
                .or_else(|| span.as_ref().map(|span| self.end_line_of_span(span))),
            Ast::FieldAccess { base, span, .. } => self
                .end_line_of_ast(base)
                .or_else(|| span.as_ref().map(|span| self.end_line_of_span(span))),
            Ast::If { condition, then, else_, span } => else_
                .as_ref()
                .and_then(|else_block| self.end_line_of_ast(&Ast::Block(else_block.clone())))
                .or_else(|| self.end_line_of_ast(&Ast::Block(then.clone())))
                .or_else(|| self.end_line_of_ast(condition))
                .or_else(|| span.as_ref().map(|span| self.end_line_of_span(span))),
            Ast::Assign { span, .. }
            | Ast::MultiAssign { span, .. }
            | Ast::Index { span, .. }
            | Ast::IndexAssign { span, .. } => {
                span.as_ref().map(|span| self.end_line_of_span(span))
            }
            Ast::Variable(name) | Ast::FunctionRef(name) => {
                name.span.as_ref().map(|span| self.end_line_of_span(span))
            }
            Ast::Expression(ExpressionAst { function_span, .. }) => {
                function_span.as_ref().map(|span| self.end_line_of_span(span))
            }
            Ast::Block(block) => block.lines.last().and_then(|line| self.end_line_of_ast(line)),
            Ast::Lambda { body, .. } => self.end_line_of_ast(body),
            Ast::MultiValue(values) | Ast::ListLiteral(values) => {
                values.last().and_then(|value| self.end_line_of_ast(value))
            }
            Ast::MapLiteral(entries) => {
                entries.last().and_then(|entry| self.end_line_of_ast(&entry.value))
            }
            Ast::StructLiteral { fields, .. } => {
                fields.last().and_then(|field| self.end_line_of_ast(&field.value))
            }
            Ast::Literal(_) => None,
        }
    }

    fn take_standalone_comments_before(
        &mut self,
        target_line: usize,
        indent: usize,
    ) -> CommentBlock {
        let mut lines = vec![];
        let mut last_line = None;
        while let Some(comment) = self.comments.get(self.next_comment) {
            if comment.line >= target_line || comment.trailing {
                break;
            }
            lines.push(format!("{}{}", self.indent(indent), comment.text));
            last_line = Some(comment.line);
            self.next_comment += 1;
        }
        CommentBlock { lines, last_line }
    }

    fn take_standalone_comments_until(&mut self, end_line: usize, indent: usize) -> Vec<String> {
        let mut lines = vec![];
        while let Some(comment) = self.comments.get(self.next_comment) {
            if comment.line >= end_line || comment.trailing {
                break;
            }
            lines.push(format!("{}{}", self.indent(indent), comment.text));
            self.next_comment += 1;
        }
        lines
    }

    fn take_trailing_comment_for(&mut self, line: usize) -> Option<String> {
        if let Some(comment) = self.comments.get(self.next_comment)
            && comment.line == line
            && comment.trailing
        {
            self.next_comment += 1;
            return Some(comment.text.clone());
        }
        None
    }

    fn append_trailing_comment(&mut self, rendered: &mut String, line: Option<usize>) {
        if let Some(line) = line
            && let Some(comment) = self.take_trailing_comment_for(line)
        {
            rendered.push(' ');
            rendered.push_str(&comment);
        }
    }

    fn has_blank_line_between(&self, start_line: usize, end_line: usize) -> bool {
        if end_line <= start_line + 1 {
            return false;
        }
        self.source
            .lines()
            .enumerate()
            .skip(start_line)
            .take(end_line - start_line - 1)
            .any(|(_, line)| line.trim().is_empty())
    }

    fn with_expression_indent<T>(&mut self, indent: usize, f: impl FnOnce(&mut Self) -> T) -> T {
        let previous = self.expression_indent;
        self.expression_indent = indent;
        let result = f(self);
        self.expression_indent = previous;
        result
    }

    fn format_map_entry(&mut self, entry: &MapEntryAst, indent: usize) -> String {
        let key = match &entry.key {
            MapKeyAst::Static(key) => {
                if key.chars().next().is_some_and(|ch| ch == '_' || ch.is_ascii_alphabetic())
                    && key.chars().all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
                {
                    format!("{key}:")
                } else {
                    format!("\"{}\":", self.escape_string(key))
                }
            }
            MapKeyAst::Dynamic(key) => format!("{} =>", key.format_node(self, 0)),
        };
        let value = self.with_expression_indent(indent, |fmt| entry.value.format_node(fmt, 0));
        format!("{}{} {}", self.indent(indent), key, value)
    }

    fn format_map_literal(&mut self, entries: &[MapEntryAst]) -> String {
        if entries.is_empty() {
            return "{}".to_string();
        }
        let entry_indent = self.expression_indent + 1;
        let body = entries
            .iter()
            .map(|entry| self.format_map_entry(entry, entry_indent))
            .collect::<Vec<_>>()
            .join(",\n");
        format!("{{\n{body},\n{}}}", self.indent(self.expression_indent))
    }

    fn format_struct_literal(&mut self, type_name: &str, fields: &[StructFieldValueAst]) -> String {
        if fields.is_empty() {
            return format!("{type_name} {{}}");
        }
        let field_indent = self.expression_indent + 1;
        let body = fields
            .iter()
            .map(|field| {
                let value = self
                    .with_expression_indent(field_indent, |fmt| field.value.format_node(fmt, 0));
                format!("{}{}: {}", self.indent(field_indent), field.name, value)
            })
            .collect::<Vec<_>>()
            .join(",\n");
        format!("{type_name} {{\n{body},\n{}}}", self.indent(self.expression_indent))
    }

    fn format_list_literal_expr(&mut self, items: &[Ast]) -> String {
        let items =
            items.iter().map(|item| item.format_node(self, 0)).collect::<Vec<_>>().join(", ");
        format!("[{items}]")
    }

    fn format_method_call_expr(&mut self, receiver: &Ast, method: &str, args: &[Ast]) -> String {
        let args = args.iter().map(|arg| arg.format_node(self, 0)).collect::<Vec<_>>().join(", ");
        format!("{}.{}({args})", receiver.format_node(self, 10), method)
    }

    fn format_lambda(&mut self, inputs: &[String], body: &Ast) -> String {
        match body {
            Ast::Block(block) => {
                let body_indent = self.expression_indent + 1;
                let rendered_body = self.with_expression_indent(body_indent, |fmt| {
                    block.format_with_indent(fmt, body_indent)
                });
                format!(
                    "fn {} ->\n{}\n{}end",
                    inputs.join(", "),
                    rendered_body,
                    self.indent(self.expression_indent)
                )
            }
            _ => format!("fn {} -> {} end", inputs.join(", "), body.format_node(self, 0)),
        }
    }

    fn format_lambda_expr(&mut self, inputs: &[String], body: &Ast, parent_prec: u8) -> String {
        let rendered = self.format_lambda(inputs, body);
        if parent_prec > 0 { format!("({rendered})") } else { rendered }
    }

    fn format_not_expr(&mut self, args: &[Ast], parent_prec: u8) -> String {
        let rendered = format!("not {}", args[0].format_node(self, 9));
        if parent_prec > 9 { format!("({rendered})") } else { rendered }
    }

    fn format_binary_expr(
        &mut self,
        function: &str,
        args: &[Ast],
        parent_prec: u8,
    ) -> Option<String> {
        let Some((op, prec)) = self.infix_operator(function) else {
            return None;
        };
        let mut lhs = args[0].format_node(self, prec);
        let mut rhs = args[1].format_node(self, prec + 1);
        if self.needs_readability_parens(function, &args[0]) {
            lhs = format!("({lhs})");
        }
        if self.needs_readability_parens(function, &args[1]) {
            rhs = format!("({rhs})");
        }
        let rendered = format!("{lhs} {op} {rhs}");
        Some(if parent_prec > prec { format!("({rendered})") } else { rendered })
    }

    fn format_call_expr(&mut self, function: &str, args: &[Ast]) -> String {
        let args = args.iter().map(|arg| arg.format_node(self, 0)).collect::<Vec<_>>().join(", ");
        format!("{function}({args})")
    }

    fn format_expression_expr(&mut self, function: &str, args: &[Ast], parent_prec: u8) -> String {
        if function == "not" && args.len() == 1 {
            self.format_not_expr(args, parent_prec)
        } else if args.len() == 2 {
            self.format_binary_expr(function, args, parent_prec)
                .unwrap_or_else(|| self.format_call_expr(function, args))
        } else {
            self.format_call_expr(function, args)
        }
    }

    fn format_index_expr(&mut self, collection: &Ast, index: &Ast) -> String {
        format!("{}[{}]", collection.format_node(self, 10), index.format_node(self, 0))
    }

    fn format_multi_value_expr(&mut self, values: &[Ast]) -> String {
        values.iter().map(|value| value.format_node(self, 0)).collect::<Vec<_>>().join(", ")
    }

    fn format_if_expr(
        &mut self,
        condition: &Ast,
        then: &BlockAst,
        else_: Option<&BlockAst>,
        indent: usize,
    ) -> String {
        let mut out = String::new();
        let head = match self.config.block_style {
            BlockStyle::DoEnd => format!("if {} do\n", condition.format_node(self, 0)),
            BlockStyle::Python => format!("if {}:\n", condition.format_node(self, 0)),
        };
        out.push_str(&head);
        let then_end_line =
            else_.and_then(|else_block| self.else_header_line(then, else_block)).or_else(|| {
                self.end_line_of_ast(condition)
                    .or_else(|| self.end_line_of_ast(&Ast::Block(then.clone())))
            });
        out.push_str(&then.format_with_indent_until(self, indent + 1, then_end_line));
        if let Some(else_block) = else_ {
            let python_chain = if self.config.block_style == BlockStyle::Python {
                self.format_python_elif(else_block, indent)
            } else {
                None
            };
            let do_end_chain = if self.config.block_style == BlockStyle::DoEnd {
                self.format_do_end_else_if(else_block, indent)
            } else {
                None
            };

            match self.config.block_style {
                BlockStyle::Python if python_chain.is_some() => {
                    out.push('\n');
                    out.push_str(&python_chain.unwrap());
                }
                BlockStyle::DoEnd if do_end_chain.is_some() => {
                    out.push('\n');
                    out.push_str(&do_end_chain.unwrap());
                }
                BlockStyle::DoEnd => {
                    out.push('\n');
                    out.push_str(&format!("{}else\n", self.indent(indent)));
                    let else_end_line = self
                        .end_line_of_ast(condition)
                        .or_else(|| self.end_line_of_ast(&Ast::Block(else_block.clone())));
                    out.push_str(&else_block.format_with_indent_until(
                        self,
                        indent + 1,
                        else_end_line,
                    ));
                }
                BlockStyle::Python => {
                    out.push('\n');
                    out.push_str(&format!("{}else:\n", self.indent(indent)));
                    let else_end_line = self
                        .end_line_of_ast(condition)
                        .or_else(|| self.end_line_of_ast(&Ast::Block(else_block.clone())));
                    out.push_str(&else_block.format_with_indent_until(
                        self,
                        indent + 1,
                        else_end_line,
                    ));
                }
            }
        }
        if self.config.block_style == BlockStyle::DoEnd {
            out.push('\n');
            out.push_str(&format!("{}end", self.indent(indent)));
        }
        out
    }

    fn format_python_elif(&mut self, else_block: &BlockAst, indent: usize) -> Option<String> {
        if else_block.lines.len() != 1 {
            return None;
        }
        let Ast::If { condition, then, else_, .. } = &else_block.lines[0] else {
            return None;
        };

        let mut out = format!("{}elif {}:\n", self.indent(indent), condition.format_node(self, 0));
        let then_end_line = else_
            .as_ref()
            .and_then(|nested_else| self.else_header_line(then, nested_else))
            .or_else(|| {
                self.end_line_of_ast(condition)
                    .or_else(|| self.end_line_of_ast(&Ast::Block(then.clone())))
            });
        out.push_str(&then.format_with_indent_until(self, indent + 1, then_end_line));

        if let Some(nested_else) = else_.as_ref() {
            if let Some(chained) = self.format_python_elif(nested_else, indent) {
                out.push('\n');
                out.push_str(&chained);
            } else {
                out.push('\n');
                out.push_str(&format!("{}else:\n", self.indent(indent)));
                let else_end_line = self
                    .end_line_of_ast(condition)
                    .or_else(|| self.end_line_of_ast(&Ast::Block(nested_else.clone())));
                out.push_str(&nested_else.format_with_indent_until(
                    self,
                    indent + 1,
                    else_end_line,
                ));
            }
        }

        Some(out)
    }

    fn format_do_end_else_if(&mut self, else_block: &BlockAst, indent: usize) -> Option<String> {
        if else_block.lines.len() != 1 {
            return None;
        }
        let Ast::If { condition, then, else_, .. } = &else_block.lines[0] else {
            return None;
        };

        let mut out =
            format!("{}else if {} do\n", self.indent(indent), condition.format_node(self, 0));
        let then_end_line = else_
            .as_ref()
            .and_then(|nested_else| self.else_header_line(then, nested_else))
            .or_else(|| {
                self.end_line_of_ast(condition)
                    .or_else(|| self.end_line_of_ast(&Ast::Block(then.clone())))
            });
        out.push_str(&then.format_with_indent_until(self, indent + 1, then_end_line));

        if let Some(nested_else) = else_.as_ref() {
            if let Some(chained) = self.format_do_end_else_if(nested_else, indent) {
                out.push('\n');
                out.push_str(&chained);
            } else {
                out.push('\n');
                out.push_str(&format!("{}else\n", self.indent(indent)));
                let else_end_line = self
                    .end_line_of_ast(condition)
                    .or_else(|| self.end_line_of_ast(&Ast::Block(nested_else.clone())));
                out.push_str(&nested_else.format_with_indent_until(
                    self,
                    indent + 1,
                    else_end_line,
                ));
            }
        }

        Some(out)
    }

    fn else_header_line(&self, then: &BlockAst, else_block: &BlockAst) -> Option<usize> {
        let search_start = self.end_line_of_ast(&Ast::Block(then.clone()))?.saturating_add(1);
        let search_end = else_block.lines.first().and_then(|line| self.line_of_ast(line))?;
        for (index, line) in self.source.lines().enumerate() {
            let line_no = index + 1;
            if line_no < search_start || line_no > search_end {
                continue;
            }
            let trimmed = line.trim_start();
            if trimmed.starts_with("else") || trimmed.starts_with("elif") {
                return Some(line_no);
            }
        }
        Some(search_end)
    }
}

#[derive(Debug, Clone)]
struct LineComment {
    line: usize,
    text: String,
    trailing: bool,
}

fn find_comment_start(line: &str) -> Option<usize> {
    let mut in_string = false;
    let mut escaped = false;
    for (index, ch) in line.char_indices() {
        if escaped {
            escaped = false;
            continue;
        }
        match ch {
            '\\' if in_string => escaped = true,
            '"' => in_string = !in_string,
            '#' if !in_string => return Some(index),
            _ => {}
        }
    }
    None
}

fn extract_line_comments(source: &str) -> Vec<LineComment> {
    source
        .lines()
        .enumerate()
        .filter_map(|(index, line)| {
            let start = find_comment_start(line)?;
            let text = line[start..].trim_end().to_string();
            let trailing = !line[..start].trim().is_empty();
            Some(LineComment { line: index + 1, text, trailing })
        })
        .collect()
}

pub trait FormatNode {
    fn format_node(&self, fmt: &mut AstFormatter<'_>, parent_prec: u8) -> String;
}

impl FormatNode for LiteralAst {
    fn format_node(&self, fmt: &mut AstFormatter<'_>, _parent_prec: u8) -> String {
        match self {
            LiteralAst::Bool(true) => "true".to_string(),
            LiteralAst::Bool(false) => "false".to_string(),
            LiteralAst::Integer(value) => value.to_string(),
            LiteralAst::BigInt(value) => format!("{value}n"),
            LiteralAst::String(value) => format!("\"{}\"", fmt.escape_string(value)),
        }
    }
}

impl FormatNode for MapEntryAst {
    fn format_node(&self, fmt: &mut AstFormatter<'_>, parent_prec: u8) -> String {
        fmt.format_map_entry(self, parent_prec as usize)
    }
}

impl BlockAst {
    fn format_with_indent(&self, fmt: &mut AstFormatter<'_>, indent: usize) -> String {
        self.format_with_indent_until(fmt, indent, None)
    }

    fn format_with_indent_until(
        &self,
        fmt: &mut AstFormatter<'_>,
        indent: usize,
        end_line: Option<usize>,
    ) -> String {
        let mut rendered = vec![];
        let mut previous_end_line = None;

        for (index, line) in self.lines.iter().enumerate() {
            let is_last_line = index + 1 == self.lines.len();
            let source_line =
                fmt.line_of_ast(line).or_else(|| if is_last_line { end_line } else { None });
            if let (Some(prev_end), Some(line_no)) = (previous_end_line, source_line)
                && fmt.has_blank_line_between(prev_end, line_no)
            {
                rendered.push(String::new());
            }

            if let Some(line_no) = source_line {
                let comments = fmt.take_standalone_comments_before(line_no, indent);
                let preserve_blank_separator = comments
                    .last_line
                    .is_some_and(|last_line| fmt.has_blank_line_between(last_line, line_no));
                rendered.extend(comments.lines);
                if preserve_blank_separator {
                    rendered.push(String::new());
                }
            }

            let mut line_rendered = match line {
                Ast::Assign { name, value, .. } => fmt.with_expression_indent(indent, |fmt| {
                    format!("{}{} = {}", fmt.indent(indent), name, value.format_node(fmt, 0))
                }),
                Ast::MultiAssign { names, value, .. } => format!(
                    "{}{} = {}",
                    fmt.indent(indent),
                    names.join(", "),
                    fmt.with_expression_indent(indent, |fmt| value.format_node(fmt, 0))
                ),
                Ast::IndexAssign { collection, index, value, .. } => {
                    fmt.with_expression_indent(indent, |fmt| {
                        format!(
                            "{}{}[{}] = {}",
                            fmt.indent(indent),
                            collection.format_node(fmt, 10),
                            index.format_node(fmt, 0),
                            value.format_node(fmt, 0)
                        )
                    })
                }
                Ast::If { condition, then, else_, .. } => format!(
                    "{}{}",
                    fmt.indent(indent),
                    fmt.format_if_expr(condition, then, else_.as_ref(), indent)
                ),
                Ast::Block(block) => block.format_with_indent(fmt, indent),
                _ => format!("{}{}", fmt.indent(indent), line.format_node(fmt, 0)),
            };

            fmt.append_trailing_comment(&mut line_rendered, source_line);
            rendered.push(line_rendered);
            previous_end_line = fmt.end_line_of_ast(line);
        }

        rendered.extend(
            end_line
                .map(|line| fmt.take_standalone_comments_until(line, indent))
                .unwrap_or_default(),
        );
        rendered.join("\n")
    }
}

impl FormatNode for BlockAst {
    fn format_node(&self, fmt: &mut AstFormatter<'_>, _parent_prec: u8) -> String {
        self.format_with_indent(fmt, 0)
    }
}

impl FormatNode for FunctionDefAst {
    fn format_node(&self, fmt: &mut AstFormatter<'_>, _parent_prec: u8) -> String {
        let head = match fmt.config.block_style {
            BlockStyle::DoEnd => format!("fn {}({}) do", self.name, self.inputs.join(", ")),
            BlockStyle::Python => format!("fn {}({}):", self.name, self.inputs.join(", ")),
        };
        let mut head = head;
        fmt.append_trailing_comment(
            &mut head,
            self.span.as_ref().map(|span| fmt.line_of_span(span)),
        );
        let mut out = format!(
            "{}\n{}",
            head,
            self.block.format_with_indent_until(
                fmt,
                1,
                self.span.as_ref().map(|span| fmt.end_line_of_span(span)),
            )
        );
        if fmt.config.block_style == BlockStyle::DoEnd {
            out.push_str("\nend");
        }
        out
    }
}

impl FormatNode for StructDefAst {
    fn format_node(&self, fmt: &mut AstFormatter<'_>, _parent_prec: u8) -> String {
        let mut head = format!("struct {} = {{", self.name);
        fmt.append_trailing_comment(
            &mut head,
            self.span.as_ref().map(|span| fmt.line_of_span(span)),
        );
        if self.fields.is_empty() {
            return format!("{head}}}");
        }
        let body = self
            .fields
            .iter()
            .map(|field| format!("{}{}", fmt.indent(1), field))
            .collect::<Vec<_>>()
            .join(",\n");
        format!("{head}\n{body},\n}}")
    }
}

impl FormatNode for Ast {
    fn format_node(&self, fmt: &mut AstFormatter<'_>, parent_prec: u8) -> String {
        match self {
            Ast::Literal(literal) => literal.format_node(fmt, parent_prec),
            Ast::Variable(name) | Ast::FunctionRef(name) => name.to_string(),
            Ast::StructDef(def) => def.format_node(fmt, parent_prec),
            Ast::ListLiteral(items) => fmt.format_list_literal_expr(items),
            Ast::MapLiteral(entries) => fmt.format_map_literal(entries),
            Ast::StructLiteral { type_name, fields, .. } => {
                fmt.format_struct_literal(type_name.as_str(), fields)
            }
            Ast::MethodCall { receiver, method, args, .. } => {
                fmt.format_method_call_expr(receiver, method, args)
            }
            Ast::FieldAccess { base, field, .. } => {
                format!("{}.{}", base.format_node(fmt, 10), field)
            }
            Ast::Lambda { inputs, body } => fmt.format_lambda_expr(inputs, body, parent_prec),
            Ast::Expression(ExpressionAst { function, args, .. }) => {
                fmt.format_expression_expr(function, args, parent_prec)
            }
            Ast::Index { collection, index, .. } => fmt.format_index_expr(collection, index),
            Ast::If { condition, then, else_, .. } => {
                fmt.format_if_expr(condition, then, else_.as_ref(), 0)
            }
            Ast::MultiValue(values) => fmt.format_multi_value_expr(values),
            Ast::Block(block) => block.format_node(fmt, parent_prec),
            Ast::Assign { .. }
            | Ast::MultiAssign { .. }
            | Ast::IndexAssign { .. }
            | Ast::FunctionDef(_) => {
                unreachable!("statement AST should not be formatted as expression")
            }
        }
    }
}

fn parse_top_level_items(source: &str) -> Result<Vec<Ast>, CompileError> {
    let lex = Token::lexer(source);
    let mut lexer = ParseLexer::new(lex);
    let mut items = vec![];
    loop {
        while lexer.peek() == Some(&Ok(Token::Newline)) {
            lexer.next();
        }
        if lexer.peek().is_none() {
            break;
        }
        match Ast::from_lexer(&mut lexer) {
            Ok(item @ Ast::FunctionDef(_)) | Ok(item @ Ast::StructDef(_)) => items.push(item),
            Ok(_) => return Err(CompileError::TopLevelExpression),
            Err(err) => {
                return Err(CompileError::Parse { message: err.to_string(), span: Some(err.span) });
            }
        }
    }
    Ok(items)
}

pub fn format_source(source: &str, config: &FormatConfig) -> Result<String, CompileError> {
    let items = parse_top_level_items(source)?;
    let mut fmt = AstFormatter::new(config, source);
    let mut rendered = String::new();
    let mut wrote_item = false;
    let mut previous_end_line = None;
    for item in &items {
        let item_span = match item {
            Ast::FunctionDef(func) => func.span.as_ref(),
            Ast::StructDef(def) => def.span.as_ref(),
            _ => None,
        };
        let item_start_line = item_span.map(|span| fmt.line_of_span(span));
        let comments = item_span
            .as_ref()
            .map(|span| fmt.line_of_span(span))
            .map(|line| (line, fmt.take_standalone_comments_before(line, 0)));

        if wrote_item {
            rendered.push('\n');
            if comments.as_ref().is_none_or(|(_, block)| block.lines.is_empty()) {
                rendered.push('\n');
            }
        } else if !rendered.is_empty()
            && comments.as_ref().is_none_or(|(_, block)| block.lines.is_empty())
        {
            rendered.push('\n');
        }

        if let (Some(prev_end), Some(line_no)) = (previous_end_line, item_start_line)
            && comments.as_ref().is_none_or(|(_, block)| block.lines.is_empty())
            && fmt.has_blank_line_between(prev_end, line_no)
            && !rendered.ends_with("\n\n")
        {
            rendered.push('\n');
        }

        if let Some((line_no, comments)) = comments {
            for (index, comment) in comments.lines.iter().enumerate() {
                if !rendered.is_empty() && index > 0 {
                    rendered.push('\n');
                }
                rendered.push_str(comment);
                rendered.push('\n');
            }
            if comments
                .last_line
                .is_some_and(|last_line| fmt.has_blank_line_between(last_line, line_no))
            {
                rendered.push('\n');
            }
        }
        match item {
            Ast::FunctionDef(func) => rendered.push_str(&func.format_node(&mut fmt, 0)),
            Ast::StructDef(def) => rendered.push_str(&def.format_node(&mut fmt, 0)),
            _ => unreachable!("top-level formatter only accepts functions and structs"),
        }
        wrote_item = true;
        previous_end_line = item_span.map(|span| fmt.end_line_of_span(span));
    }
    for comment in fmt.take_standalone_comments_until(usize::MAX, 0) {
        if !rendered.is_empty() {
            rendered.push('\n');
        }
        rendered.push_str(&comment);
    }
    if config.final_newline {
        rendered.push('\n');
    }
    Ok(rendered)
}

#[cfg(test)]
mod tests {
    use super::{BlockStyle, FormatConfig, format_source};

    fn config(style: BlockStyle) -> FormatConfig {
        FormatConfig { block_style: style, ..FormatConfig::default() }
    }

    #[test]
    fn inferred_from_source_uses_first_block_style_in_mixed_file() {
        let source = "fn inner_fib(a, b, i, n, do_print):\n    if i + 1 == n:\n        if do_print do\n            print(a)\n        end\n        b\n";
        let config = FormatConfig::inferred_from_source(source);
        assert_eq!(config.block_style, BlockStyle::Python);
    }

    #[test]
    fn format_source_preserves_top_level_and_function_comments() {
        let source = "# file comment\nfn main() do # function comment\n    # before assignment\n    value = 1 # trailing assign\n    # after assignment\nend\n";
        let formatted = format_source(source, &config(BlockStyle::DoEnd)).unwrap();
        assert_eq!(
            formatted,
            "# file comment\nfn main() do # function comment\n    # before assignment\n    value = 1 # trailing assign\n    # after assignment\nend\n"
        );
    }

    #[test]
    fn format_source_preserves_blank_line_after_leading_comment() {
        let source = "# Python-style FizzBuzz using recursion for iteration.\n\nfn fizzbuzz_one(x):\n    if x % 15 == 0:\n        print(\"fizzbuzz\")\n";
        let formatted = format_source(source, &config(BlockStyle::Python)).unwrap();
        assert_eq!(
            formatted,
            "# Python-style FizzBuzz using recursion for iteration.\n\nfn fizzbuzz_one(x):\n    if x % 15 == 0:\n        print(\"fizzbuzz\")\n"
        );
    }

    #[test]
    fn format_source_preserves_comments_inside_if_blocks() {
        let source = "fn main() do\n    if 1 do\n        # then comment\n        print(1) # trailing then\n    else\n        # else comment\n        print(0)\n    end\nend\n";
        let formatted = format_source(source, &config(BlockStyle::DoEnd)).unwrap();
        assert_eq!(
            formatted,
            "fn main() do\n    if 1 do\n        # then comment\n        print(1) # trailing then\n    else\n        # else comment\n        print(0)\n    end\nend\n"
        );
    }

    #[test]
    fn format_source_preserves_python_style_comments() {
        let source = "# file comment\nfn main(): # function comment\n    # before assignment\n    value = 1 # trailing assign\n";
        let formatted = format_source(source, &config(BlockStyle::Python)).unwrap();
        assert_eq!(
            formatted,
            "# file comment\nfn main(): # function comment\n    # before assignment\n    value = 1 # trailing assign\n"
        );
    }

    #[test]
    fn format_source_preserves_python_elif_chain() {
        let source = "fn main():\n    if x % 15 == 0:\n        print(\"fizzbuzz\")\n    elif x % 3 == 0:\n        print(\"fizz\")\n    elif x % 5 == 0:\n        print(\"buzz\")\n    else:\n        print(x)\n";
        let formatted = format_source(source, &config(BlockStyle::Python)).unwrap();
        assert_eq!(
            formatted,
            "fn main():\n    if x % 15 == 0:\n        print(\"fizzbuzz\")\n    elif x % 3 == 0:\n        print(\"fizz\")\n    elif x % 5 == 0:\n        print(\"buzz\")\n    else:\n        print(x)\n"
        );
    }

    #[test]
    fn format_source_preserves_do_end_else_if_chain() {
        let source = "fn main() do\n    if x % 15 == 0 do\n        print(\"fizzbuzz\")\n    else if x % 3 == 0 do\n        print(\"fizz\")\n    else if x % 5 == 0 do\n        print(\"buzz\")\n    else\n        print(x)\n    end\nend\n";
        let formatted = format_source(source, &config(BlockStyle::DoEnd)).unwrap();
        assert_eq!(
            formatted,
            "fn main() do\n    if x % 15 == 0 do\n        print(\"fizzbuzz\")\n    else if x % 3 == 0 do\n        print(\"fizz\")\n    else if x % 5 == 0 do\n        print(\"buzz\")\n    else\n        print(x)\n    end\nend\n"
        );
    }

    #[test]
    fn format_source_indents_nested_map_literals() {
        let source = "fn main() do\n    value = {\n        outer: {\n            inner: 1,\n        },\n    }\nend\n";
        let formatted = format_source(source, &config(BlockStyle::DoEnd)).unwrap();
        assert_eq!(
            formatted,
            "fn main() do\n    value = {\n        outer: {\n            inner: 1,\n        },\n    }\nend\n"
        );
    }

    #[test]
    fn format_source_indents_multiline_inline_functions() {
        let source = "fn main() do\n    ops = {\n        \"+\": fn lhs, rhs ->\n            if lhs > rhs do\n                lhs\n            else\n                rhs\n            end\n        end,\n    }\n    ops\nend\n";
        let formatted = format_source(source, &config(BlockStyle::DoEnd)).unwrap();
        assert_eq!(
            formatted,
            "fn main() do\n    ops = {\n        \"+\": fn lhs, rhs ->\n            if lhs > rhs do\n                lhs\n            else\n                rhs\n            end\n        end,\n    }\n    ops\nend\n"
        );
    }

    #[test]
    fn format_source_preserves_readable_boolean_grouping() {
        let source = "fn main() do\n    print((key_pop2 == \"name\" and value_pop2 == \"expr-compiler\") or (key_pop2 == \"year\" and value_pop2 == 2027))\nend\n";
        let formatted = format_source(source, &config(BlockStyle::DoEnd)).unwrap();
        assert_eq!(
            formatted,
            "fn main() do\n    print((key_pop2 == \"name\" and value_pop2 == \"expr-compiler\") or (key_pop2 == \"year\" and value_pop2 == 2027))\nend\n"
        );
    }

    #[test]
    fn format_source_preserves_readable_bitwise_grouping() {
        let source = "fn xorshift32(x) do\n    mask = 2147483647\n    x1 = (x ^ (x << 13)) & mask\n    x2 = (x1 ^ (x1 >> 17)) & mask\n    (x2 ^ (x2 << 5)) & mask\nend\n";
        let formatted = format_source(source, &config(BlockStyle::DoEnd)).unwrap();
        assert_eq!(
            formatted,
            "fn xorshift32(x) do\n    mask = 2147483647\n    x1 = (x ^ (x << 13)) & mask\n    x2 = (x1 ^ (x1 >> 17)) & mask\n    (x2 ^ (x2 << 5)) & mask\nend\n"
        );
    }

    #[test]
    fn format_source_preserves_comments_between_functions() {
        let source = "fn first() do\n    1\nend\n# between functions\nfn second() do\n    2\nend\n";
        let formatted = format_source(source, &config(BlockStyle::DoEnd)).unwrap();
        assert_eq!(
            formatted,
            "fn first() do\n    1\nend\n# between functions\nfn second() do\n    2\nend\n"
        );
    }

    #[test]
    fn format_source_preserves_blank_line_between_statements() {
        let source = "fn main() do\n    first = 1\n\n    second = 2\nend\n";
        let formatted = format_source(source, &config(BlockStyle::DoEnd)).unwrap();
        assert_eq!(formatted, "fn main() do\n    first = 1\n\n    second = 2\nend\n");
    }

    #[test]
    fn format_source_preserves_blank_line_after_python_if_block() {
        let source = "fn main():\n    if not string_is_integer(\"11T234\"):\n        print(\"not integer :(\")\n\n    ok, value, err = string_try_first(\"\")\n";
        let formatted = format_source(source, &config(BlockStyle::Python)).unwrap();
        assert_eq!(
            formatted,
            "fn main():\n    if not string_is_integer(\"11T234\"):\n        print(\"not integer :(\")\n\n    ok, value, err = string_try_first(\"\")\n"
        );
    }

    #[test]
    fn format_source_preserves_blank_line_before_last_literal_statement() {
        let source = "fn main():\n    print(ys[3])\n\n    0\n";
        let formatted = format_source(source, &config(BlockStyle::Python)).unwrap();
        assert_eq!(formatted, "fn main():\n    print(ys[3])\n\n    0\n");
    }

    #[test]
    fn format_source_preserves_blank_line_between_functions() {
        let source = "fn first() do\n    1\nend\n\nfn second() do\n    2\nend\n";
        let formatted = format_source(source, &config(BlockStyle::DoEnd)).unwrap();
        assert_eq!(formatted, "fn first() do\n    1\nend\n\nfn second() do\n    2\nend\n");
    }

    #[test]
    fn format_source_preserves_struct_syntax() {
        let source = "struct Person = {\n    name,\n    age,\n}\n";
        let formatted = format_source(source, &config(BlockStyle::DoEnd)).unwrap();
        assert_eq!(formatted, source);
    }
}
