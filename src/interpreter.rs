use crate::parser::{Ast, ExpressionAst, FunctionDefAst, LiteralAst};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Interpreter {
    output: Ast,
    functions: HashMap<String, FunctionDefAst>,
    scopes: Vec<HashMap<String, Ast>>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            output: Ast::Literal(LiteralAst::Integer(0)),
            functions: HashMap::new(),
            scopes: vec![HashMap::new()],
        }
    }
}

impl Interpreter {
    pub fn execute(&mut self, ast: Ast) {
        match ast {
            Ast::Block(block) => {
                if block.lines.is_empty() {
                    self.execute_literal(LiteralAst::Integer(0));
                } else {
                    for line in block.lines {
                        self.execute(line);
                    }
                }
            }
            Ast::FunctionDef(function) => {
                let name = function.name.clone();
                self.functions.insert(name, function);
                self.execute_literal(LiteralAst::Integer(0));
            }
            Ast::Expression(expr) => {
                self.execute_expression(expr);
            }
            Ast::Literal(literal_ast) => self.execute_literal(literal_ast),
            Ast::ListLiteral(_) => {
                panic!("list literals are not supported by the interpreter");
            }
            Ast::Index { .. } => {
                panic!("index expressions are not supported by the interpreter");
            }
            Ast::IndexAssign { .. } => {
                panic!("index assignments are not supported by the interpreter");
            }
            Ast::Variable(name) => {
                if let Some(value) = self.lookup_variable(&name).cloned() {
                    self.output = value;
                } else {
                    eprintln!("variable '{name}' is not defined");
                    self.execute_literal(LiteralAst::Integer(0));
                }
            }
            Ast::Assign { name, value } => {
                self.execute(*value);
                let assigned = self.output.clone();
                if let Some(scope) = self.scopes.last_mut() {
                    scope.insert(name, assigned.clone());
                }
                self.output = assigned;
            }
            Ast::If {
                condition,
                then,
                else_,
            } => {
                self.execute(*condition);
                if self.output_is_truthy() {
                    self.execute(Ast::Block(then));
                } else if let Some(else_block) = else_ {
                    self.execute(Ast::Block(else_block));
                } else {
                    self.execute_literal(LiteralAst::Integer(0));
                }
            }
        }
    }

    fn execute_expression(&mut self, expression: ExpressionAst) {
        if expression.function.is_empty() {
            if let Some(first_arg) = expression.args.into_iter().next() {
                self.execute(first_arg);
            }
            return;
        }

        match expression.function.as_ref() {
            "add" => {
                let mut sum = 0i64;
                for arg in expression.args {
                    self.execute(arg);
                    match self.get_output() {
                        Ast::Literal(LiteralAst::Integer(integer)) => {
                            sum = sum
                                .checked_add(*integer)
                                .unwrap_or_else(|| panic!("integer overflow in add"));
                        }
                        _ => {
                            eprintln!("sum expects integer arguments")
                        }
                    }
                }
                self.execute_literal(LiteralAst::Integer(sum));
            }
            "subtract" => {
                let mut sum = 0;
                let mut first = true;
                for arg in expression.args {
                    self.execute(arg);
                    match self.get_output() {
                        Ast::Literal(LiteralAst::Integer(integer)) => {
                            if first {
                                sum = *integer;
                                first = false;
                            } else {
                                sum = sum
                                    .checked_sub(*integer)
                                    .unwrap_or_else(|| panic!("integer overflow in subtract"));
                            }
                        }
                        _ => {
                            eprintln!("sum expects integer arguments")
                        }
                    }
                }
                self.execute_literal(LiteralAst::Integer(sum));
            }
            "multiply" => {
                let mut product = 1i64;
                for arg in expression.args {
                    self.execute(arg);
                    match self.get_output() {
                        Ast::Literal(LiteralAst::Integer(integer)) => {
                            product = product
                                .checked_mul(*integer)
                                .unwrap_or_else(|| panic!("integer overflow in multiply"));
                        }
                        _ => {
                            eprintln!("multiply expects integer arguments")
                        }
                    }
                }
                self.execute_literal(LiteralAst::Integer(product));
            }
            "divide" => {
                let mut quotient = 0i64;
                let mut first = true;
                for arg in expression.args {
                    self.execute(arg);
                    match self.get_output() {
                        Ast::Literal(LiteralAst::Integer(integer)) => {
                            if first {
                                quotient = *integer;
                                first = false;
                            } else if *integer == 0 {
                                eprintln!("divide by zero");
                                self.execute_literal(LiteralAst::Integer(0));
                                return;
                            } else {
                                quotient = quotient
                                    .checked_div(*integer)
                                    .unwrap_or_else(|| panic!("integer overflow in divide"));
                            }
                        }
                        _ => {
                            eprintln!("divide expects integer arguments")
                        }
                    }
                }
                self.execute_literal(LiteralAst::Integer(quotient));
            }
            "modulo" => {
                let mut remainder = 0i64;
                let mut first = true;
                for arg in expression.args {
                    self.execute(arg);
                    match self.get_output() {
                        Ast::Literal(LiteralAst::Integer(integer)) => {
                            if first {
                                remainder = *integer;
                                first = false;
                            } else if *integer == 0 {
                                eprintln!("modulo by zero");
                                self.execute_literal(LiteralAst::Integer(0));
                                return;
                            } else {
                                remainder = remainder
                                    .checked_rem(*integer)
                                    .unwrap_or_else(|| panic!("integer overflow in modulo"));
                            }
                        }
                        _ => {
                            eprintln!("modulo expects integer arguments")
                        }
                    }
                }
                self.execute_literal(LiteralAst::Integer(remainder));
            }
            "gt" => self.execute_comparison(expression.args, |x, y| x > y),
            "lt" => self.execute_comparison(expression.args, |x, y| x < y),
            "gte" => self.execute_comparison(expression.args, |x, y| x >= y),
            "lte" => self.execute_comparison(expression.args, |x, y| x <= y),
            "eq" => self.execute_comparison(expression.args, |x, y| x == y),
            "ne" => self.execute_comparison(expression.args, |x, y| x != y),
            x => {
                if let Some(function_def) = self.functions.get(x).cloned() {
                    if function_def.inputs.len() != expression.args.len() {
                        eprintln!(
                            "function '{}' expects {} args, got {}",
                            x,
                            function_def.inputs.len(),
                            expression.args.len()
                        );
                        self.execute_literal(LiteralAst::Integer(0));
                        return;
                    }

                    let mut values = vec![];
                    for arg in expression.args {
                        self.execute(arg);
                        values.push(self.output.clone());
                    }

                    let mut scope = HashMap::new();
                    for (name, value) in function_def.inputs.iter().zip(values) {
                        scope.insert(name.clone(), value);
                    }
                    self.scopes.push(scope);
                    self.execute(Ast::Block(function_def.block));
                    self.scopes.pop();
                } else {
                    eprintln!("function '{}' not supported", x);
                    self.execute_literal(LiteralAst::Integer(0));
                }
            }
        }
    }

    fn execute_comparison<F>(&mut self, args: Vec<Ast>, cmp: F)
    where
        F: Fn(i64, i64) -> bool,
    {
        if args.len() != 2 {
            eprintln!("comparison expects 2 arguments");
            self.execute_literal(LiteralAst::Integer(0));
            return;
        }

        let left = self.evaluate_to_integer(args[0].clone());
        let right = self.evaluate_to_integer(args[1].clone());
        let value = i64::from(cmp(left, right));
        self.execute_literal(LiteralAst::Integer(value));
    }

    fn evaluate_to_integer(&mut self, ast: Ast) -> i64 {
        self.execute(ast);
        match self.get_output() {
            Ast::Literal(LiteralAst::Integer(integer)) => *integer,
            _ => 0,
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<&Ast> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    fn output_is_truthy(&self) -> bool {
        match &self.output {
            Ast::Literal(LiteralAst::Integer(value)) => *value != 0,
            _ => false,
        }
    }

    fn execute_literal(&mut self, literal: LiteralAst) {
        self.output = Ast::Literal(literal);
    }

    pub fn get_output(&mut self) -> &Ast {
        &self.output
    }
}

#[cfg(test)]
mod tests {
    use super::Interpreter;
    use crate::parser::{Ast, BlockAst, ExpressionAst, FunctionDefAst, LiteralAst};

    #[test]
    fn executes_block_and_assignment() {
        let mut interpreter = Interpreter::default();
        interpreter.execute(Ast::Block(BlockAst {
            lines: vec![
                Ast::Assign {
                    name: "x".to_string(),
                    value: Box::new(Ast::Literal(LiteralAst::Integer(10))),
                },
                Ast::Expression(ExpressionAst {
                    function: "add".to_string(),
                    args: vec![
                        Ast::Variable("x".to_string()),
                        Ast::Literal(LiteralAst::Integer(5)),
                    ],
                }),
            ],
        }));

        assert_eq!(
            *interpreter.get_output(),
            Ast::Literal(LiteralAst::Integer(15))
        );
    }

    #[test]
    fn executes_if_with_else() {
        let mut interpreter = Interpreter::default();
        interpreter.execute(Ast::If {
            condition: Box::new(Ast::Expression(ExpressionAst {
                function: "gt".to_string(),
                args: vec![
                    Ast::Literal(LiteralAst::Integer(3)),
                    Ast::Literal(LiteralAst::Integer(5)),
                ],
            })),
            then: BlockAst {
                lines: vec![Ast::Literal(LiteralAst::Integer(1))],
            },
            else_: Some(BlockAst {
                lines: vec![Ast::Literal(LiteralAst::Integer(99))],
            }),
        });

        assert_eq!(
            *interpreter.get_output(),
            Ast::Literal(LiteralAst::Integer(99))
        );
    }

    #[test]
    fn executes_user_defined_function() {
        let mut interpreter = Interpreter::default();
        interpreter.execute(Ast::FunctionDef(FunctionDefAst {
            name: "double".to_string(),
            inputs: vec!["x".to_string()],
            output: None,
            block: BlockAst {
                lines: vec![Ast::Expression(ExpressionAst {
                    function: "add".to_string(),
                    args: vec![
                        Ast::Variable("x".to_string()),
                        Ast::Variable("x".to_string()),
                    ],
                })],
            },
        }));

        interpreter.execute(Ast::Expression(ExpressionAst {
            function: "double".to_string(),
            args: vec![Ast::Literal(LiteralAst::Integer(21))],
        }));

        assert_eq!(
            *interpreter.get_output(),
            Ast::Literal(LiteralAst::Integer(42))
        );
    }

    #[test]
    #[should_panic(expected = "integer overflow in add")]
    fn add_overflow_panics() {
        let mut interpreter = Interpreter::default();
        interpreter.execute(Ast::Expression(ExpressionAst {
            function: "add".to_string(),
            args: vec![
                Ast::Literal(LiteralAst::Integer(i64::MAX)),
                Ast::Literal(LiteralAst::Integer(1)),
            ],
        }));
    }
}
