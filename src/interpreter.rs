use crate::parser::{Ast, ExpressionAst, LiteralAst};

#[derive(Debug)]
pub struct Interpreter {
    output: Ast,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            output: Ast::Literal(LiteralAst::Integer(0)),
        }
    }
}

impl Interpreter {
    pub fn execute(&mut self, ast: Ast) {
        match ast {
            Ast::Expression(expr) => {
                self.execute_expression(expr);
            }
            Ast::Literal(literal_ast) => self.execute_literal(literal_ast),
        }
    }

    fn execute_expression(&mut self, expression: ExpressionAst) {
        match expression.function.as_ref() {
            "add" => {
                let mut sum = 0i64;
                for arg in expression.args {
                    self.execute(arg);
                    match self.get_output() {
                        Ast::Literal(LiteralAst::Integer(integer)) => {
                            sum = sum.wrapping_add(*integer)
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
                                sum = sum.wrapping_sub(*integer);
                            }
                        }
                        _ => {
                            eprintln!("sum expects integer arguments")
                        }
                    }
                }
                self.execute_literal(LiteralAst::Integer(sum));
            }
            x => {
                eprintln!("function '{}' not supported", x)
            }
        }
    }

    fn execute_literal(&mut self, literal: LiteralAst) {
        self.output = Ast::Literal(literal);
    }

    pub fn get_output(&mut self) -> &Ast {
        &self.output
    }
}
