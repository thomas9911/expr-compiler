use crate::parser::{Ast, ExpressionAst, LiteralAst};

type Variable = String;

enum Instruction {
    Assign{variable: Variable, arg: Value},
    Buildin{function: String, args: Vec<Value>},
    Call{function: String, args: Vec<Value>},
    ExternCall{function: String, args: Vec<Value>},
}

enum Value {
    Literal(ValueLiteral),
    Variable(Variable)
}

/// or something
enum Variable_ {
    Register(usize),
    Stack(usize),
    Heap(usize)
}

enum ValueLiteral {
    Integer(i64)
}

pub struct Converter {

}

impl Converter {
    pub fn new() -> Self {
        Converter {  }
    }
    
    pub fn ast_to_instructions(&mut self, ast: &Ast, instructions: &mut Vec<Instruction>) {
    
    }
}


#[test]
fn xd() {
    let ast = Ast::Expression(ExpressionAst{function: "add".to_string(), args: vec![Ast::Literal(LiteralAst::Integer(3)), Ast::Literal(LiteralAst::Integer(9))]});

    let mut buffer = Vec::new();

    let mut converter = Converter::new();
    converter.ast_to_instructions(&ast, &mut buffer);

    let expected = vec![
        Instruction::Assign { variable: "tmp1".to_string(), arg: Value::Literal(ValueLiteral::Integer(3)) },
        Instruction::Assign { variable: "tmp2".to_string(), arg: Value::Literal(ValueLiteral::Integer(9)) },
        Instruction::Buildin { function: "add".to_string(), args: vec![Value::Variable("tmp1".to_string()), Value::Variable("tmp2".to_string())] }
    ];
    // assert_eq!()

}