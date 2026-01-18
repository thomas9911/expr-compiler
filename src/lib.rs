pub mod compiler;
pub mod interpreter;
pub mod parser;
pub mod tokenizer;
// pub mod instruction;

#[cfg(test)]
use crate::{
    interpreter::Interpreter,
    parser::{Ast, LiteralAst, ParseLexer},
};
#[cfg(test)]
use logos::Logos;

#[test]
fn execute_test() {
    let lex = tokenizer::Token::lexer("1 + 5 - 2 + 7 - 6 - 2");
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let mut interpreter = Interpreter::default();

    interpreter.execute(ast);
    let output = interpreter.get_output();

    assert_eq!(Ast::Literal(LiteralAst::Integer(3)), *output);
}
