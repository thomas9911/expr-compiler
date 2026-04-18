use crate::parser::{Ast, BlockAst, ExpressionAst, FunctionDefAst, LiteralAst};
use cranelift::codegen::{ir::UserFuncName, verify_function};
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{FuncId, Linkage, Module as CraneliftModule, default_libcall_names};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::{isa::OwnedTargetIsa, settings, *};
use std::collections::HashMap;

pub struct Module {
    pub functions: Vec<FunctionDefAst>,
}

impl Module {
    pub fn new() -> Self {
        Module { functions: vec![] }
    }

    pub fn add_function(&mut self, func: FunctionDefAst) {
        self.functions.push(func);
    }

    pub fn from_ast(ast: Ast) -> Self {
        let mut module = Module::new();
        match ast {
            Ast::FunctionDef(func) => module.functions.push(func),
            Ast::Block(block) => {
                for line in block.lines {
                    if let Ast::FunctionDef(func) = line {
                        module.functions.push(func);
                    }
                }
            }
            _ => {}
        }
        module
    }

    pub fn compile_to_jit(self) -> JitModule {
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift::native::builder()
            .expect("host machine supported")
            .finish(flags.clone())
            .unwrap();

        let mut cranelift_module =
            JITModule::new(JITBuilder::with_isa(isa.clone(), default_libcall_names()));

        let mut func_ids = HashMap::new();
        for func_def in &self.functions {
            let id = add_function_to_module(&mut cranelift_module, isa.clone(), &flags, func_def);
            func_ids.insert(func_def.name.clone(), id);
        }

        cranelift_module.finalize_definitions().unwrap();

        JitModule {
            module: cranelift_module,
            func_ids,
        }
    }

    pub fn compile_to_object(self, name: &str) -> Vec<u8> {
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift::native::builder()
            .expect("host machine supported")
            .finish(flags.clone())
            .unwrap();

        let mut cranelift_module = ObjectModule::new(
            ObjectBuilder::new(isa.clone(), name, default_libcall_names()).unwrap(),
        );

        for func_def in &self.functions {
            add_function_to_module(&mut cranelift_module, isa.clone(), &flags, func_def);
        }
        cranelift_module.finish().emit().unwrap()
    }
}

pub struct JitModule {
    module: JITModule,
    func_ids: HashMap<String, FuncId>,
}

impl JitModule {
    pub fn get_fn_ptr(&self, name: &str) -> *const u8 {
        self.module.get_finalized_function(self.func_ids[name])
    }
}

fn add_function_to_module(
    module: &mut impl CraneliftModule,
    isa: OwnedTargetIsa,
    flags: &settings::Flags,
    func_def: &FunctionDefAst,
) -> FuncId {
    let mut sig = Signature::new(isa.default_call_conv());
    sig.returns.push(AbiParam::new(types::I64));
    for _ in &func_def.inputs {
        sig.params.push(AbiParam::new(types::I64));
    }

    let func_id = module
        .declare_function(&func_def.name, Linkage::Export, &sig)
        .unwrap();

    let mut ctx = module.make_context();
    ctx.func.signature = sig;
    ctx.func.name = UserFuncName::user(0, func_id.as_u32());

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
        let block0 = builder.create_block();
        builder.append_block_params_for_function_params(block0);
        builder.switch_to_block(block0);
        builder.seal_block(block0);

        let params: Vec<(String, Variable)> = func_def
            .inputs
            .iter()
            .enumerate()
            .map(|(i, name)| {
                let var = builder.declare_var(types::I64);
                let param_val = builder.block_params(block0)[i];
                builder.def_var(var, param_val);
                (name.clone(), var)
            })
            .collect();

        let mut last_val = None;
        for line in &func_def.block.lines {
            last_val = Some(compile_ast(&mut builder, line, &params));
        }

        if let Some(val) = last_val {
            builder.ins().return_(&[val]);
        }

        builder.finalize();
    }

    let res = verify_function(&ctx.func, flags);
    if let Err(errors) = res {
        panic!("{}", errors);
    }

    module.define_function(func_id, &mut ctx).unwrap();
    module.clear_context(&mut ctx);
    func_id
}

fn compile_ast(
    builder: &mut FunctionBuilder,
    ast: &Ast,
    params: &[(String, Variable)],
) -> cranelift::prelude::Value {
    match ast {
        Ast::Literal(LiteralAst::Integer(n)) => builder.ins().iconst(types::I64, *n),
        Ast::Expression(ExpressionAst { function, args }) => {
            let compiled: Vec<_> = args
                .iter()
                .map(|arg| compile_ast(builder, arg, params))
                .collect();
            match function.as_str() {
                "add" => builder.ins().iadd(compiled[0], compiled[1]),
                "subtract" => builder.ins().isub(compiled[0], compiled[1]),
                _ => unimplemented!("unknown function: {function}"),
            }
        }
        Ast::Block(block) => {
            let mut last = None;
            for line in &block.lines {
                last = Some(compile_ast(builder, line, params));
            }
            last.expect("empty block")
        }
        Ast::Variable(name) => {
            let (_, var) = params
                .iter()
                .find(|(n, _)| n == name)
                .unwrap_or_else(|| panic!("undefined variable: {name}"));
            builder.use_var(*var)
        }
        Ast::FunctionDef(_) => unimplemented!("nested function definitions"),
    }
}

#[test]
fn text_to_native_execute() {
    use crate::parser::ParseLexer;
    use crate::tokenizer::{self, Logos};

    let src = "fn main() do\n    7 + 5 - 4 \nend";
    let lex = tokenizer::Token::lexer(src);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let jit = Module::from_ast(ast).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };

    assert_eq!(func(), 8);
}

#[test]
fn text_to_native_execute_with_params() {
    use crate::parser::ParseLexer;
    use crate::tokenizer::{self, Logos};

    let src = "fn add(x, y) do\n    x + y\nend";
    let lex = tokenizer::Token::lexer(src);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let jit = Module::from_ast(ast).compile_to_jit();
    let ptr = jit.get_fn_ptr("add");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn(i64, i64) -> i64>(ptr) };

    assert_eq!(func(3, 5), 8);
    assert_eq!(func(10, -4), 6);
}

#[test]
fn compile_parsed_function() {
    use crate::parser::ParseLexer;
    use crate::tokenizer::{self, Logos};

    let src = "fn main() do\n    1 + 2 - 3\nend";
    let lex = tokenizer::Token::lexer(src);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let module = Module::from_ast(ast);
    assert_eq!(module.functions.len(), 1);
    assert_eq!(module.functions[0].name, "main");

    let bytes = module.compile_to_object("test_module");
    assert!(!bytes.is_empty());
}
