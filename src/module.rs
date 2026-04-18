use crate::parser::{Ast, BlockAst, ExpressionAst, FunctionDefAst, LiteralAst};
use cranelift::codegen::{ir::UserFuncName, verify_function};
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{FuncId, Linkage, Module as CraneliftModule, default_libcall_names};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::{isa::OwnedTargetIsa, settings, *};
use std::collections::HashMap;
use std::path::Path;
use std::process::Command;

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
            let id = add_function_to_module(&mut cranelift_module, isa.clone(), &flags, func_def, &func_def.name, Linkage::Export);
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
            add_function_to_module(&mut cranelift_module, isa.clone(), &flags, func_def, &func_def.name, Linkage::Export);
        }
        cranelift_module.finish().emit().unwrap()
    }

    pub fn compile_to_executable(self, output: &Path) {
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift::native::builder()
            .expect("host machine supported")
            .finish(flags.clone())
            .unwrap();

        let mut cranelift_module = ObjectModule::new(
            ObjectBuilder::new(isa.clone(), "exe", default_libcall_names()).unwrap(),
        );

        let mut expr_main_id: Option<FuncId> = None;
        for func_def in &self.functions {
            if func_def.name == "main" {
                let id = add_function_to_module(
                    &mut cranelift_module,
                    isa.clone(),
                    &flags,
                    func_def,
                    "__expr_main",
                    Linkage::Local,
                );
                expr_main_id = Some(id);
            } else {
                add_function_to_module(
                    &mut cranelift_module,
                    isa.clone(),
                    &flags,
                    func_def,
                    &func_def.name,
                    Linkage::Export,
                );
            }
        }

        if let Some(id) = expr_main_id {
            generate_c_main(&mut cranelift_module, isa.clone(), &flags, id);
        }

        let bytes = cranelift_module.finish().emit().unwrap();

        let tmp = std::env::temp_dir().join("__expr_compiler_main.o");
        std::fs::write(&tmp, &bytes).unwrap();

        let status = Command::new("cc")
            .arg(&tmp)
            .arg("-o")
            .arg(output)
            .status()
            .expect("cc not found — install gcc or clang");

        std::fs::remove_file(&tmp).ok();
        assert!(status.success(), "linker failed with: {status}");
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
    name: &str,
    linkage: Linkage,
) -> FuncId {
    let mut sig = Signature::new(isa.default_call_conv());
    sig.returns.push(AbiParam::new(types::I64));
    for _ in &func_def.inputs {
        sig.params.push(AbiParam::new(types::I64));
    }

    let func_id = module
        .declare_function(name, linkage, &sig)
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

fn generate_c_main(
    module: &mut impl CraneliftModule,
    isa: OwnedTargetIsa,
    flags: &settings::Flags,
    expr_main_id: FuncId,
) {
    let mut sig = Signature::new(isa.default_call_conv());
    sig.returns.push(AbiParam::new(types::I32));
    sig.params.push(AbiParam::new(types::I32)); // argc
    sig.params.push(AbiParam::new(types::I64)); // argv (pointer)

    let main_id = module
        .declare_function("main", Linkage::Export, &sig)
        .unwrap();

    let mut ctx = module.make_context();
    ctx.func.signature = sig;
    ctx.func.name = UserFuncName::user(0, main_id.as_u32());

    let expr_main_ref = module.declare_func_in_func(expr_main_id, &mut ctx.func);

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);

        let block_entry = builder.create_block();
        let block_fits = builder.create_block();
        let block_overflow = builder.create_block();

        builder.append_block_params_for_function_params(block_entry);
        builder.switch_to_block(block_entry);

        let call = builder.ins().call(expr_main_ref, &[]);
        let result = builder.inst_results(call)[0];

        let min = builder.ins().iconst(types::I64, i32::MIN as i64);
        let max = builder.ins().iconst(types::I64, i32::MAX as i64);
        let fits_low = builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, result, min);
        let fits_high = builder.ins().icmp(IntCC::SignedLessThanOrEqual, result, max);
        let fits = builder.ins().band(fits_low, fits_high);
        builder.ins().brif(fits, block_fits, &[], block_overflow, &[]);
        builder.seal_block(block_entry);

        builder.switch_to_block(block_fits);
        builder.seal_block(block_fits);
        let narrow = builder.ins().ireduce(types::I32, result);
        builder.ins().return_(&[narrow]);

        builder.switch_to_block(block_overflow);
        builder.seal_block(block_overflow);
        let one = builder.ins().iconst(types::I32, 1);
        builder.ins().return_(&[one]);

        builder.finalize();
    }

    let res = verify_function(&ctx.func, flags);
    if let Err(errors) = res {
        panic!("{}", errors);
    }

    module.define_function(main_id, &mut ctx).unwrap();
    module.clear_context(&mut ctx);
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
fn compile_to_executable_runs() {
    use crate::parser::ParseLexer;
    use crate::tokenizer::{self, Logos};

    let src = "fn main() do\n    7 + 5 - 4\nend";
    let lex = tokenizer::Token::lexer(src);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    let output = std::env::temp_dir().join("__expr_compiler_test_exe");
    Module::from_ast(ast).compile_to_executable(&output);

    let status = Command::new(&output).status().expect("failed to run executable");
    assert_eq!(status.code(), Some(8)); // 7 + 5 - 4 = 8

    std::fs::remove_file(&output).ok();
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
