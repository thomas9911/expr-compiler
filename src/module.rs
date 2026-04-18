use crate::parser::{Ast, BlockAst, ExpressionAst, FunctionDefAst, LiteralAst};
use cranelift::codegen::ir::FuncRef;
use cranelift::codegen::{ir::UserFuncName, verify_function};
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{DataDescription, FuncId, Linkage, Module as CraneliftModule, default_libcall_names};
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

    pub fn from_source(source: &str) -> Self {
        use crate::parser::ParseLexer;
        use crate::tokenizer::{Logos, Token};

        let mut module = Module::new();
        let lex = Token::lexer(source);
        let mut lexer = ParseLexer::new(lex);

        loop {
            while lexer.peek() == Some(&Ok(Token::Newline)) {
                lexer.next();
            }
            if lexer.peek().is_none() {
                break;
            }
            match Ast::from_lexer(&mut lexer) {
                Ok(Ast::FunctionDef(func)) => module.functions.push(func),
                Ok(_) | Err(_) => break,
            }
        }

        module
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

        let mut func_ids = setup_builtins(&mut cranelift_module, &isa, &flags);
        for func_def in &self.functions {
            let id = declare_function_sig(
                &mut cranelift_module,
                &isa,
                func_def,
                &func_def.name,
                Linkage::Export,
            );
            func_ids.insert(func_def.name.clone(), id);
        }
        for func_def in &self.functions {
            define_function_body(
                &mut cranelift_module,
                isa.clone(),
                &flags,
                func_def,
                func_ids[&func_def.name],
                &func_ids,
            );
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

        let mut func_ids = setup_builtins(&mut cranelift_module, &isa, &flags);
        for func_def in &self.functions {
            let id = declare_function_sig(
                &mut cranelift_module,
                &isa,
                func_def,
                &func_def.name,
                Linkage::Export,
            );
            func_ids.insert(func_def.name.clone(), id);
        }
        for func_def in &self.functions {
            define_function_body(
                &mut cranelift_module,
                isa.clone(),
                &flags,
                func_def,
                func_ids[&func_def.name],
                &func_ids,
            );
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

        let mut all_funcs = setup_builtins(&mut cranelift_module, &isa, &flags);
        let mut expr_main_id: Option<FuncId> = None;
        for func_def in &self.functions {
            if func_def.name == "main" {
                let id = declare_function_sig(
                    &mut cranelift_module,
                    &isa,
                    func_def,
                    "__expr_main",
                    Linkage::Local,
                );
                all_funcs.insert("main".to_string(), id);
                expr_main_id = Some(id);
            } else {
                let id = declare_function_sig(
                    &mut cranelift_module,
                    &isa,
                    func_def,
                    &func_def.name,
                    Linkage::Local,
                );
                all_funcs.insert(func_def.name.clone(), id);
            }
        }
        for func_def in &self.functions {
            define_function_body(
                &mut cranelift_module,
                isa.clone(),
                &flags,
                func_def,
                all_funcs[&func_def.name],
                &all_funcs,
            );
        }

        if let Some(id) = expr_main_id {
            generate_c_main(&mut cranelift_module, isa.clone(), &flags, id);
        }

        let bytes = cranelift_module.finish().emit().unwrap();

        let tmp = output.with_extension("o");
        std::fs::write(&tmp, &bytes).unwrap();

        let status = Command::new("cc")
            .arg("-no-pie")
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

fn setup_builtins(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
) -> HashMap<String, FuncId> {
    let fmt_id = module
        .declare_data("__fmt_int", Linkage::Local, false, false)
        .unwrap();
    let mut data_desc = DataDescription::new();
    data_desc.define(b"%lld\n\0".to_vec().into_boxed_slice());
    module.define_data(fmt_id, &data_desc).unwrap();

    let mut printf_sig = Signature::new(isa.default_call_conv());
    printf_sig.params.push(AbiParam::new(isa.pointer_type()));
    printf_sig.params.push(AbiParam::new(types::I64));
    printf_sig.returns.push(AbiParam::new(types::I32));
    let printf_id = module
        .declare_function("printf", Linkage::Import, &printf_sig)
        .unwrap();

    let mut print_sig = Signature::new(isa.default_call_conv());
    print_sig.params.push(AbiParam::new(types::I64));
    print_sig.returns.push(AbiParam::new(types::I64));
    let print_id = module
        .declare_function("__expr_print", Linkage::Local, &print_sig)
        .unwrap();

    let mut ctx = module.make_context();
    ctx.func.signature = print_sig;
    ctx.func.name = UserFuncName::user(0, print_id.as_u32());

    let fmt_gv = module.declare_data_in_func(fmt_id, &mut ctx.func);
    let printf_ref = module.declare_func_in_func(printf_id, &mut ctx.func);

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
        let block0 = builder.create_block();
        builder.append_block_params_for_function_params(block0);
        builder.switch_to_block(block0);
        builder.seal_block(block0);

        let n = builder.block_params(block0)[0];
        let fmt_ptr = builder.ins().global_value(isa.pointer_type(), fmt_gv);
        builder.ins().call(printf_ref, &[fmt_ptr, n]);
        let zero = builder.ins().iconst(types::I64, 0);
        builder.ins().return_(&[zero]);

        builder.finalize();
    }

    let res = verify_function(&ctx.func, flags);
    if let Err(errors) = res {
        panic!("{}", errors);
    }
    module.define_function(print_id, &mut ctx).unwrap();
    module.clear_context(&mut ctx);

    let mut builtins = HashMap::new();
    builtins.insert("print".to_string(), print_id);
    builtins
}

fn declare_function_sig(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    func_def: &FunctionDefAst,
    name: &str,
    linkage: Linkage,
) -> FuncId {
    let mut sig = Signature::new(isa.default_call_conv());
    sig.returns.push(AbiParam::new(types::I64));
    for _ in &func_def.inputs {
        sig.params.push(AbiParam::new(types::I64));
    }
    module.declare_function(name, linkage, &sig).unwrap()
}

fn define_function_body(
    module: &mut impl CraneliftModule,
    isa: OwnedTargetIsa,
    flags: &settings::Flags,
    func_def: &FunctionDefAst,
    func_id: FuncId,
    all_funcs: &HashMap<String, FuncId>,
) {
    let mut sig = Signature::new(isa.default_call_conv());
    sig.returns.push(AbiParam::new(types::I64));
    for _ in &func_def.inputs {
        sig.params.push(AbiParam::new(types::I64));
    }

    let mut ctx = module.make_context();
    ctx.func.signature = sig;
    ctx.func.name = UserFuncName::user(0, func_id.as_u32());

    let func_refs: HashMap<String, FuncRef> = all_funcs
        .iter()
        .map(|(name, &id)| (name.clone(), module.declare_func_in_func(id, &mut ctx.func)))
        .collect();

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
        let block0 = builder.create_block();
        builder.append_block_params_for_function_params(block0);
        builder.switch_to_block(block0);
        builder.seal_block(block0);

        let mut vars: HashMap<String, Variable> = HashMap::new();
        for (i, name) in func_def.inputs.iter().enumerate() {
            let var = builder.declare_var(types::I64);
            let param_val = builder.block_params(block0)[i];
            builder.def_var(var, param_val);
            vars.insert(name.clone(), var);
        }
        for name in local_var_names(&func_def.block) {
            if !vars.contains_key(&name) {
                vars.insert(name, builder.declare_var(types::I64));
            }
        }

        let mut last_val = None;
        for line in &func_def.block.lines {
            last_val = Some(compile_ast(&mut builder, line, &vars, &func_refs));
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
        let fits_low = builder
            .ins()
            .icmp(IntCC::SignedGreaterThanOrEqual, result, min);
        let fits_high = builder
            .ins()
            .icmp(IntCC::SignedLessThanOrEqual, result, max);
        let fits = builder.ins().band(fits_low, fits_high);
        builder
            .ins()
            .brif(fits, block_fits, &[], block_overflow, &[]);
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

fn local_var_names(block: &BlockAst) -> Vec<String> {
    let mut names = vec![];
    for line in &block.lines {
        if let Ast::Assign { name, .. } = line {
            if !names.contains(name) {
                names.push(name.clone());
            }
        }
    }
    names
}

fn compile_ast(
    builder: &mut FunctionBuilder,
    ast: &Ast,
    vars: &HashMap<String, Variable>,
    func_refs: &HashMap<String, FuncRef>,
) -> cranelift::prelude::Value {
    match ast {
        Ast::Literal(LiteralAst::Integer(n)) => builder.ins().iconst(types::I64, *n),
        Ast::Expression(ExpressionAst { function, args }) => {
            let compiled: Vec<_> = args
                .iter()
                .map(|arg| compile_ast(builder, arg, vars, func_refs))
                .collect();
            if function.is_empty() {
                return compiled[0];
            }
            match function.as_str() {
                "add" => builder.ins().iadd(compiled[0], compiled[1]),
                "subtract" => builder.ins().isub(compiled[0], compiled[1]),
                "multiply" => builder.ins().imul(compiled[0], compiled[1]),
                "divide" => builder.ins().sdiv(compiled[0], compiled[1]),
                "modulo" => builder.ins().srem(compiled[0], compiled[1]),
                name => {
                    let func_ref = func_refs
                        .get(name)
                        .unwrap_or_else(|| panic!("undefined function: {name}"));
                    let call = builder.ins().call(*func_ref, &compiled);
                    builder.inst_results(call)[0]
                }
            }
        }
        Ast::Block(block) => {
            let mut last = None;
            for line in &block.lines {
                last = Some(compile_ast(builder, line, vars, func_refs));
            }
            last.expect("empty block")
        }
        Ast::Variable(name) => {
            let var = vars
                .get(name)
                .unwrap_or_else(|| panic!("undefined variable: {name}"));
            builder.use_var(*var)
        }
        Ast::Assign { name, value } => {
            let val = compile_ast(builder, value, vars, func_refs);
            let var = vars
                .get(name)
                .unwrap_or_else(|| panic!("undeclared variable: {name}"));
            builder.def_var(*var, val);
            val
        }
        Ast::FunctionDef(_) => unimplemented!("nested function definitions"),
    }
}

#[test]
fn jit_python_style_multi_function() {
    let src = "fn double(a):\n    a + a\n\nfn square(a):\n    a * a\n\nfn main():\n    square(25) / double(4)\n";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), 78); // square(25)/double(4) = 625/8 = 78
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
fn call_user_defined_function() {
    use crate::parser::ParseLexer;
    use crate::tokenizer::{self, Logos};

    let src = "fn double(x) do\n    x + x\nend\nfn main() do\n    double(21)\nend";
    let lex = tokenizer::Token::lexer(src);
    let mut lexer = ParseLexer::new(lex);

    // parse both functions from sequential Ast::from_lexer calls
    let ast1 = Ast::from_lexer(&mut lexer).unwrap();
    let ast2 = Ast::from_lexer(&mut lexer).unwrap();

    let mut module = Module::new();
    module.add_function(match ast1 {
        Ast::FunctionDef(f) => f,
        _ => panic!(),
    });
    module.add_function(match ast2 {
        Ast::FunctionDef(f) => f,
        _ => panic!(),
    });

    let jit = module.compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };

    assert_eq!(func(), 42); // double(21) = 42
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

    let status = Command::new(&output)
        .status()
        .expect("failed to run executable");
    assert_eq!(status.code(), Some(8)); // 7 + 5 - 4 = 8

    std::fs::remove_file(&output).ok();
}

#[test]
fn print_builtin_executable() {
    let src = "fn main() do\n    print(42)\nend";
    let output = std::env::temp_dir().join("__expr_compiler_print_test");
    Module::from_source(src).compile_to_executable(&output);

    let out = Command::new(&output).output().expect("run failed");
    std::fs::remove_file(&output).ok();

    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "42");
    assert_eq!(out.status.code(), Some(0));
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

#[test]
fn local_variable_assignment() {
    let src = "fn main() do\n    x = 10\n    y = x + 5\n    y * 2\nend";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), 30); // x=10, y=15, 15*2=30
}
