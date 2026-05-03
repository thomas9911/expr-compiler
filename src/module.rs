use crate::parser::{Ast, BlockAst, ExpressionAst, FunctionDefAst, LiteralAst};
use cranelift::codegen::ir::FuncRef;
use cranelift::codegen::ir::condcodes::IntCC;
use cranelift::codegen::ir::instructions::BlockArg;
use cranelift::codegen::{ir::UserFuncName, verify_function};
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{FuncId, Linkage, Module as CraneliftModule, default_libcall_names};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::{isa::OwnedTargetIsa, settings, *};
use std::collections::HashMap;
use std::path::Path;
use std::process::Command;
#[cfg(feature = "llvm-backend")]
mod llvm_backend;
mod runtime_ir;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodegenBackend {
    Cranelift,
    Llvm,
}

impl std::str::FromStr for CodegenBackend {
    type Err = String;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "cranelift" => Ok(Self::Cranelift),
            "llvm" => Ok(Self::Llvm),
            _ => Err(format!("unknown backend: {value}")),
        }
    }
}

pub fn llvm_backend_available() -> bool {
    cfg!(feature = "llvm-backend")
}

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

    pub fn compile_to_jit(self) -> JitArtifact {
        self.compile_to_jit_with_backend(CodegenBackend::Cranelift)
    }

    pub fn compile_to_jit_with_backend(self, backend: CodegenBackend) -> JitArtifact {
        match backend {
            CodegenBackend::Cranelift => JitArtifact::Cranelift(self.compile_to_cranelift_jit()),
            CodegenBackend::Llvm => {
                #[cfg(feature = "llvm-backend")]
                {
                    JitArtifact::Llvm(llvm_backend::compile_to_jit(self))
                }
                #[cfg(not(feature = "llvm-backend"))]
                {
                    let _ = self;
                    panic!(
                        "llvm backend is not available in this build; enable the `llvm-backend` cargo feature"
                    );
                }
            }
        }
    }

    fn compile_to_cranelift_jit(self) -> CraneliftJitModule {
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift::native::builder()
            .expect("host machine supported")
            .finish(flags.clone())
            .unwrap();

        let jit_builder = JITBuilder::with_isa(isa.clone(), default_libcall_names());
        let mut cranelift_module = JITModule::new(jit_builder);

        let (arena_base_addr, arena_offset_addr) = crate::runtime::jit_arena_addresses();
        let mut func_ids = runtime_ir::setup_builtins_jit(
            &mut cranelift_module,
            &isa,
            &flags,
            crate::runtime::__expr_print_host as usize as i64,
            crate::runtime::__expr_list_print_host as usize as i64,
            arena_base_addr,
            arena_offset_addr,
        );
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

        CraneliftJitModule {
            module: cranelift_module,
            func_ids,
        }
    }

    pub fn compile_to_ir(self) -> String {
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift::native::builder()
            .expect("host machine supported")
            .finish(flags.clone())
            .unwrap();

        let mut cranelift_module = ObjectModule::new(
            ObjectBuilder::new(isa.clone(), "ir", default_libcall_names()).unwrap(),
        );

        let mut func_ids = runtime_ir::setup_builtins(&mut cranelift_module, &isa, &flags);
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

        let mut out = String::new();

        // Stub for __expr_print: the Cranelift interpreter cannot call external
        // functions (printf), so we emit a pure-IR stub that returns its argument.
        // This means print() won't produce output in --run-ir mode but won't crash,
        // and the "printed" value surfaces as the function's return value.
        let print_func_id = func_ids["print"].as_u32();
        let print_stub = format!(
            "; builtin: print (interpreter stub — no I/O; use --run-jit for real output)\n\
             function u0:{print_func_id}(i64) -> i64 system_v {{\n\
             block0(v0: i64):\n    v1 = iconst.i64 0\n    return v1\n}}\n\n"
        );
        out.push_str(&print_stub);

        for func_def in &self.functions {
            let ir = define_function_body(
                &mut cranelift_module,
                isa.clone(),
                &flags,
                func_def,
                func_ids[&func_def.name],
                &func_ids,
            );
            out.push_str(&ir);
            out.push('\n');
        }
        out
    }

    pub fn compile_to_object(self, name: &str) -> Vec<u8> {
        self.compile_to_object_with_backend(name, CodegenBackend::Cranelift)
    }

    pub fn compile_to_object_with_backend(self, name: &str, backend: CodegenBackend) -> Vec<u8> {
        match backend {
            CodegenBackend::Cranelift => self.compile_to_cranelift_object(name),
            CodegenBackend::Llvm => {
                #[cfg(feature = "llvm-backend")]
                {
                    llvm_backend::compile_to_object(self, name)
                }
                #[cfg(not(feature = "llvm-backend"))]
                {
                    let _ = name;
                    let _ = self;
                    panic!(
                        "llvm backend is not available in this build; enable the `llvm-backend` cargo feature"
                    );
                }
            }
        }
    }

    fn compile_to_cranelift_object(self, name: &str) -> Vec<u8> {
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift::native::builder()
            .expect("host machine supported")
            .finish(flags.clone())
            .unwrap();

        let mut cranelift_module = ObjectModule::new(
            ObjectBuilder::new(isa.clone(), name, default_libcall_names()).unwrap(),
        );

        let mut func_ids = runtime_ir::setup_builtins(&mut cranelift_module, &isa, &flags);
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
        self.compile_to_executable_with_backend(output, CodegenBackend::Cranelift)
    }

    pub fn compile_to_executable_with_backend(self, output: &Path, backend: CodegenBackend) {
        match backend {
            CodegenBackend::Cranelift => self.compile_to_cranelift_executable(output),
            CodegenBackend::Llvm => {
                #[cfg(feature = "llvm-backend")]
                {
                    self.compile_to_llvm_executable(output);
                }
                #[cfg(not(feature = "llvm-backend"))]
                {
                    let _ = output;
                    let _ = self;
                    panic!(
                        "llvm backend is not available in this build; enable the `llvm-backend` cargo feature"
                    );
                }
            }
        }
    }

    fn compile_to_cranelift_executable(self, output: &Path) {
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift::native::builder()
            .expect("host machine supported")
            .finish(flags.clone())
            .unwrap();

        let mut cranelift_module = ObjectModule::new(
            ObjectBuilder::new(isa.clone(), "exe", default_libcall_names()).unwrap(),
        );

        let mut all_funcs = runtime_ir::setup_builtins(&mut cranelift_module, &isa, &flags);
        let mut expr_main_id: Option<FuncId> = None;
        #[cfg(windows)]
        let expr_main_symbol = "expr_main_entry";
        #[cfg(not(windows))]
        let expr_main_symbol = "__expr_main";
        for func_def in &self.functions {
            if func_def.name == "main" {
                let main_linkage = if cfg!(windows) {
                    Linkage::Export
                } else {
                    Linkage::Local
                };
                let id = declare_function_sig(
                    &mut cranelift_module,
                    &isa,
                    func_def,
                    expr_main_symbol,
                    main_linkage,
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

        #[cfg(not(windows))]
        if let Some(id) = expr_main_id {
            generate_c_main(
                &mut cranelift_module,
                isa.clone(),
                &flags,
                id,
                all_funcs["__value_to_i64"],
            );
        }
        #[cfg(windows)]
        if true {
            _ = expr_main_id;
        }

        let bytes = cranelift_module.finish().emit().unwrap();

        #[cfg(windows)]
        let tmp = output.with_extension("obj");
        #[cfg(not(windows))]
        let tmp = output.with_extension("o");
        std::fs::write(&tmp, &bytes).unwrap();

        #[cfg(windows)]
        let status = Command::new("rustc")
            .arg(write_windows_wrapper(output))
            .arg("--crate-name")
            .arg("expr_windows_wrapper")
            .arg("-C")
            .arg("panic=abort")
            .arg("-C")
            .arg("opt-level=s")
            .arg("-C")
            .arg("strip=symbols")
            .arg("-C")
            .arg("debuginfo=0")
            .arg("-C")
            .arg("link-arg=/DEBUG:NONE")
            .arg("-C")
            .arg("link-arg=/ENTRY:mainCRTStartup")
            .arg("-C")
            .arg("link-arg=/SUBSYSTEM:CONSOLE")
            .arg("-C")
            .arg(format!("link-arg={}", tmp.display()))
            .arg("-o")
            .arg(output)
            .status()
            .expect("rustc not found");

        #[cfg(not(windows))]
        let wrapper = write_unix_wrapper(output);
        #[cfg(not(windows))]
        let status = Command::new("cc")
            .arg("-no-pie")
            .arg(&tmp)
            .arg(&wrapper)
            .arg("-o")
            .arg(output)
            .status()
            .expect("cc not found — install gcc or clang");

        #[cfg(windows)]
        std::fs::remove_file(output.with_extension("wrapper.rs")).ok();
        #[cfg(not(windows))]
        std::fs::remove_file(output.with_extension("wrapper.c")).ok();
        std::fs::remove_file(&tmp).ok();
        assert!(status.success(), "linker failed with: {status}");
    }

    #[cfg(feature = "llvm-backend")]
    fn compile_to_llvm_executable(self, output: &Path) {
        let bytes = llvm_backend::compile_to_object(self, "llvm_exe");
        #[cfg(windows)]
        let tmp = output.with_extension("obj");
        #[cfg(not(windows))]
        let tmp = output.with_extension("o");
        std::fs::write(&tmp, &bytes).unwrap();

        #[cfg(windows)]
        let status = Command::new("rustc")
            .arg(write_windows_wrapper(output))
            .arg("--crate-name")
            .arg("expr_windows_wrapper")
            .arg("-C")
            .arg("panic=abort")
            .arg("-C")
            .arg("opt-level=s")
            .arg("-C")
            .arg("strip=symbols")
            .arg("-C")
            .arg("debuginfo=0")
            .arg("-C")
            .arg("link-arg=/DEBUG:NONE")
            .arg("-C")
            .arg("link-arg=/ENTRY:mainCRTStartup")
            .arg("-C")
            .arg("link-arg=/SUBSYSTEM:CONSOLE")
            .arg("-C")
            .arg(format!("link-arg={}", tmp.display()))
            .arg("-o")
            .arg(output)
            .status()
            .expect("rustc not found");

        #[cfg(not(windows))]
        let status = Command::new("rustc")
            .arg(write_unix_rust_wrapper(output))
            .arg("--crate-name")
            .arg("expr_unix_wrapper")
            .arg("-C")
            .arg("panic=abort")
            .arg("-C")
            .arg("opt-level=s")
            .arg("-C")
            .arg("strip=symbols")
            .arg("-C")
            .arg("debuginfo=0")
            .arg("-C")
            .arg(format!("link-arg={}", tmp.display()))
            .arg("-o")
            .arg(output)
            .status()
            .expect("rustc not found");

        #[cfg(windows)]
        std::fs::remove_file(output.with_extension("wrapper.rs")).ok();
        #[cfg(not(windows))]
        std::fs::remove_file(output.with_extension("wrapper.rs")).ok();
        std::fs::remove_file(&tmp).ok();
        assert!(status.success(), "linker failed with: {status}");
    }
}

pub enum JitArtifact {
    Cranelift(CraneliftJitModule),
    #[cfg(feature = "llvm-backend")]
    Llvm(llvm_backend::LlvmJitModule),
}

impl JitArtifact {
    pub fn get_fn_ptr(&self, name: &str) -> *const u8 {
        match self {
            Self::Cranelift(module) => module.get_fn_ptr(name),
            #[cfg(feature = "llvm-backend")]
            Self::Llvm(module) => module.get_fn_ptr(name),
        }
    }

    pub fn has_function(&self, name: &str) -> bool {
        match self {
            Self::Cranelift(module) => module.has_function(name),
            #[cfg(feature = "llvm-backend")]
            Self::Llvm(module) => module.has_function(name),
        }
    }

    pub fn user_function_names(&self) -> impl Iterator<Item = &str> {
        match self {
            Self::Cranelift(module) => module.user_function_names().collect::<Vec<_>>().into_iter(),
            #[cfg(feature = "llvm-backend")]
            Self::Llvm(module) => module.user_function_names().collect::<Vec<_>>().into_iter(),
        }
    }
}

pub struct CraneliftJitModule {
    module: JITModule,
    func_ids: HashMap<String, FuncId>,
}

impl CraneliftJitModule {
    pub fn get_fn_ptr(&self, name: &str) -> *const u8 {
        self.module.get_finalized_function(self.func_ids[name])
    }

    pub fn has_function(&self, name: &str) -> bool {
        self.func_ids.contains_key(name)
    }

    pub fn user_function_names(&self) -> impl Iterator<Item = &str> {
        self.func_ids
            .keys()
            .filter(|n| !is_builtin_name(n))
            .map(|s| s.as_str())
    }
}

#[cfg(windows)]
fn write_windows_wrapper(output: &Path) -> std::path::PathBuf {
    let wrapper = output.with_extension("wrapper.rs");
    let source = include_str!("./wrapper/windows.rs");
    std::fs::write(&wrapper, source).unwrap();
    wrapper
}

#[cfg(not(windows))]
fn write_unix_wrapper(output: &Path) -> std::path::PathBuf {
    let wrapper = output.with_extension("wrapper.c");
    let source = include_str!("./wrapper/unix.c");
    std::fs::write(&wrapper, source).unwrap();
    wrapper
}

#[cfg(not(windows))]
fn write_unix_rust_wrapper(output: &Path) -> std::path::PathBuf {
    let wrapper = output.with_extension("wrapper.rs");
    let source = include_str!("./wrapper/unix.rs");
    std::fs::write(&wrapper, source).unwrap();
    wrapper
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
) -> String {
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

    let ir = format!("; fn {}\n{}", func_def.name, ctx.func.display());
    module.define_function(func_id, &mut ctx).unwrap();
    module.clear_context(&mut ctx);
    ir
}

#[cfg(not(windows))]
fn generate_c_main(
    module: &mut impl CraneliftModule,
    isa: OwnedTargetIsa,
    flags: &settings::Flags,
    expr_main_id: FuncId,
    value_to_i64_id: FuncId,
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
    let value_to_i64_ref = module.declare_func_in_func(value_to_i64_id, &mut ctx.func);

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
        let decode_call = builder.ins().call(value_to_i64_ref, &[result]);
        let int_result = builder.inst_results(decode_call)[0];

        let min = builder.ins().iconst(types::I64, i32::MIN as i64);
        let max = builder.ins().iconst(types::I64, i32::MAX as i64);
        let fits_low = builder
            .ins()
            .icmp(IntCC::SignedGreaterThanOrEqual, int_result, min);
        let fits_high = builder
            .ins()
            .icmp(IntCC::SignedLessThanOrEqual, int_result, max);
        let fits = builder.ins().band(fits_low, fits_high);
        builder
            .ins()
            .brif(fits, block_fits, &[], block_overflow, &[]);
        builder.seal_block(block_entry);

        builder.switch_to_block(block_fits);
        builder.seal_block(block_fits);
        let narrow = builder.ins().ireduce(types::I32, int_result);
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

pub(super) fn local_var_names(block: &BlockAst) -> Vec<String> {
    let mut names = vec![];
    for line in &block.lines {
        collect_var_names(line, &mut names);
    }
    names
}

pub(super) fn is_builtin_name(name: &str) -> bool {
    name.starts_with("__")
        || matches!(
            name,
            "print"
                | "list_new"
                | "list_push"
                | "list_len"
                | "list_get"
                | "list_pop"
                | "list_copy"
                | "list_print"
        )
}

fn collect_var_names(ast: &Ast, names: &mut Vec<String>) {
    match ast {
        Ast::Assign { name, value } => {
            if !names.contains(name) {
                names.push(name.clone());
            }
            collect_var_names(value, names);
        }
        Ast::If {
            condition,
            then,
            else_,
        } => {
            collect_var_names(condition, names);
            for line in &then.lines {
                collect_var_names(line, names);
            }
            if let Some(e) = else_ {
                for line in &e.lines {
                    collect_var_names(line, names);
                }
            }
        }
        _ => {}
    }
}

fn require_func(func_refs: &HashMap<String, FuncRef>, name: &str) -> FuncRef {
    *func_refs
        .get(name)
        .unwrap_or_else(|| panic!("builtin function '{name}' is missing"))
}

fn call_unary(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    name: &str,
    arg: Value,
) -> Value {
    let func_ref = require_func(func_refs, name);
    let call = builder.ins().call(func_ref, &[arg]);
    builder.inst_results(call)[0]
}

fn call_binary(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    name: &str,
    lhs: Value,
    rhs: Value,
) -> Value {
    let func_ref = require_func(func_refs, name);
    let call = builder.ins().call(func_ref, &[lhs, rhs]);
    builder.inst_results(call)[0]
}

fn boxed_int_const(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    value: i64,
) -> Value {
    let raw = builder.ins().iconst(types::I64, value);
    call_unary(builder, func_refs, "__value_int", raw)
}

fn compile_list_literal(
    builder: &mut FunctionBuilder,
    items: &[Ast],
    vars: &HashMap<String, Variable>,
    func_refs: &HashMap<String, FuncRef>,
) -> Value {
    let list_new_ref = *func_refs
        .get("list_new")
        .expect("builtin function 'list_new' is missing");
    let list_push_ref = *func_refs
        .get("list_push")
        .expect("builtin function 'list_push' is missing");

    let create_call = builder.ins().call(list_new_ref, &[]);
    let handle = builder.inst_results(create_call)[0];

    for item in items {
        let value = compile_ast(builder, item, vars, func_refs);
        let push_call = builder.ins().call(list_push_ref, &[handle, value]);
        let _ = builder.inst_results(push_call)[0];
    }

    handle
}

fn compile_ast(
    builder: &mut FunctionBuilder,
    ast: &Ast,
    vars: &HashMap<String, Variable>,
    func_refs: &HashMap<String, FuncRef>,
) -> cranelift::prelude::Value {
    match ast {
        Ast::Literal(LiteralAst::Integer(n)) => boxed_int_const(builder, func_refs, *n),
        Ast::ListLiteral(items) => compile_list_literal(builder, items, vars, func_refs),
        Ast::Index { collection, index } => {
            let collection_value = compile_ast(builder, collection, vars, func_refs);
            let index_value = compile_ast(builder, index, vars, func_refs);
            call_binary(
                builder,
                func_refs,
                "list_get",
                collection_value,
                index_value,
            )
        }
        Ast::Expression(ExpressionAst { function, args }) => {
            let compiled: Vec<_> = args
                .iter()
                .map(|arg| compile_ast(builder, arg, vars, func_refs))
                .collect();
            if function.is_empty() {
                return compiled[0];
            }
            match function.as_str() {
                "add" => call_binary(builder, func_refs, "__op_add", compiled[0], compiled[1]),
                "subtract" => call_binary(
                    builder,
                    func_refs,
                    "__op_subtract",
                    compiled[0],
                    compiled[1],
                ),
                "multiply" => call_binary(
                    builder,
                    func_refs,
                    "__op_multiply",
                    compiled[0],
                    compiled[1],
                ),
                "divide" => {
                    call_binary(builder, func_refs, "__op_divide", compiled[0], compiled[1])
                }
                "modulo" => {
                    call_binary(builder, func_refs, "__op_modulo", compiled[0], compiled[1])
                }
                "gt" => call_binary(builder, func_refs, "__op_gt", compiled[0], compiled[1]),
                "lt" => call_binary(builder, func_refs, "__op_lt", compiled[0], compiled[1]),
                "gte" => call_binary(builder, func_refs, "__op_gte", compiled[0], compiled[1]),
                "lte" => call_binary(builder, func_refs, "__op_lte", compiled[0], compiled[1]),
                "eq" => call_binary(builder, func_refs, "__op_eq", compiled[0], compiled[1]),
                "ne" => call_binary(builder, func_refs, "__op_ne", compiled[0], compiled[1]),
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
        Ast::If {
            condition,
            then,
            else_,
        } => {
            let cond_val = compile_ast(builder, condition, vars, func_refs);
            let truth_value = call_unary(builder, func_refs, "__value_is_truthy", cond_val);
            let cond_non_zero = builder.ins().icmp_imm(IntCC::NotEqual, truth_value, 0);

            let then_block = builder.create_block();
            let merge_block = builder.create_block();
            builder.append_block_param(merge_block, types::I64);

            if let Some(else_block_ast) = else_ {
                let else_block = builder.create_block();
                builder
                    .ins()
                    .brif(cond_non_zero, then_block, &[], else_block, &[]);

                builder.switch_to_block(then_block);
                builder.seal_block(then_block);
                let mut then_val = boxed_int_const(builder, func_refs, 0);
                for line in &then.lines {
                    then_val = compile_ast(builder, line, vars, func_refs);
                }
                builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(then_val)]);

                builder.switch_to_block(else_block);
                builder.seal_block(else_block);
                let mut else_val = boxed_int_const(builder, func_refs, 0);
                for line in &else_block_ast.lines {
                    else_val = compile_ast(builder, line, vars, func_refs);
                }
                builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(else_val)]);
            } else {
                let boxed_zero = boxed_int_const(builder, func_refs, 0);
                builder.ins().brif(
                    cond_non_zero,
                    then_block,
                    &[],
                    merge_block,
                    &[BlockArg::Value(boxed_zero)],
                );

                builder.switch_to_block(then_block);
                builder.seal_block(then_block);
                let mut then_val = boxed_int_const(builder, func_refs, 0);
                for line in &then.lines {
                    then_val = compile_ast(builder, line, vars, func_refs);
                }
                builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(then_val)]);
            }

            builder.switch_to_block(merge_block);
            builder.seal_block(merge_block);
            builder.block_params(merge_block)[0]
        }
        Ast::FunctionDef(_) => panic!("nested function definitions are not supported"),
    }
}

#[cfg(all(windows, test))]
fn windows_temp_exe_path(base: &str) -> std::path::PathBuf {
    let mut path = std::env::temp_dir().join(base);
    if path.extension().is_none() {
        path.set_extension("exe");
    }
    path
}

#[cfg(test)]
fn expect_int(value: i64) -> i64 {
    crate::runtime::decode_int(value).expect("expected boxed integer")
}

#[cfg(test)]
fn boxed_int(value: i64) -> i64 {
    crate::runtime::__expr_value_int_host(value)
}

#[cfg(all(test, feature = "llvm-backend"))]
fn assert_jit_backend_result(src: &str, backend: CodegenBackend, expected: i64) {
    crate::runtime::reset_runtime_arena();
    let jit = Module::from_source(src).compile_to_jit_with_backend(backend);
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(expect_int(func()), expected);
}

#[test]
fn jit_python_style_multi_function() {
    let src = "fn double(a):\n    a + a\n\nfn square(a):\n    a * a\n\nfn main():\n    square(25) / double(4)\n";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(expect_int(func()), 78); // square(25)/double(4) = 625/8 = 78
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

    assert_eq!(expect_int(func()), 8);
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

    assert_eq!(expect_int(func(boxed_int(3), boxed_int(5))), 8);
    assert_eq!(expect_int(func(boxed_int(10), boxed_int(-4))), 6);
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

    assert_eq!(expect_int(func()), 42); // double(21) = 42
}

#[test]
fn compile_to_executable_runs() {
    use crate::parser::ParseLexer;
    use crate::tokenizer::{self, Logos};

    let src = "fn main() do\n    7 + 5 - 4\nend";
    let lex = tokenizer::Token::lexer(src);
    let mut lexer = ParseLexer::new(lex);
    let ast = Ast::from_lexer(&mut lexer).unwrap();

    #[cfg(windows)]
    let output = windows_temp_exe_path("__expr_compiler_test_exe");
    #[cfg(not(windows))]
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
    #[cfg(windows)]
    let output = windows_temp_exe_path("__expr_compiler_print_test");
    #[cfg(not(windows))]
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
    assert_eq!(expect_int(func()), 30); // x=10, y=15, 15*2=30
}

#[test]
fn if_without_else() {
    // returns then-value when true, 0 when false
    let src = "fn main() do\n    if 10 > 5 do\n        42\n    end\nend";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(expect_int(func()), 42);
}

#[test]
fn if_with_else() {
    let src = "fn main() do\n    if 3 > 5 do\n        1\n    else\n        99\n    end\nend";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(expect_int(func()), 99);
}

#[test]
fn if_python_style() {
    let src = "fn main():\n    x = 10\n    if x > 5:\n        x * 2\n    else:\n        x\n";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(expect_int(func()), 20);
}

#[test]
fn ir_contains_overflow_trap_for_add() {
    let src = "fn main() do\n    9223372036854775807 + 1\nend";
    let ir = Module::from_source(src).compile_to_ir();
    assert!(ir.contains("; fn main"));
    assert!(ir.contains("function"));
}

#[test]
fn jit_list_builtins_work() {
    crate::runtime::reset_runtime_arena();
    let src = "fn main() do\n    xs = list_new()\n    list_push(xs, 10)\n    list_push(xs, 32)\n    list_get(xs, 0) + list_get(xs, 1) + list_len(xs)\nend";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(expect_int(func()), 44);
}

#[test]
fn jit_list_literal_works() {
    crate::runtime::reset_runtime_arena();
    let src = "fn main() do\n    xs = [10, 32]\n    list_get(xs, 0) + list_get(xs, 1) + list_len(xs)\nend";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(expect_int(func()), 44);
}

#[test]
fn jit_index_syntax_works() {
    crate::runtime::reset_runtime_arena();
    let src = "fn main() do\n    xs = [1, 2, 3]\n    xs[1]\nend";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(expect_int(func()), 2);
}

#[test]
fn jit_list_pop_works() {
    crate::runtime::reset_runtime_arena();
    let src = "fn main() do\n    xs = [1, 2, 3]\n    x = list_pop(xs)\n    x + list_len(xs)\nend";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(expect_int(func()), 5);
}

#[test]
fn jit_list_copy_works() {
    crate::runtime::reset_runtime_arena();
    let src = "fn main() do\n    xs = [1, 2, 3]\n    ys = list_copy(xs)\n    list_pop(xs)\n    list_len(xs) + list_len(ys) + ys[2]\nend";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(expect_int(func()), 8);
}

#[test]
fn jit_list_print_returns_zero() {
    crate::runtime::reset_runtime_arena();
    let src = "fn main() do\n    xs = [4, 5, 6]\n    list_print(xs)\nend";
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit.get_fn_ptr("main");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(expect_int(func()), 0);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_arithmetic_works() {
    assert_jit_backend_result("fn main() do\n    7 + 5 - 4\nend", CodegenBackend::Llvm, 8);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_function_calls_work() {
    let src = "fn add(x, y) do\n    x + y\nend\n\nfn main() do\n    add(20, 22)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 42);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_if_else_works() {
    let src = "fn main() do\n    if 1 do\n        41\n    else\n        0\n    end\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 41);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_lists_work() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    ys = list_copy(xs)\n    list_pop(xs)\n    list_len(xs) + list_len(ys) + ys[2]\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 8);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_list_print_returns_zero() {
    let src = "fn main() do\n    xs = [4, 5, 6]\n    list_print(xs)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 0);
}

#[cfg(all(feature = "llvm-backend", windows))]
#[test]
fn llvm_compile_to_executable_runs() {
    let src = "fn main() do\n    7 + 5 - 4\nend";
    let output = windows_temp_exe_path("__expr_compiler_llvm_test_exe");
    Module::from_source(src).compile_to_executable_with_backend(&output, CodegenBackend::Llvm);

    let status = Command::new(&output)
        .status()
        .expect("failed to run llvm executable");
    assert_eq!(status.code(), Some(8));

    std::fs::remove_file(&output).ok();
}
