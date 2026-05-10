use crate::parser::{Ast, BlockAst, ExpressionAst, FunctionDefAst, LiteralAst};
use crate::value::{
    CLOSURE_ENV_PTR_OFFSET, CLOSURE_FUNCTION_ORDINAL_OFFSET, CLOSURE_SIZE, TAG_FUNCTION, TAG_INT,
    VALUE_PAYLOAD_OFFSET, VALUE_SIZE,
};
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
    closure_metadata: HashMap<String, ClosureMetadata>,
}

struct LambdaLifter {
    next_id: usize,
    lifted: Vec<FunctionDefAst>,
    metadata: HashMap<String, ClosureMetadata>,
}

#[derive(Clone, Debug, Default)]
pub(super) struct ClosureMetadata {
    pub(super) captures: Vec<String>,
}

#[derive(Clone, Copy)]
struct CompiledValue {
    tag: Value,
    payload: Value,
}

#[derive(Clone, Copy)]
struct LocalValueVar {
    tag: Variable,
    payload: Variable,
}

impl Module {
    pub fn new() -> Self {
        Module {
            functions: vec![],
            closure_metadata: HashMap::new(),
        }
    }

    pub fn add_function(&mut self, func: FunctionDefAst) {
        self.functions.push(func);
    }

    pub fn from_source(source: &str) -> Self {
        use crate::parser::ParseLexer;
        use crate::tokenizer::{Logos, Token};

        let mut functions = vec![];
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
                Ok(Ast::FunctionDef(func)) => functions.push(func),
                Ok(_) | Err(_) => break,
            }
        }

        let (functions, closure_metadata) = lift_anonymous_functions(functions);
        Module {
            functions,
            closure_metadata,
        }
    }

    pub fn from_ast(ast: Ast) -> Self {
        let mut functions = vec![];
        match ast {
            Ast::FunctionDef(func) => functions.push(func),
            Ast::Block(block) => {
                for line in block.lines {
                    if let Ast::FunctionDef(func) = line {
                        functions.push(func);
                    }
                }
            }
            _ => {}
        }
        let (functions, closure_metadata) = lift_anonymous_functions(functions);
        Module {
            functions,
            closure_metadata,
        }
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
        let builtin_ids = runtime_ir::setup_builtins_jit(
            &mut cranelift_module,
            &isa,
            &flags,
            crate::runtime::__expr_print_host as usize as i64,
            crate::runtime::__expr_list_print_host as usize as i64,
            arena_base_addr,
            arena_offset_addr,
        );
        let function_ordinals = function_ordinals(&self.functions);
        let function_arities = function_arities(&self.functions);
        let closure_metadata = self.closure_metadata.clone();
        let mut internal_func_ids = builtin_ids.clone();
        let mut int_result_func_ids = HashMap::new();
        for func_def in &self.functions {
            let internal_id = declare_internal_function_sig(
                &mut cranelift_module,
                &isa,
                func_def,
                closure_metadata.contains_key(&func_def.name),
                &internal_symbol_name(&func_def.name),
                Linkage::Local,
            );
            internal_func_ids.insert(func_def.name.clone(), internal_id);
        }
        for func_def in &self.functions {
            define_function_body(
                &mut cranelift_module,
                isa.clone(),
                &flags,
                func_def,
                internal_func_ids[&func_def.name],
                &internal_func_ids,
                &function_ordinals,
                &function_arities,
                &closure_metadata,
            );
            if func_def.inputs.is_empty() {
                let scalar_id = declare_zero_arg_int_result_sig(
                    &mut cranelift_module,
                    &isa,
                    &int_result_symbol_name(&func_def.name),
                    Linkage::Local,
                );
                define_zero_arg_int_result_wrapper(
                    &mut cranelift_module,
                    isa.clone(),
                    &flags,
                    scalar_id,
                    internal_func_ids[&func_def.name],
                );
                int_result_func_ids.insert(func_def.name.clone(), scalar_id);
            }
        }

        cranelift_module.finalize_definitions().unwrap();

        CraneliftJitModule {
            module: cranelift_module,
            func_ids: internal_func_ids.clone(),
            internal_func_ids,
            int_result_func_ids,
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

        let builtin_ids = runtime_ir::setup_builtins(&mut cranelift_module, &isa, &flags);
        let function_ordinals = function_ordinals(&self.functions);
        let function_arities = function_arities(&self.functions);
        let closure_metadata = self.closure_metadata.clone();
        let mut internal_func_ids = builtin_ids.clone();
        for func_def in &self.functions {
            let id = declare_internal_function_sig(
                &mut cranelift_module,
                &isa,
                func_def,
                closure_metadata.contains_key(&func_def.name),
                &internal_symbol_name(&func_def.name),
                Linkage::Local,
            );
            internal_func_ids.insert(func_def.name.clone(), id);
        }

        let mut out = String::new();

        // Stub for __expr_print: the Cranelift interpreter cannot call external
        // functions (printf), so we emit a pure-IR stub that returns its argument.
        // This means print() won't produce output in --run-ir mode but won't crash,
        // and the "printed" value surfaces as the function's return value.
        let print_func_id = builtin_ids["print"].as_u32();
        let print_stub = format!(
            "; builtin: print (interpreter stub — no I/O; use --run-jit for real output)\n\
             function u0:{print_func_id}(i64, i64) -> i64, i64 system_v {{\n\
             block0(v0: i64, v1: i64):\n    v2 = iconst.i64 {tag_int}\n    v3 = iconst.i64 0\n    return v2, v3\n}}\n\n",
            tag_int = TAG_INT
        );
        out.push_str(&print_stub);

        for func_def in &self.functions {
            let ir = define_function_body(
                &mut cranelift_module,
                isa.clone(),
                &flags,
                func_def,
                internal_func_ids[&func_def.name],
                &internal_func_ids,
                &function_ordinals,
                &function_arities,
                &closure_metadata,
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

        let builtin_ids = runtime_ir::setup_builtins(&mut cranelift_module, &isa, &flags);
        let function_ordinals = function_ordinals(&self.functions);
        let function_arities = function_arities(&self.functions);
        let closure_metadata = self.closure_metadata.clone();
        let mut internal_func_ids = builtin_ids.clone();
        for func_def in &self.functions {
            let internal_id = declare_internal_function_sig(
                &mut cranelift_module,
                &isa,
                func_def,
                closure_metadata.contains_key(&func_def.name),
                &internal_symbol_name(&func_def.name),
                Linkage::Local,
            );
            internal_func_ids.insert(func_def.name.clone(), internal_id);
        }
        for func_def in &self.functions {
            define_function_body(
                &mut cranelift_module,
                isa.clone(),
                &flags,
                func_def,
                internal_func_ids[&func_def.name],
                &internal_func_ids,
                &function_ordinals,
                &function_arities,
                &closure_metadata,
            );
        }
        cranelift_module.finish().emit().unwrap()
    }

    pub fn compile_to_executable(self, output: &Path) {
        self.compile_to_executable_with_backend(output, CodegenBackend::Cranelift)
    }

    pub fn compile_to_executable_with_backend(self, output: &Path, backend: CodegenBackend) {
        if is_component_wasm_output(output) {
            match backend {
                CodegenBackend::Llvm => {
                    #[cfg(all(feature = "llvm-backend", feature = "wasi"))]
                    {
                        self.compile_to_llvm_component(output);
                        return;
                    }
                    #[cfg(not(all(feature = "llvm-backend", feature = "wasi")))]
                    {
                        let _ = output;
                        let _ = self;
                        panic!(
                            "component wasm output requires the `wasi` cargo feature (which also enables `llvm-backend`)"
                        );
                    }
                }
                CodegenBackend::Cranelift => {
                    panic!("component wasm output currently supports only the llvm backend");
                }
            }
        }

        if is_wasm_output(output) {
            match backend {
                CodegenBackend::Llvm => {
                    #[cfg(feature = "llvm-backend")]
                    {
                        self.compile_to_llvm_wasm(output);
                        return;
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
                CodegenBackend::Cranelift => {
                    panic!("core wasm output currently supports only the llvm backend");
                }
            }
        }

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

        let builtin_ids = runtime_ir::setup_builtins(&mut cranelift_module, &isa, &flags);
        let function_ordinals = function_ordinals(&self.functions);
        let function_arities = function_arities(&self.functions);
        let closure_metadata = self.closure_metadata.clone();
        let mut internal_func_ids = builtin_ids.clone();
        let mut expr_main_int_id: Option<FuncId> = None;
        for func_def in &self.functions {
            if func_def.name == "main" {
                let internal_id = declare_internal_function_sig(
                    &mut cranelift_module,
                    &isa,
                    func_def,
                    closure_metadata.contains_key(&func_def.name),
                    &internal_symbol_name(&func_def.name),
                    Linkage::Local,
                );
                internal_func_ids.insert("main".to_string(), internal_id);
                if func_def.inputs.is_empty() {
                    #[cfg(windows)]
                    let int_symbol = "expr_main_entry_int";
                    #[cfg(not(windows))]
                    let int_symbol = &int_result_symbol_name(&func_def.name);
                    let int_id = declare_zero_arg_int_result_sig(
                        &mut cranelift_module,
                        &isa,
                        int_symbol,
                        if cfg!(windows) {
                            Linkage::Export
                        } else {
                            Linkage::Local
                        },
                    );
                    expr_main_int_id = Some(int_id);
                }
            } else {
                let internal_id = declare_internal_function_sig(
                    &mut cranelift_module,
                    &isa,
                    func_def,
                    closure_metadata.contains_key(&func_def.name),
                    &internal_symbol_name(&func_def.name),
                    Linkage::Local,
                );
                internal_func_ids.insert(func_def.name.clone(), internal_id);
            }
        }
        for func_def in &self.functions {
            define_function_body(
                &mut cranelift_module,
                isa.clone(),
                &flags,
                func_def,
                internal_func_ids[&func_def.name],
                &internal_func_ids,
                &function_ordinals,
                &function_arities,
                &closure_metadata,
            );
            if func_def.name == "main" && func_def.inputs.is_empty() {
                define_zero_arg_int_result_wrapper(
                    &mut cranelift_module,
                    isa.clone(),
                    &flags,
                    expr_main_int_id.expect("main int wrapper id should exist"),
                    internal_func_ids[&func_def.name],
                );
            }
        }

        #[cfg(not(windows))]
        if let Some(id) = expr_main_int_id {
            generate_c_main(&mut cranelift_module, isa.clone(), &flags, id);
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

    #[cfg(feature = "llvm-backend")]
    fn compile_to_llvm_wasm(self, output: &Path) {
        let has_zero_arg_main = self
            .functions
            .iter()
            .any(|func| func.name == "main" && func.inputs.is_empty());
        assert!(
            has_zero_arg_main,
            "llvm wasm output requires a zero-argument main function"
        );

        let asm = llvm_backend::compile_to_wasm_assembly(self, "llvm_wasm");
        let asm_tmp = output.with_extension("s");
        let obj_tmp = output.with_extension("o");
        std::fs::write(&asm_tmp, &asm).unwrap();

        let assemble_status = Command::new(find_llvm_tool("llvm-mc"))
            .arg("-triple=wasm32-unknown-unknown")
            .arg("-filetype=obj")
            .arg(&asm_tmp)
            .arg("-o")
            .arg(&obj_tmp)
            .status()
            .expect("llvm-mc not found");
        assert!(
            assemble_status.success(),
            "wasm assembler failed with: {assemble_status}"
        );

        let status = Command::new(find_wasm_ld())
            .arg(&obj_tmp)
            .arg("--no-entry")
            .arg("--export=__expr_main_i64")
            .arg("--export-memory")
            .arg(format!("--initial-memory={}", 16 * 1024 * 1024))
            .arg("--no-growable-memory")
            .arg("--import-undefined")
            .arg("-o")
            .arg(output)
            .status()
            .expect("wasm-ld not found");

        if status.success() {
            std::fs::remove_file(&asm_tmp).ok();
            std::fs::remove_file(&obj_tmp).ok();
        } else {
            eprintln!(
                "keeping intermediate wasm files at {} and {}",
                asm_tmp.display(),
                obj_tmp.display()
            );
        }
        assert!(status.success(), "wasm linker failed with: {status}");
    }

    #[cfg(all(feature = "llvm-backend", feature = "wasi"))]
    fn compile_to_llvm_component(self, output: &Path) {
        let has_zero_arg_main = self
            .functions
            .iter()
            .any(|func| func.name == "main" && func.inputs.is_empty());
        assert!(
            has_zero_arg_main,
            "llvm component output requires a zero-argument main function"
        );

        let asm = llvm_backend::compile_to_wasm_preview1_command_assembly(self, "llvm_component");
        let asm_tmp = output.with_extension("component.s");
        let obj_tmp = output.with_extension("component.o");
        let core_tmp = output.with_extension("core.wasm");
        std::fs::write(&asm_tmp, &asm).unwrap();

        let assemble_status = Command::new(find_llvm_tool("llvm-mc"))
            .arg("-triple=wasm32-unknown-unknown")
            .arg("-filetype=obj")
            .arg(&asm_tmp)
            .arg("-o")
            .arg(&obj_tmp)
            .status()
            .expect("llvm-mc not found");
        assert!(
            assemble_status.success(),
            "wasm assembler failed with: {assemble_status}"
        );

        let link_status = Command::new(find_wasm_ld())
            .arg(&obj_tmp)
            .arg("--no-entry")
            .arg("--export=_start")
            .arg("--export-memory")
            .arg(format!("--initial-memory={}", 16 * 1024 * 1024))
            .arg("--import-undefined")
            .arg("-o")
            .arg(&core_tmp)
            .status()
            .expect("wasm-ld not found");
        assert!(
            link_status.success(),
            "wasm linker failed with: {link_status}"
        );

        let core_bytes = std::fs::read(&core_tmp).expect("failed to read intermediate core wasm");
        let component_bytes = wit_component::ComponentEncoder::default()
            .module(&core_bytes)
            .expect("failed to load core wasm into component encoder")
            .adapter(
                "wasi_snapshot_preview1",
                wasi_preview1_component_adapter_provider::WASI_SNAPSHOT_PREVIEW1_COMMAND_ADAPTER,
            )
            .expect("failed to attach wasi preview1 command adapter")
            .validate(true)
            .encode()
            .expect("failed to encode wasi component");
        std::fs::write(output, component_bytes).expect("failed to write component output");

        if output.exists() {
            std::fs::remove_file(&asm_tmp).ok();
            std::fs::remove_file(&obj_tmp).ok();
            std::fs::remove_file(&core_tmp).ok();
        }
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

    pub fn get_internal_fn_ptr(&self, name: &str) -> Option<*const u8> {
        match self {
            Self::Cranelift(module) => module.get_internal_fn_ptr(name),
            #[cfg(feature = "llvm-backend")]
            Self::Llvm(_) => None,
        }
    }

    pub fn get_int_result_fn_ptr(&self, name: &str) -> Option<*const u8> {
        match self {
            Self::Cranelift(module) => module.get_int_result_fn_ptr(name),
            #[cfg(feature = "llvm-backend")]
            Self::Llvm(module) => module.get_int_result_fn_ptr(name),
        }
    }
}

pub struct CraneliftJitModule {
    module: JITModule,
    func_ids: HashMap<String, FuncId>,
    internal_func_ids: HashMap<String, FuncId>,
    int_result_func_ids: HashMap<String, FuncId>,
}

impl CraneliftJitModule {
    pub fn get_fn_ptr(&self, name: &str) -> *const u8 {
        self.module.get_finalized_function(self.func_ids[name])
    }

    pub fn has_function(&self, name: &str) -> bool {
        self.func_ids.contains_key(name)
    }

    pub fn get_internal_fn_ptr(&self, name: &str) -> Option<*const u8> {
        self.internal_func_ids
            .get(name)
            .map(|id| self.module.get_finalized_function(*id))
    }

    pub fn get_int_result_fn_ptr(&self, name: &str) -> Option<*const u8> {
        self.int_result_func_ids
            .get(name)
            .map(|id| self.module.get_finalized_function(*id))
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

#[cfg(all(not(windows), feature = "llvm-backend"))]
fn write_unix_rust_wrapper(output: &Path) -> std::path::PathBuf {
    let wrapper = output.with_extension("wrapper.rs");
    let source = include_str!("./wrapper/unix.rs");
    std::fs::write(&wrapper, source).unwrap();
    wrapper
}

fn is_wasm_output(output: &Path) -> bool {
    output
        .extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("wasm"))
}

fn is_component_wasm_output(output: &Path) -> bool {
    output
        .file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.ends_with(".component.wasm"))
}

#[cfg(feature = "llvm-backend")]
fn find_wasm_ld() -> std::path::PathBuf {
    find_llvm_tool("wasm-ld")
}

#[cfg(feature = "llvm-backend")]
fn find_llvm_tool(tool: &str) -> std::path::PathBuf {
    #[cfg(windows)]
    let exe_name = format!("{tool}.exe");
    #[cfg(not(windows))]
    let exe_name = tool.to_string();

    let env_name = tool.replace('-', "_").to_ascii_uppercase();
    if let Some(path) = std::env::var_os(&env_name) {
        return path.into();
    }

    if let Some(path) = std::env::var_os("WASM_LD") {
        if tool == "wasm-ld" {
            return path.into();
        }
    }

    if let Some(prefix) = std::env::var_os("LLVM_SYS_201_PREFIX") {
        let mut candidate = std::path::PathBuf::from(prefix);
        candidate.push("bin");
        candidate.push(&exe_name);
        if candidate.exists() {
            return candidate;
        }
    }

    std::path::PathBuf::from(exe_name)
}

fn declare_internal_function_sig(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    func_def: &FunctionDefAst,
    _has_closure_metadata: bool,
    name: &str,
    linkage: Linkage,
) -> FuncId {
    let mut sig = Signature::new(isa.default_call_conv());
    sig.returns.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    for _ in &func_def.inputs {
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
    }
    module.declare_function(name, linkage, &sig).unwrap()
}

fn internal_symbol_name(name: &str) -> String {
    format!("__expr_internal_{name}")
}

fn int_result_symbol_name(name: &str) -> String {
    format!("__expr_i64_{name}")
}

fn declare_zero_arg_int_result_sig(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    name: &str,
    linkage: Linkage,
) -> FuncId {
    let mut sig = Signature::new(isa.default_call_conv());
    sig.returns.push(AbiParam::new(types::I64));
    module.declare_function(name, linkage, &sig).unwrap()
}

fn function_ordinals(functions: &[FunctionDefAst]) -> HashMap<String, i64> {
    functions
        .iter()
        .enumerate()
        .map(|(index, func)| {
            (
                func.name.clone(),
                i64::try_from(index).expect("too many functions to assign ordinals"),
            )
        })
        .collect()
}

fn function_arities(functions: &[FunctionDefAst]) -> HashMap<String, usize> {
    functions
        .iter()
        .map(|func| (func.name.clone(), func.inputs.len()))
        .collect()
}

fn define_function_body(
    module: &mut impl CraneliftModule,
    isa: OwnedTargetIsa,
    flags: &settings::Flags,
    func_def: &FunctionDefAst,
    func_id: FuncId,
    all_funcs: &HashMap<String, FuncId>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
) -> String {
    let mut sig = Signature::new(isa.default_call_conv());
    sig.returns.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    for _ in &func_def.inputs {
        sig.params.push(AbiParam::new(types::I64));
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
        let entry_block = builder.create_block();
        let loop_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.append_block_params_for_function_params(loop_block);
        builder.switch_to_block(entry_block);
        let entry_params = builder.block_params(entry_block).to_vec();
        let loop_args: Vec<_> = entry_params.iter().copied().map(BlockArg::Value).collect();
        builder.ins().jump(loop_block, &loop_args);
        builder.seal_block(entry_block);

        builder.switch_to_block(loop_block);
        let env_ptr = builder.block_params(loop_block)[0];

        let mut vars: HashMap<String, LocalValueVar> = HashMap::new();
        let capture_slots: HashMap<String, usize> = closure_metadata
            .get(&func_def.name)
            .map(|metadata| {
                metadata
                    .captures
                    .iter()
                    .enumerate()
                    .map(|(index, name)| (name.clone(), index))
                    .collect()
            })
            .unwrap_or_default();
        for (i, name) in func_def.inputs.iter().enumerate() {
            let tag = builder.declare_var(types::I64);
            let payload = builder.declare_var(types::I64);
            let param_tag = builder.block_params(loop_block)[i * 2 + 1];
            let param_payload = builder.block_params(loop_block)[i * 2 + 2];
            builder.def_var(tag, param_tag);
            builder.def_var(payload, param_payload);
            vars.insert(name.clone(), LocalValueVar { tag, payload });
        }
        for name in local_var_names(&func_def.block) {
            if !vars.contains_key(&name) {
                vars.insert(
                    name,
                    LocalValueVar {
                        tag: builder.declare_var(types::I64),
                        payload: builder.declare_var(types::I64),
                    },
                );
            }
        }

        compile_tail_block(
            &mut builder,
            &func_def.block,
            &vars,
            &func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            &capture_slots,
            env_ptr,
            &func_def.name,
            loop_block,
        );

        builder.seal_block(loop_block);
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

fn define_zero_arg_int_result_wrapper(
    module: &mut impl CraneliftModule,
    isa: OwnedTargetIsa,
    flags: &settings::Flags,
    wrapper_id: FuncId,
    internal_id: FuncId,
) {
    let mut sig = Signature::new(isa.default_call_conv());
    sig.returns.push(AbiParam::new(types::I64));

    let mut ctx = module.make_context();
    ctx.func.signature = sig;
    ctx.func.name = UserFuncName::user(0, wrapper_id.as_u32());

    let internal_ref = module.declare_func_in_func(internal_id, &mut ctx.func);

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
        let block0 = builder.create_block();
        builder.switch_to_block(block0);
        builder.seal_block(block0);

        let zero_env = builder.ins().iconst(types::I64, 0);
        let internal_call = builder.ins().call(internal_ref, &[zero_env]);
        let result_tag = builder.inst_results(internal_call)[0];
        let result_payload = builder.inst_results(internal_call)[1];
        let is_int = builder.ins().icmp_imm(IntCC::Equal, result_tag, TAG_INT);
        builder
            .ins()
            .trapz(is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);
        builder.ins().return_(&[result_payload]);
        builder.finalize();
    }

    let res = verify_function(&ctx.func, flags);
    if let Err(errors) = res {
        panic!("{}", errors);
    }
    module.define_function(wrapper_id, &mut ctx).unwrap();
    module.clear_context(&mut ctx);
}

#[cfg(not(windows))]
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
        let int_result = builder.inst_results(call)[0];

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
                | "list_insert"
                | "list_len"
                | "list_get"
                | "list_set"
                | "list_swap"
                | "list_pop"
                | "list_copy"
                | "list_range"
                | "list_map"
                | "list_filter"
        )
}

fn lift_anonymous_functions(
    functions: Vec<FunctionDefAst>,
) -> (Vec<FunctionDefAst>, HashMap<String, ClosureMetadata>) {
    let mut lifter = LambdaLifter {
        next_id: 0,
        lifted: vec![],
        metadata: HashMap::new(),
    };
    let mut transformed = Vec::with_capacity(functions.len());
    for mut function in functions {
        let mut scope_names = function.inputs.clone();
        collect_var_names(&Ast::Block(function.block.clone()), &mut scope_names);
        lifter.lift_block(&mut function.block, &scope_names);
        transformed.push(function);
    }
    transformed.extend(lifter.lifted);
    (transformed, lifter.metadata)
}

impl LambdaLifter {
    fn fresh_name(&mut self) -> String {
        self.next_id += 1;
        format!("__lambda_{}", self.next_id)
    }

    fn lift_block(&mut self, block: &mut BlockAst, scope_names: &[String]) {
        for line in &mut block.lines {
            self.lift_ast(line, scope_names);
        }
    }

    fn lift_ast(&mut self, ast: &mut Ast, scope_names: &[String]) {
        match ast {
            Ast::Lambda { inputs, body } => {
                let mut lambda_scope = inputs.clone();
                collect_var_names(body, &mut lambda_scope);
                let capture_source = (**body).clone();
                let mut nested_scope = lambda_scope.clone();
                for name in scope_names {
                    if !nested_scope.contains(name) {
                        nested_scope.push(name.clone());
                    }
                }
                self.lift_ast(body, &nested_scope);
                let captures = collect_captures(&capture_source, &lambda_scope, scope_names);
                let name = self.fresh_name();
                self.metadata
                    .insert(name.clone(), ClosureMetadata { captures });
                self.lifted.push(FunctionDefAst {
                    name: name.clone(),
                    inputs: inputs.clone(),
                    output: None,
                    block: BlockAst {
                        lines: vec![(**body).clone()],
                    },
                });
                *ast = Ast::FunctionRef(name);
            }
            Ast::Block(block) => self.lift_block(block, scope_names),
            Ast::Expression(ExpressionAst { args, .. }) => {
                for arg in args {
                    self.lift_ast(arg, scope_names);
                }
            }
            Ast::ListLiteral(items) => {
                for item in items {
                    self.lift_ast(item, scope_names);
                }
            }
            Ast::Index { collection, index } => {
                self.lift_ast(collection, scope_names);
                self.lift_ast(index, scope_names);
            }
            Ast::IndexAssign {
                collection,
                index,
                value,
            } => {
                self.lift_ast(collection, scope_names);
                self.lift_ast(index, scope_names);
                self.lift_ast(value, scope_names);
            }
            Ast::Assign { value, .. } => self.lift_ast(value, scope_names),
            Ast::If {
                condition,
                then,
                else_,
            } => {
                self.lift_ast(condition, scope_names);
                self.lift_block(then, scope_names);
                if let Some(else_block) = else_ {
                    self.lift_block(else_block, scope_names);
                }
            }
            Ast::FunctionDef(_) => panic!("nested function definitions are not supported"),
            Ast::Literal(_) | Ast::Variable(_) | Ast::FunctionRef(_) => {}
        }
    }
}

fn collect_captures(ast: &Ast, local_names: &[String], scope_names: &[String]) -> Vec<String> {
    let mut captures = Vec::new();
    collect_captures_into(ast, local_names, scope_names, &mut captures);
    captures
}

fn collect_captures_into(
    ast: &Ast,
    local_names: &[String],
    scope_names: &[String],
    captures: &mut Vec<String>,
) {
    match ast {
        Ast::Variable(name) => {
            if !local_names.contains(name) && scope_names.contains(name) && !captures.contains(name)
            {
                captures.push(name.clone());
            }
        }
        Ast::Expression(ExpressionAst { args, .. }) => {
            for arg in args {
                collect_captures_into(arg, local_names, scope_names, captures);
            }
        }
        Ast::ListLiteral(items) => {
            for item in items {
                collect_captures_into(item, local_names, scope_names, captures);
            }
        }
        Ast::Index { collection, index } => {
            collect_captures_into(collection, local_names, scope_names, captures);
            collect_captures_into(index, local_names, scope_names, captures);
        }
        Ast::IndexAssign {
            collection,
            index,
            value,
        } => {
            collect_captures_into(collection, local_names, scope_names, captures);
            collect_captures_into(index, local_names, scope_names, captures);
            collect_captures_into(value, local_names, scope_names, captures);
        }
        Ast::If {
            condition,
            then,
            else_,
        } => {
            collect_captures_into(condition, local_names, scope_names, captures);
            for line in &then.lines {
                collect_captures_into(line, local_names, scope_names, captures);
            }
            if let Some(else_block) = else_ {
                for line in &else_block.lines {
                    collect_captures_into(line, local_names, scope_names, captures);
                }
            }
        }
        Ast::Lambda { inputs, body } => {
            let mut nested_local_names = local_names.to_vec();
            for input in inputs {
                if !nested_local_names.contains(input) {
                    nested_local_names.push(input.clone());
                }
            }
            collect_var_names(body, &mut nested_local_names);
            collect_captures_into(body, &nested_local_names, scope_names, captures);
        }
        Ast::Block(block) => {
            for line in &block.lines {
                collect_captures_into(line, local_names, scope_names, captures);
            }
        }
        Ast::Literal(_) | Ast::FunctionRef(_) => {}
        Ast::Assign { value, .. } => {
            collect_captures_into(value, local_names, scope_names, captures);
        }
        Ast::FunctionDef(_) => panic!("nested function definitions are not supported"),
    }
}

fn collect_var_names(ast: &Ast, names: &mut Vec<String>) {
    match ast {
        Ast::Lambda { body, .. } => {
            collect_var_names(body, names);
        }
        Ast::Block(block) => {
            for line in &block.lines {
                collect_var_names(line, names);
            }
        }
        Ast::FunctionRef(_) => {}
        Ast::Assign { name, value } => {
            if !names.contains(name) {
                names.push(name.clone());
            }
            collect_var_names(value, names);
        }
        Ast::IndexAssign {
            collection,
            index,
            value,
        } => {
            collect_var_names(collection, names);
            collect_var_names(index, names);
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

fn call_unary_scalar(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    name: &str,
    arg: CompiledValue,
) -> Value {
    let func_ref = require_func(func_refs, name);
    let call = builder.ins().call(func_ref, &[arg.tag, arg.payload]);
    builder.inst_results(call)[0]
}

fn call_binary(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    name: &str,
    lhs: CompiledValue,
    rhs: CompiledValue,
) -> CompiledValue {
    let func_ref = require_func(func_refs, name);
    let call = builder
        .ins()
        .call(func_ref, &[lhs.tag, lhs.payload, rhs.tag, rhs.payload]);
    let results = builder.inst_results(call);
    CompiledValue {
        tag: results[0],
        payload: results[1],
    }
}

fn call_ternary(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    name: &str,
    a: CompiledValue,
    b: CompiledValue,
    c: CompiledValue,
) -> CompiledValue {
    let func_ref = require_func(func_refs, name);
    let call = builder.ins().call(
        func_ref,
        &[a.tag, a.payload, b.tag, b.payload, c.tag, c.payload],
    );
    let results = builder.inst_results(call);
    CompiledValue {
        tag: results[0],
        payload: results[1],
    }
}

fn boxed_int_const(builder: &mut FunctionBuilder, value: i64) -> CompiledValue {
    CompiledValue {
        tag: builder.ins().iconst(types::I64, TAG_INT),
        payload: builder.ins().iconst(types::I64, value),
    }
}

fn load_value_from_env(
    builder: &mut FunctionBuilder,
    env_ptr: Value,
    slot: usize,
) -> CompiledValue {
    let slot_offset = i32::try_from(i64::try_from(slot).unwrap() * VALUE_SIZE)
        .expect("closure env offset overflow");
    let tag_i8 = builder
        .ins()
        .load(types::I8, MemFlags::new(), env_ptr, slot_offset);
    let tag = builder.ins().uextend(types::I64, tag_i8);
    let payload = builder.ins().load(
        types::I64,
        MemFlags::new(),
        env_ptr,
        slot_offset + VALUE_PAYLOAD_OFFSET,
    );
    CompiledValue { tag, payload }
}

fn allocate_closure_for_function(
    builder: &mut FunctionBuilder,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_name: &str,
    function_ordinals: &HashMap<String, i64>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
) -> CompiledValue {
    let alloc_ref = require_func(func_refs, "__alloc");
    let metadata = closure_metadata.get(function_name);
    let captures = metadata.map(|m| m.captures.as_slice()).unwrap_or(&[]);

    let env_raw = if captures.is_empty() {
        builder.ins().iconst(types::I64, 0)
    } else {
        let env_bytes = i64::try_from(captures.len())
            .expect("too many closure captures")
            .checked_mul(VALUE_SIZE)
            .expect("closure env size overflow");
        let env_size = builder.ins().iconst(types::I64, env_bytes);
        let env_align = builder
            .ins()
            .iconst(types::I64, std::mem::align_of::<i64>() as i64);
        let env_call = builder.ins().call(alloc_ref, &[env_size, env_align]);
        let env_ptr_raw = builder.inst_results(env_call)[0];
        for (index, capture_name) in captures.iter().enumerate() {
            let capture_value = resolve_named_value(
                builder,
                capture_name,
                vars,
                func_refs,
                function_ordinals,
                closure_metadata,
                capture_slots,
                env_ptr,
            );
            let slot_offset = i32::try_from(i64::try_from(index).unwrap() * VALUE_SIZE)
                .expect("closure env offset overflow");
            let tag_i8 = builder.ins().ireduce(types::I8, capture_value.tag);
            builder
                .ins()
                .store(MemFlags::new(), tag_i8, env_ptr_raw, slot_offset);
            builder.ins().store(
                MemFlags::new(),
                capture_value.payload,
                env_ptr_raw,
                slot_offset + VALUE_PAYLOAD_OFFSET,
            );
        }
        env_ptr_raw
    };

    let closure_size = builder.ins().iconst(types::I64, CLOSURE_SIZE);
    let closure_align = builder
        .ins()
        .iconst(types::I64, std::mem::align_of::<i64>() as i64);
    let closure_call = builder
        .ins()
        .call(alloc_ref, &[closure_size, closure_align]);
    let closure_ptr = builder.inst_results(closure_call)[0];
    let ordinal = *function_ordinals.get(function_name).unwrap_or_else(|| {
        panic!("missing function ordinal for function reference: {function_name}")
    });
    let ordinal_value = builder.ins().iconst(types::I64, ordinal);
    builder.ins().store(
        MemFlags::new(),
        ordinal_value,
        closure_ptr,
        CLOSURE_FUNCTION_ORDINAL_OFFSET,
    );
    builder.ins().store(
        MemFlags::new(),
        env_raw,
        closure_ptr,
        CLOSURE_ENV_PTR_OFFSET,
    );
    CompiledValue {
        tag: builder.ins().iconst(types::I64, TAG_FUNCTION),
        payload: closure_ptr,
    }
}

fn resolve_named_value(
    builder: &mut FunctionBuilder,
    name: &str,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
) -> CompiledValue {
    if let Some(var) = vars.get(name) {
        CompiledValue {
            tag: builder.use_var(var.tag),
            payload: builder.use_var(var.payload),
        }
    } else if let Some(&slot) = capture_slots.get(name) {
        load_value_from_env(builder, env_ptr, slot)
    } else if function_ordinals.contains_key(name) {
        allocate_closure_for_function(
            builder,
            vars,
            func_refs,
            name,
            function_ordinals,
            closure_metadata,
            capture_slots,
            env_ptr,
        )
    } else {
        panic!("undefined variable: {name}");
    }
}

fn expect_int_payload(builder: &mut FunctionBuilder, value: CompiledValue) -> Value {
    let is_int = builder.ins().icmp_imm(IntCC::Equal, value.tag, TAG_INT);
    builder
        .ins()
        .trapz(is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);
    value.payload
}

fn compile_list_literal(
    builder: &mut FunctionBuilder,
    items: &[Ast],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
) -> CompiledValue {
    let list_new_ref = *func_refs
        .get("list_new")
        .expect("builtin function 'list_new' is missing");
    let create_call = builder.ins().call(list_new_ref, &[]);
    let created = builder.inst_results(create_call);
    let handle = CompiledValue {
        tag: created[0],
        payload: created[1],
    };

    for item in items {
        let value = compile_ast(
            builder,
            item,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
        );
        let _ = call_binary(builder, func_refs, "list_push", handle, value);
    }

    handle
}

fn create_empty_list(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
) -> CompiledValue {
    let list_new_ref = *func_refs
        .get("list_new")
        .expect("builtin function 'list_new' is missing");
    let create_call = builder.ins().call(list_new_ref, &[]);
    let created = builder.inst_results(create_call);
    CompiledValue {
        tag: created[0],
        payload: created[1],
    }
}

fn validate_unary_callback_ast(
    ast: &Ast,
    vars: &HashMap<String, LocalValueVar>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    builtin: &str,
) {
    match ast {
        Ast::FunctionRef(name) => {
            if function_arities.get(name) != Some(&1usize) {
                panic!("{builtin} callback must take exactly 1 argument");
            }
        }
        Ast::Variable(name) if !vars.contains_key(name) && function_ordinals.contains_key(name) => {
            if function_arities.get(name) != Some(&1usize) {
                panic!("{builtin} callback must take exactly 1 argument");
            }
        }
        _ => {}
    }
}

fn call_unary(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    name: &str,
    arg: CompiledValue,
) -> CompiledValue {
    let func_ref = require_func(func_refs, name);
    let call = builder.ins().call(func_ref, &[arg.tag, arg.payload]);
    let results = builder.inst_results(call);
    CompiledValue {
        tag: results[0],
        payload: results[1],
    }
}

fn call_named_with_env(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    name: &str,
    env_ptr: Value,
    args: &[CompiledValue],
) -> CompiledValue {
    let func_ref = *func_refs
        .get(name)
        .unwrap_or_else(|| panic!("function '{name}' is missing"));
    let mut call_args = Vec::with_capacity(1 + args.len() * 2);
    call_args.push(env_ptr);
    for arg in args {
        call_args.push(arg.tag);
        call_args.push(arg.payload);
    }
    let call = builder.ins().call(func_ref, &call_args);
    let results = builder.inst_results(call);
    CompiledValue {
        tag: results[0],
        payload: results[1],
    }
}

fn apply_function_value(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    function_value: CompiledValue,
    args: &[CompiledValue],
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
) -> CompiledValue {
    let is_function = builder
        .ins()
        .icmp_imm(IntCC::Equal, function_value.tag, TAG_FUNCTION);
    builder
        .ins()
        .trapz(is_function, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let closure_ptr = function_value.payload;
    let closure_ordinal = builder.ins().load(
        types::I64,
        MemFlags::new(),
        closure_ptr,
        CLOSURE_FUNCTION_ORDINAL_OFFSET,
    );
    let closure_env_ptr = builder.ins().load(
        types::I64,
        MemFlags::new(),
        closure_ptr,
        CLOSURE_ENV_PTR_OFFSET,
    );

    let mut candidates: Vec<_> = function_ordinals
        .iter()
        .filter_map(|(name, &ordinal)| {
            (function_arities.get(name) == Some(&args.len())).then_some((ordinal, name.as_str()))
        })
        .collect();
    candidates.sort_by_key(|(ordinal, _)| *ordinal);
    if candidates.is_empty() {
        panic!("no unary functions are available for higher-order list builtins");
    }

    let entry_check = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, types::I64);
    builder.append_block_param(merge_block, types::I64);
    builder.ins().jump(entry_check, &[]);

    let mut check_block = entry_check;
    for (index, (ordinal, name)) in candidates.iter().enumerate() {
        builder.switch_to_block(check_block);
        let matched = builder
            .ins()
            .icmp_imm(IntCC::Equal, closure_ordinal, *ordinal);
        let call_block = builder.create_block();
        let next_block = if index + 1 == candidates.len() {
            None
        } else {
            Some(builder.create_block())
        };
        match next_block {
            Some(next) => {
                builder.ins().brif(matched, call_block, &[], next, &[]);
            }
            None => {
                builder
                    .ins()
                    .trapz(matched, TrapCode::BAD_CONVERSION_TO_INTEGER);
                builder.ins().jump(call_block, &[]);
            }
        }

        builder.switch_to_block(call_block);
        let result = call_named_with_env(builder, func_refs, name, closure_env_ptr, args);
        builder.ins().jump(
            merge_block,
            &[BlockArg::Value(result.tag), BlockArg::Value(result.payload)],
        );
        builder.seal_block(call_block);
        builder.seal_block(check_block);

        if let Some(next) = next_block {
            check_block = next;
        }
    }

    builder.switch_to_block(merge_block);
    builder.seal_block(merge_block);
    let params = builder.block_params(merge_block);
    CompiledValue {
        tag: params[0],
        payload: params[1],
    }
}

fn compile_list_map(
    builder: &mut FunctionBuilder,
    args: &[Ast],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
) -> CompiledValue {
    assert_eq!(args.len(), 2, "list_map expects 2 arguments");
    validate_unary_callback_ast(
        &args[1],
        vars,
        function_ordinals,
        function_arities,
        "list_map",
    );
    let input = compile_ast(
        builder,
        &args[0],
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
    );
    let callback = compile_ast(
        builder,
        &args[1],
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
    );
    let output = create_empty_list(builder, func_refs);
    let len = call_unary(builder, func_refs, "list_len", input);

    let loop_block = builder.create_block();
    let body_block = builder.create_block();
    let exit_block = builder.create_block();
    builder.append_block_param(loop_block, types::I64);

    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(loop_block, &[BlockArg::Value(zero)]);

    builder.switch_to_block(loop_block);
    let idx = builder.block_params(loop_block)[0];
    let has_more = builder
        .ins()
        .icmp(IntCC::UnsignedLessThan, idx, len.payload);
    builder
        .ins()
        .brif(has_more, body_block, &[], exit_block, &[]);

    builder.switch_to_block(body_block);
    let index_value = CompiledValue {
        tag: builder.ins().iconst(types::I64, TAG_INT),
        payload: idx,
    };
    let item = call_binary(builder, func_refs, "list_get", input, index_value);
    let mapped = apply_function_value(
        builder,
        func_refs,
        callback,
        &[item],
        function_ordinals,
        function_arities,
    );
    let _ = call_binary(builder, func_refs, "list_push", output, mapped);
    let next = builder.ins().iadd_imm(idx, 1);
    builder.ins().jump(loop_block, &[BlockArg::Value(next)]);

    builder.switch_to_block(exit_block);
    builder.seal_block(loop_block);
    builder.seal_block(body_block);
    builder.seal_block(exit_block);
    output
}

fn compile_list_filter(
    builder: &mut FunctionBuilder,
    args: &[Ast],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
) -> CompiledValue {
    assert_eq!(args.len(), 2, "list_filter expects 2 arguments");
    validate_unary_callback_ast(
        &args[1],
        vars,
        function_ordinals,
        function_arities,
        "list_filter",
    );
    let input = compile_ast(
        builder,
        &args[0],
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
    );
    let callback = compile_ast(
        builder,
        &args[1],
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
    );
    let output = create_empty_list(builder, func_refs);
    let len = call_unary(builder, func_refs, "list_len", input);

    let loop_block = builder.create_block();
    let body_block = builder.create_block();
    let push_block = builder.create_block();
    let skip_block = builder.create_block();
    let continue_block = builder.create_block();
    let exit_block = builder.create_block();
    builder.append_block_param(loop_block, types::I64);

    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(loop_block, &[BlockArg::Value(zero)]);

    builder.switch_to_block(loop_block);
    let idx = builder.block_params(loop_block)[0];
    let has_more = builder
        .ins()
        .icmp(IntCC::UnsignedLessThan, idx, len.payload);
    builder
        .ins()
        .brif(has_more, body_block, &[], exit_block, &[]);

    builder.switch_to_block(body_block);
    let index_value = CompiledValue {
        tag: builder.ins().iconst(types::I64, TAG_INT),
        payload: idx,
    };
    let item = call_binary(builder, func_refs, "list_get", input, index_value);
    let predicate = apply_function_value(
        builder,
        func_refs,
        callback,
        &[item],
        function_ordinals,
        function_arities,
    );
    let truth = call_unary_scalar(builder, func_refs, "__value_is_truthy", predicate);
    let keep = builder.ins().icmp_imm(IntCC::NotEqual, truth, 0);
    builder.ins().brif(keep, push_block, &[], skip_block, &[]);

    builder.switch_to_block(push_block);
    let _ = call_binary(builder, func_refs, "list_push", output, item);
    builder.ins().jump(continue_block, &[]);

    builder.switch_to_block(skip_block);
    builder.ins().jump(continue_block, &[]);

    builder.switch_to_block(continue_block);
    let next = builder.ins().iadd_imm(idx, 1);
    builder.ins().jump(loop_block, &[BlockArg::Value(next)]);

    builder.switch_to_block(exit_block);
    builder.seal_block(loop_block);
    builder.seal_block(body_block);
    builder.seal_block(push_block);
    builder.seal_block(skip_block);
    builder.seal_block(continue_block);
    builder.seal_block(exit_block);
    output
}

fn compile_list_range(
    builder: &mut FunctionBuilder,
    args: &[Ast],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
) -> CompiledValue {
    assert_eq!(args.len(), 2, "list_range expects 2 arguments");
    let start_value = compile_ast(
        builder,
        &args[0],
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
    );
    let end_value = compile_ast(
        builder,
        &args[1],
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
    );
    let start = expect_int_payload(builder, start_value);
    let end = expect_int_payload(builder, end_value);
    let output = create_empty_list(builder, func_refs);

    let loop_block = builder.create_block();
    let body_block = builder.create_block();
    let exit_block = builder.create_block();
    builder.append_block_param(loop_block, types::I64);

    builder.ins().jump(loop_block, &[BlockArg::Value(start)]);

    builder.switch_to_block(loop_block);
    let current = builder.block_params(loop_block)[0];
    let has_more = builder.ins().icmp(IntCC::SignedLessThan, current, end);
    builder
        .ins()
        .brif(has_more, body_block, &[], exit_block, &[]);

    builder.switch_to_block(body_block);
    let current_value = CompiledValue {
        tag: builder.ins().iconst(types::I64, TAG_INT),
        payload: current,
    };
    let _ = call_binary(builder, func_refs, "list_push", output, current_value);
    let next = builder.ins().iadd_imm(current, 1);
    builder.ins().jump(loop_block, &[BlockArg::Value(next)]);

    builder.switch_to_block(exit_block);
    builder.seal_block(loop_block);
    builder.seal_block(body_block);
    builder.seal_block(exit_block);
    output
}

fn compile_tail_block(
    builder: &mut FunctionBuilder,
    block: &BlockAst,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    current_function_name: &str,
    loop_block: Block,
) {
    if block.lines.is_empty() {
        let zero = boxed_int_const(builder, 0);
        builder.ins().return_(&[zero.tag, zero.payload]);
        return;
    }

    for line in &block.lines[..block.lines.len() - 1] {
        let _ = compile_ast(
            builder,
            line,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
        );
    }

    compile_tail_ast(
        builder,
        &block.lines[block.lines.len() - 1],
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        current_function_name,
        loop_block,
    );
}

fn compile_tail_ast(
    builder: &mut FunctionBuilder,
    ast: &Ast,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    current_function_name: &str,
    loop_block: Block,
) {
    match ast {
        Ast::Expression(ExpressionAst { function, args })
            if function == current_function_name && !is_builtin_name(function) =>
        {
            let compiled_args: Vec<_> = args
                .iter()
                .map(|arg| {
                    compile_ast(
                        builder,
                        arg,
                        vars,
                        func_refs,
                        function_ordinals,
                        function_arities,
                        closure_metadata,
                        capture_slots,
                        env_ptr,
                    )
                })
                .collect();
            let mut jump_args = Vec::with_capacity(1 + compiled_args.len() * 2);
            jump_args.push(BlockArg::Value(env_ptr));
            for value in compiled_args {
                jump_args.push(BlockArg::Value(value.tag));
                jump_args.push(BlockArg::Value(value.payload));
            }
            builder.ins().jump(loop_block, &jump_args);
        }
        Ast::If {
            condition,
            then,
            else_,
        } => {
            let cond_val = compile_ast(
                builder,
                condition,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
            );
            let truth_value = call_unary_scalar(builder, func_refs, "__value_is_truthy", cond_val);
            let cond_non_zero = builder.ins().icmp_imm(IntCC::NotEqual, truth_value, 0);

            let then_block = builder.create_block();
            let else_block = builder.create_block();
            builder
                .ins()
                .brif(cond_non_zero, then_block, &[], else_block, &[]);

            builder.switch_to_block(then_block);
            compile_tail_block(
                builder,
                then,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                current_function_name,
                loop_block,
            );
            builder.seal_block(then_block);

            builder.switch_to_block(else_block);
            if let Some(else_block_ast) = else_ {
                compile_tail_block(
                    builder,
                    else_block_ast,
                    vars,
                    func_refs,
                    function_ordinals,
                    function_arities,
                    closure_metadata,
                    capture_slots,
                    env_ptr,
                    current_function_name,
                    loop_block,
                );
            } else {
                let zero = boxed_int_const(builder, 0);
                builder.ins().return_(&[zero.tag, zero.payload]);
            }
            builder.seal_block(else_block);
        }
        Ast::Block(block) => compile_tail_block(
            builder,
            block,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            current_function_name,
            loop_block,
        ),
        _ => {
            let val = compile_ast(
                builder,
                ast,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
            );
            builder.ins().return_(&[val.tag, val.payload]);
        }
    }
}

fn compile_ast(
    builder: &mut FunctionBuilder,
    ast: &Ast,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
) -> CompiledValue {
    match ast {
        Ast::Literal(LiteralAst::Integer(n)) => boxed_int_const(builder, *n),
        Ast::Lambda { .. } => {
            panic!("anonymous functions are not implemented by the compiler yet");
        }
        Ast::FunctionRef(name) => allocate_closure_for_function(
            builder,
            vars,
            func_refs,
            name,
            function_ordinals,
            closure_metadata,
            capture_slots,
            env_ptr,
        ),
        Ast::ListLiteral(items) => compile_list_literal(
            builder,
            items,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
        ),
        Ast::Index { collection, index } => {
            let collection_value = compile_ast(
                builder,
                collection,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
            );
            let index_value = compile_ast(
                builder,
                index,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
            );
            call_binary(
                builder,
                func_refs,
                "list_get",
                collection_value,
                index_value,
            )
        }
        Ast::IndexAssign {
            collection,
            index,
            value,
        } => {
            let collection_value = compile_ast(
                builder,
                collection,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
            );
            let index_value = compile_ast(
                builder,
                index,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
            );
            let value = compile_ast(
                builder,
                value,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
            );
            call_ternary(
                builder,
                func_refs,
                "list_set",
                collection_value,
                index_value,
                value,
            )
        }
        Ast::Expression(ExpressionAst { function, args }) => {
            if function == "list_map" {
                return compile_list_map(
                    builder,
                    args,
                    vars,
                    func_refs,
                    function_ordinals,
                    function_arities,
                    closure_metadata,
                    capture_slots,
                    env_ptr,
                );
            }
            if function == "list_filter" {
                return compile_list_filter(
                    builder,
                    args,
                    vars,
                    func_refs,
                    function_ordinals,
                    function_arities,
                    closure_metadata,
                    capture_slots,
                    env_ptr,
                );
            }
            if function == "list_range" {
                return compile_list_range(
                    builder,
                    args,
                    vars,
                    func_refs,
                    function_ordinals,
                    function_arities,
                    closure_metadata,
                    capture_slots,
                    env_ptr,
                );
            }
            let compiled: Vec<_> = args
                .iter()
                .map(|arg| {
                    compile_ast(
                        builder,
                        arg,
                        vars,
                        func_refs,
                        function_ordinals,
                        function_arities,
                        closure_metadata,
                        capture_slots,
                        env_ptr,
                    )
                })
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
                    if vars.contains_key(name) || capture_slots.contains_key(name) {
                        let callee = resolve_named_value(
                            builder,
                            name,
                            vars,
                            func_refs,
                            function_ordinals,
                            closure_metadata,
                            capture_slots,
                            env_ptr,
                        );
                        return apply_function_value(
                            builder,
                            func_refs,
                            callee,
                            &compiled,
                            function_ordinals,
                            function_arities,
                        );
                    }
                    if function_ordinals.contains_key(name) {
                        let zero_env = builder.ins().iconst(types::I64, 0);
                        return call_named_with_env(builder, func_refs, name, zero_env, &compiled);
                    }
                    if let Some(func_ref) = func_refs.get(name) {
                        let mut args = Vec::with_capacity(compiled.len() * 2);
                        for value in &compiled {
                            args.push(value.tag);
                            args.push(value.payload);
                        }
                        let call = builder.ins().call(*func_ref, &args);
                        let results = builder.inst_results(call);
                        return CompiledValue {
                            tag: results[0],
                            payload: results[1],
                        };
                    }
                    panic!("undefined function: {name}");
                }
            }
        }
        Ast::Block(block) => {
            let mut last = None;
            for line in &block.lines {
                last = Some(compile_ast(
                    builder,
                    line,
                    vars,
                    func_refs,
                    function_ordinals,
                    function_arities,
                    closure_metadata,
                    capture_slots,
                    env_ptr,
                ));
            }
            last.expect("empty block")
        }
        Ast::Variable(name) => resolve_named_value(
            builder,
            name,
            vars,
            func_refs,
            function_ordinals,
            closure_metadata,
            capture_slots,
            env_ptr,
        ),
        Ast::Assign { name, value } => {
            let val = compile_ast(
                builder,
                value,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
            );
            let var = vars
                .get(name)
                .unwrap_or_else(|| panic!("undeclared variable: {name}"));
            builder.def_var(var.tag, val.tag);
            builder.def_var(var.payload, val.payload);
            val
        }
        Ast::If {
            condition,
            then,
            else_,
        } => {
            let cond_val = compile_ast(
                builder,
                condition,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
            );
            let truth_value = call_unary_scalar(builder, func_refs, "__value_is_truthy", cond_val);
            let cond_non_zero = builder.ins().icmp_imm(IntCC::NotEqual, truth_value, 0);

            let then_block = builder.create_block();
            let merge_block = builder.create_block();
            builder.append_block_param(merge_block, types::I64);
            builder.append_block_param(merge_block, types::I64);

            if let Some(else_block_ast) = else_ {
                let else_block = builder.create_block();
                builder
                    .ins()
                    .brif(cond_non_zero, then_block, &[], else_block, &[]);

                builder.switch_to_block(then_block);
                builder.seal_block(then_block);
                let mut then_val = boxed_int_const(builder, 0);
                for line in &then.lines {
                    then_val = compile_ast(
                        builder,
                        line,
                        vars,
                        func_refs,
                        function_ordinals,
                        function_arities,
                        closure_metadata,
                        capture_slots,
                        env_ptr,
                    );
                }
                builder.ins().jump(
                    merge_block,
                    &[
                        BlockArg::Value(then_val.tag),
                        BlockArg::Value(then_val.payload),
                    ],
                );

                builder.switch_to_block(else_block);
                builder.seal_block(else_block);
                let mut else_val = boxed_int_const(builder, 0);
                for line in &else_block_ast.lines {
                    else_val = compile_ast(
                        builder,
                        line,
                        vars,
                        func_refs,
                        function_ordinals,
                        function_arities,
                        closure_metadata,
                        capture_slots,
                        env_ptr,
                    );
                }
                builder.ins().jump(
                    merge_block,
                    &[
                        BlockArg::Value(else_val.tag),
                        BlockArg::Value(else_val.payload),
                    ],
                );
            } else {
                let boxed_zero = boxed_int_const(builder, 0);
                builder.ins().brif(
                    cond_non_zero,
                    then_block,
                    &[],
                    merge_block,
                    &[
                        BlockArg::Value(boxed_zero.tag),
                        BlockArg::Value(boxed_zero.payload),
                    ],
                );

                builder.switch_to_block(then_block);
                builder.seal_block(then_block);
                let mut then_val = boxed_int_const(builder, 0);
                for line in &then.lines {
                    then_val = compile_ast(
                        builder,
                        line,
                        vars,
                        func_refs,
                        function_ordinals,
                        function_arities,
                        closure_metadata,
                        capture_slots,
                        env_ptr,
                    );
                }
                builder.ins().jump(
                    merge_block,
                    &[
                        BlockArg::Value(then_val.tag),
                        BlockArg::Value(then_val.payload),
                    ],
                );
            }

            builder.switch_to_block(merge_block);
            builder.seal_block(merge_block);
            let params = builder.block_params(merge_block);
            CompiledValue {
                tag: params[0],
                payload: params[1],
            }
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
fn assert_cranelift_jit_result(src: &str, expected: i64) {
    crate::runtime::reset_runtime_arena();
    let jit = Module::from_source(src).compile_to_jit();
    let ptr = jit
        .get_int_result_fn_ptr("main")
        .expect("cranelift int-result wrapper should exist");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), expected);
}

#[cfg(all(test, feature = "llvm-backend"))]
fn assert_jit_backend_result(src: &str, backend: CodegenBackend, expected: i64) {
    crate::runtime::reset_runtime_arena();
    let jit = Module::from_source(src).compile_to_jit_with_backend(backend);
    let ptr = jit
        .get_int_result_fn_ptr("main")
        .expect("int-result wrapper should exist");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), expected);
}

#[test]
fn jit_python_style_multi_function() {
    let src = "fn double(a):\n    a + a\n\nfn square(a):\n    a * a\n\nfn main():\n    square(25) / double(4)\n";
    assert_cranelift_jit_result(src, 78); // square(25)/double(4) = 625/8 = 78
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
    let ptr = jit
        .get_int_result_fn_ptr("main")
        .expect("cranelift int-result wrapper should exist");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), 8);
}

#[test]
fn text_to_native_execute_with_params() {
    let src = "fn add(x, y) do\n    x + y\nend\nfn main() do\n    add(10, 4)\nend";
    assert_cranelift_jit_result(src, 14);
}

#[test]
fn call_user_defined_function() {
    let src = "fn double(x) do\n    x + x\nend\nfn main() do\n    double(21)\nend";
    assert_cranelift_jit_result(src, 42); // double(21) = 42
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
    assert_cranelift_jit_result(src, 30); // x=10, y=15, 15*2=30
}

#[test]
fn if_without_else() {
    // returns then-value when true, 0 when false
    let src = "fn main() do\n    if 10 > 5 do\n        42\n    end\nend";
    assert_cranelift_jit_result(src, 42);
}

#[test]
fn if_with_else() {
    let src = "fn main() do\n    if 3 > 5 do\n        1\n    else\n        99\n    end\nend";
    assert_cranelift_jit_result(src, 99);
}

#[test]
fn if_python_style() {
    let src = "fn main():\n    x = 10\n    if x > 5:\n        x * 2\n    else:\n        x\n";
    assert_cranelift_jit_result(src, 20);
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
    let src = "fn main() do\n    xs = list_new()\n    list_push(xs, 10)\n    list_push(xs, 32)\n    list_get(xs, 0) + list_get(xs, 1) + list_len(xs)\nend";
    assert_cranelift_jit_result(src, 44);
}

#[test]
fn jit_list_literal_works() {
    let src = "fn main() do\n    xs = [10, 32]\n    list_get(xs, 0) + list_get(xs, 1) + list_len(xs)\nend";
    assert_cranelift_jit_result(src, 44);
}

#[test]
fn jit_list_map_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    ys = list_map(xs, fn item -> item * 2 end)\n    ys[0] + ys[1] + ys[2]\nend";
    assert_cranelift_jit_result(src, 12);
}

#[test]
fn jit_list_map_works_with_multiline_lambda() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    ys = list_map(xs, fn item ->\n        item * 2\n    end)\n    ys[0] + ys[1] + ys[2]\nend";
    assert_cranelift_jit_result(src, 12);
}

#[test]
fn jit_list_map_lambda_allows_local_assignments() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    ys = list_map(xs, fn x ->\n        tmp = [x, x, x]\n        tmp[0] + tmp[1] + tmp[2]\n    end)\n    ys[1]\nend";
    assert_cranelift_jit_result(src, 6);
}

#[test]
fn jit_list_map_accepts_function_values_in_variables() {
    let src = "fn main() do\n    f = fn item -> item * 2 end\n    xs = [1, 2, 3]\n    ys = list_map(xs, f)\n    ys[0] + ys[1] + ys[2]\nend";
    assert_cranelift_jit_result(src, 12);
}

#[test]
fn jit_list_map_accepts_named_functions_as_values() {
    let src = "fn double(item) do\n    item * 2\nend\nfn main() do\n    xs = [1, 2, 3]\n    ys = list_map(xs, double)\n    ys[0] + ys[1] + ys[2]\nend";
    assert_cranelift_jit_result(src, 12);
}

#[test]
fn jit_list_filter_works() {
    let src = "fn main() do\n    xs = [1, 2, 3, 4]\n    ys = list_filter(xs, fn item -> item % 2 end)\n    list_len(ys)\nend";
    assert_cranelift_jit_result(src, 2);
}

#[test]
fn jit_list_range_works() {
    let src = "fn main() do\n    xs = list_range(2, 6)\n    xs[0] + xs[1] + xs[2] + xs[3] + list_len(xs)\nend";
    assert_cranelift_jit_result(src, 18);
}

#[test]
#[should_panic(expected = "list_map callback must take exactly 1 argument")]
fn jit_list_map_rejects_non_unary_callbacks() {
    let src =
        "fn main() do\n    xs = [1, 2, 3]\n    list_map(xs, fn a, b -> a + b end)\n    0\nend";
    let _ = Module::from_source(src).compile_to_jit();
}

#[test]
fn jit_index_syntax_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    xs[1]\nend";
    assert_cranelift_jit_result(src, 2);
}

#[test]
fn jit_list_set_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    list_set(xs, 1, 9)\n    xs[1]\nend";
    assert_cranelift_jit_result(src, 9);
}

#[test]
fn jit_list_insert_works() {
    let src = "fn main() do\n    xs = [1, 3]\n    list_insert(xs, 1, 2)\n    xs[0] + xs[1] + xs[2] + list_len(xs)\nend";
    assert_cranelift_jit_result(src, 9);
}

#[test]
fn jit_index_assignment_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    xs[1] = 9\n    xs[1]\nend";
    assert_cranelift_jit_result(src, 9);
}

#[test]
fn jit_list_swap_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    list_swap(xs, 0, 2)\n    xs[0] + xs[2]\nend";
    assert_cranelift_jit_result(src, 4);
}

#[test]
fn jit_list_pop_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    x = list_pop(xs)\n    x + list_len(xs)\nend";
    assert_cranelift_jit_result(src, 5);
}

#[test]
fn jit_list_copy_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    ys = list_copy(xs)\n    list_pop(xs)\n    list_len(xs) + list_len(ys) + ys[2]\nend";
    assert_cranelift_jit_result(src, 8);
}

#[test]
fn jit_nested_list_works() {
    let src = "fn main() do\n    xs = [[1, 2], [3, 4, 5]]\n    list_len(xs) + list_len(xs[1])\nend";
    assert_cranelift_jit_result(src, 5);
}

#[test]
fn jit_list_print_returns_zero() {
    let src = "fn main() do\n    xs = [4, 5, 6]\n    print(xs)\nend";
    assert_cranelift_jit_result(src, 0);
}

#[test]
fn anonymous_functions_are_lifted_into_hidden_functions() {
    let module =
        Module::from_source("fn main() do\n    list_map(xs, fn item -> item * 2 end)\nend");

    assert_eq!(module.functions.len(), 2);
    assert_eq!(module.functions[0].name, "main");
    assert_eq!(module.functions[1].name, "__lambda_1");
    assert_eq!(module.functions[1].inputs, vec!["item".to_string()]);

    match &module.functions[0].block.lines[0] {
        Ast::Expression(ExpressionAst { function, args }) => {
            assert_eq!(function, "list_map");
            assert!(matches!(args[1], Ast::FunctionRef(_)));
        }
        other => panic!("unexpected lifted main body: {other:?}"),
    }
}

#[test]
fn anonymous_functions_record_captures() {
    let module = Module::from_source(
        "fn main() do\n    a = 2\n    list_filter(xs, fn item -> item == a end)\nend",
    );
    let metadata = module
        .closure_metadata
        .get("__lambda_1")
        .expect("missing lifted lambda metadata");
    assert_eq!(metadata.captures, vec!["a".to_string()]);
}

#[test]
fn jit_list_filter_supports_capturing_closures() {
    let src = "fn main() do\n    a = 2\n    xs = [1, 2, 3, 4]\n    ys = list_filter(xs, fn item -> item > a end)\n    list_len(ys)\nend";
    assert_cranelift_jit_result(src, 2);
}

#[test]
fn jit_function_value_calls_support_closures() {
    let src = "fn main() do\n    a = 7\n    f = fn x -> x + a end\n    f(5)\nend";
    assert_cranelift_jit_result(src, 12);
}

#[test]
fn jit_nested_closures_work() {
    let src = "fn main() do\n    a = 2\n    xs = [1, 2, 3]\n    ys = list_map(xs, fn x ->\n        inner = fn y -> y + a end\n        inner(x)\n    end)\n    ys[2]\nend";
    assert_cranelift_jit_result(src, 5);
}

#[test]
fn jit_self_tail_recursion_works() {
    let src = "fn sum(n, acc) do\n    if n == 0 do\n        acc\n    else\n        sum(n - 1, acc + n)\n    end\nend\n\nfn main() do\n    sum(10000, 0)\nend";
    assert_cranelift_jit_result(src, 50005000);
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
fn llvm_jit_list_map_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    ys = list_map(xs, fn item -> item * 2 end)\n    ys[0] + ys[1] + ys[2]\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 12);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_list_map_accepts_function_values_in_variables() {
    let src = "fn main() do\n    f = fn item -> item * 2 end\n    xs = [1, 2, 3]\n    ys = list_map(xs, f)\n    ys[0] + ys[1] + ys[2]\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 12);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_list_map_accepts_named_functions_as_values() {
    let src = "fn double(item) do\n    item * 2\nend\nfn main() do\n    xs = [1, 2, 3]\n    ys = list_map(xs, double)\n    ys[0] + ys[1] + ys[2]\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 12);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_list_filter_works() {
    let src = "fn main() do\n    xs = [1, 2, 3, 4]\n    ys = list_filter(xs, fn item -> item % 2 end)\n    list_len(ys)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 2);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_list_filter_supports_capturing_closures() {
    let src = "fn main() do\n    a = 2\n    xs = [1, 2, 3, 4]\n    ys = list_filter(xs, fn item -> item > a end)\n    list_len(ys)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 2);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_function_value_calls_support_closures() {
    let src = "fn main() do\n    a = 7\n    f = fn x -> x + a end\n    f(5)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 12);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_nested_closures_work() {
    let src = "fn main() do\n    a = 2\n    xs = [1, 2, 3]\n    ys = list_map(xs, fn x ->\n        inner = fn y -> y + a end\n        inner(x)\n    end)\n    ys[2]\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 5);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_self_tail_recursion_works() {
    let src = "fn sum(n, acc) do\n    if n == 0 do\n        acc\n    else\n        sum(n - 1, acc + n)\n    end\nend\n\nfn main() do\n    sum(10000, 0)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 50005000);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_list_range_works() {
    let src = "fn main() do\n    xs = list_range(2, 6)\n    xs[0] + xs[1] + xs[2] + xs[3] + list_len(xs)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 18);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_nested_list_works() {
    let src = "fn main() do\n    xs = [[1, 2], [3, 4, 5]]\n    list_len(xs) + list_len(xs[1])\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 5);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_list_set_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    xs[1] = 9\n    xs[1]\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 9);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_list_insert_works() {
    let src = "fn main() do\n    xs = [1, 3]\n    list_insert(xs, 1, 2)\n    xs[0] + xs[1] + xs[2] + list_len(xs)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 9);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_list_swap_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    list_swap(xs, 0, 2)\n    xs[0] + xs[2]\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 4);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_list_print_returns_zero() {
    let src = "fn main() do\n    xs = [4, 5, 6]\n    print(xs)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 0);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_int_result_wrapper_works() {
    let src = "fn main() do\n    7 + 5 - 4\nend";
    let jit = Module::from_source(src).compile_to_jit_with_backend(CodegenBackend::Llvm);
    let ptr = jit
        .get_int_result_fn_ptr("main")
        .expect("llvm int-result wrapper should exist");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), 8);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn cranelift_jit_int_result_wrapper_works() {
    let src = "fn main() do\n    7 + 5 - 4\nend";
    let jit = Module::from_source(src).compile_to_jit_with_backend(CodegenBackend::Cranelift);
    let ptr = jit
        .get_int_result_fn_ptr("main")
        .expect("cranelift int-result wrapper should exist");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), 8);
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
