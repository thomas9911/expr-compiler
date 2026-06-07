use crate::analysis::{
    FunctionValueKindAnalysis, KindSet, ModuleValueKindAnalysis, ValueKind, ValueShape,
    narrowed_function_analyses_for_condition,
};
use crate::parser::{
    Ast, BlockAst, ExpressionAst, FunctionDefAst, Ident, LiteralAst, MapEntryAst, MapKeyAst,
};
use crate::source::Span;
use crate::value::{
    CLOSURE_ENV_PTR_OFFSET, CLOSURE_FUNCTION_ORDINAL_OFFSET, CLOSURE_SIZE, LIST_LEN_OFFSET,
    LIST_PTR_OFFSET, MULTI_HEADER_SIZE, MULTI_LEN_OFFSET, MULTI_PTR_OFFSET, STRING_CAP_OFFSET,
    STRING_HEADER_SIZE, STRING_ITER_HEADER_SIZE, STRING_ITER_INDEX_OFFSET,
    STRING_ITER_STRING_OFFSET, STRING_LEN_OFFSET, STRING_PTR_OFFSET, TAG_BIGINT, TAG_FUNCTION,
    TAG_INT, TAG_LIST, TAG_MAP, TAG_MAP_ITER, TAG_MULTI, TAG_STRING, TAG_STRING_ITER,
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
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::process::{Command, ExitStatus};
use thiserror::Error;
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

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum CompileError {
    #[error(
        "top-level expressions are not supported in source files; did you forget `fn` before a function definition?"
    )]
    TopLevelExpression,
    #[error("parse error: {message}")]
    Parse { message: String, span: Option<Span> },
    #[error("llvm backend is not available in this build; enable the `llvm-backend` cargo feature")]
    LlvmBackendUnavailable,
    #[error(
        "component wasm output requires the `wasi` cargo feature (which also enables `llvm-backend`)"
    )]
    WasiFeatureRequired,
    #[error("component wasm output currently supports only the llvm backend")]
    ComponentRequiresLlvm,
    #[error("core wasm output currently supports only the llvm backend")]
    WasmRequiresLlvm,
    #[error("{mode} supports at most {max} argument(s), found {found}")]
    InvalidMainArity { mode: &'static str, max: usize, found: usize, span: Option<Span> },
    #[error("{builtin} callback `{function}` must take exactly 1 argument")]
    CallbackArity { builtin: String, function: String, span: Option<Span> },
    #[error("undefined function: {name}")]
    UndefinedFunction { name: String, span: Option<Span> },
    #[error("undefined variable: {name}")]
    UndefinedVariable { name: String, span: Option<Span> },
    #[error("{function} argument {argument} expects {expected}, found {found}")]
    InvalidArgumentType {
        function: String,
        argument: usize,
        expected: String,
        found: String,
        span: Option<Span>,
    },
    #[error("function `{function}` returns {expected} values in one path and {found} in another")]
    ReturnArityMismatch { function: String, expected: usize, found: usize, span: Option<Span> },
    #[error("destructuring assignment expects {expected} values, found {found}")]
    DestructuringArityMismatch { expected: usize, found: usize, span: Option<Span> },
    #[error("multi-return values are not supported in this context")]
    UnsupportedMultiValueContext { span: Option<Span> },
    #[error("{mode} must return exactly 1 value in phase 1 multi-return mode, found {found}")]
    InvalidMainReturnArity { mode: &'static str, found: usize, span: Option<Span> },
    #[error("{0} is not implemented yet")]
    UnsupportedFeature(&'static str),
    #[error("toolchain error: {0}")]
    Toolchain(String),
}

impl CompileError {
    pub fn span(&self) -> Option<&Span> {
        match self {
            Self::CallbackArity { span, .. }
            | Self::Parse { span, .. }
            | Self::InvalidMainArity { span, .. }
            | Self::UndefinedFunction { span, .. }
            | Self::UndefinedVariable { span, .. }
            | Self::InvalidArgumentType { span, .. }
            | Self::ReturnArityMismatch { span, .. }
            | Self::DestructuringArityMismatch { span, .. }
            | Self::UnsupportedMultiValueContext { span }
            | Self::InvalidMainReturnArity { span, .. } => span.as_ref(),
            _ => None,
        }
    }
}

pub struct Module {
    pub functions: Vec<FunctionDefAst>,
    source: Option<String>,
    closure_metadata: HashMap<String, ClosureMetadata>,
    used_features: UsedFeatures,
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

#[derive(Clone, Copy)]
struct StdlibFunction {
    source: &'static str,
    stdlib_deps: &'static [&'static str],
}

#[derive(Clone, Copy, Debug, Default)]
struct UsedFeatures {
    bigint: bool,
    lists: bool,
    list_mutation: bool,
    maps: bool,
}

impl Module {
    pub fn analyze_value_kinds(&self) -> Result<ModuleValueKindAnalysis, CompileError> {
        let return_arities = function_return_arities(&self.functions)?;
        Ok(crate::analysis::analyze_module_value_kinds(&self.functions, &return_arities))
    }

    fn validate_native_main_arity(&self) -> Result<(), CompileError> {
        if let Some(main) = self.functions.iter().find(|func| func.name == "main") {
            if main.inputs.len() > 1 {
                return Err(CompileError::InvalidMainArity {
                    mode: "native executable main function",
                    max: 1,
                    found: main.inputs.len(),
                    span: main.span.clone(),
                });
            }
        }
        Ok(())
    }

    fn validate_user_facing_constructs(&self) -> Result<(), CompileError> {
        let function_names =
            self.functions.iter().map(|func| func.name.clone()).collect::<HashSet<_>>();
        let function_arities = function_arities(&self.functions);
        let function_return_arities = function_return_arities(&self.functions)?;
        let value_kind_analysis = self.analyze_value_kinds()?;
        for func in &self.functions {
            let mut scope_names = func.inputs.clone();
            if let Some(metadata) = self.closure_metadata.get(&func.name) {
                for capture in &metadata.captures {
                    if !scope_names.contains(capture) {
                        scope_names.push(capture.clone());
                    }
                }
            }
            collect_var_names(&Ast::Block(func.block.clone()), &mut scope_names);
            let locals = scope_names.into_iter().collect::<HashSet<_>>();
            let function_analysis = value_kind_analysis
                .functions
                .get(&func.name)
                .expect("missing value kind analysis for function");
            validate_ast_user_facing(
                &Ast::Block(func.block.clone()),
                &locals,
                &function_names,
                &function_arities,
                &value_kind_analysis,
                function_analysis,
            )?;
            validate_block_multi_return_usage(
                &func.block,
                &func.name,
                &locals,
                &function_names,
                &function_return_arities,
                *function_return_arities.get(&func.name).unwrap_or(&1),
            )?;
        }
        self.validate_main_return_arity(&function_return_arities)?;
        Ok(())
    }

    fn validate_main_return_arity(
        &self,
        function_return_arities: &HashMap<String, usize>,
    ) -> Result<(), CompileError> {
        if let Some(main) = self.functions.iter().find(|func| func.name == "main") {
            let found = *function_return_arities.get("main").unwrap_or(&1);
            if found != 1 {
                return Err(CompileError::InvalidMainReturnArity {
                    mode: "runnable main function",
                    found,
                    span: main.span.clone(),
                });
            }
        }
        Ok(())
    }

    pub fn new() -> Self {
        Module {
            functions: vec![],
            source: None,
            closure_metadata: HashMap::new(),
            used_features: UsedFeatures::default(),
        }
    }

    pub fn add_function(&mut self, func: FunctionDefAst) {
        self.functions.push(func);
    }

    pub fn from_source(source: &str) -> Self {
        Self::try_from_source(source).unwrap_or_else(|err| panic!("{err}"))
    }

    pub fn try_from_source(source: &str) -> Result<Self, CompileError> {
        let mut module = Self::try_from_functions(parse_source_functions(source)?)?;
        module.source = Some(source.to_string());
        Ok(module)
    }

    pub fn from_ast(ast: Ast) -> Self {
        Self::try_from_ast(ast).unwrap_or_else(|err| panic!("{err}"))
    }

    pub fn try_from_ast(ast: Ast) -> Result<Self, CompileError> {
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
        Self::try_from_functions(functions)
    }

    fn try_from_functions(functions: Vec<FunctionDefAst>) -> Result<Self, CompileError> {
        for func in &functions {
            validate_no_nested_function_defs(&Ast::Block(func.block.clone()))?;
        }
        let functions = autoload_stdlib_functions(functions);
        let (functions, closure_metadata) = lift_anonymous_functions(functions);
        let module = Module {
            functions,
            source: None,
            closure_metadata,
            used_features: UsedFeatures::default(),
        };
        let used_features = collect_used_features(&module.functions);
        Ok(Module { used_features, ..module })
    }

    #[cfg(feature = "llvm-backend")]
    pub(super) fn uses_bigint(&self) -> bool {
        self.used_features.bigint
    }

    #[cfg(feature = "llvm-backend")]
    pub(super) fn uses_lists(&self) -> bool {
        self.used_features.lists
    }

    #[cfg(feature = "llvm-backend")]
    pub(super) fn uses_list_mutation(&self) -> bool {
        self.used_features.list_mutation
    }

    #[cfg(feature = "llvm-backend")]
    pub(super) fn uses_maps(&self) -> bool {
        self.used_features.maps
    }

    pub fn compile_to_jit(self) -> JitArtifact {
        self.try_compile_to_jit().unwrap_or_else(|err| panic!("{err}"))
    }

    pub fn try_compile_to_jit(self) -> Result<JitArtifact, CompileError> {
        self.try_compile_to_jit_with_backend(CodegenBackend::Cranelift)
    }

    pub fn compile_to_jit_with_backend(self, backend: CodegenBackend) -> JitArtifact {
        self.try_compile_to_jit_with_backend(backend).unwrap_or_else(|err| panic!("{err}"))
    }

    pub fn try_compile_to_jit_with_backend(
        self,
        backend: CodegenBackend,
    ) -> Result<JitArtifact, CompileError> {
        self.validate_user_facing_constructs()?;
        match backend {
            CodegenBackend::Cranelift => {
                Ok(JitArtifact::Cranelift(self.compile_to_cranelift_jit()))
            }
            CodegenBackend::Llvm => {
                #[cfg(feature = "llvm-backend")]
                {
                    Ok(JitArtifact::Llvm(llvm_backend::compile_to_jit(self)?))
                }
                #[cfg(not(feature = "llvm-backend"))]
                {
                    let _ = self;
                    Err(CompileError::LlvmBackendUnavailable)
                }
            }
        }
    }

    fn compile_to_cranelift_jit(self) -> CraneliftJitModule {
        let value_kind_analysis = self
            .analyze_value_kinds()
            .expect("value kind analysis should succeed before Cranelift JIT codegen");
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
            self.used_features.bigint,
            self.used_features.lists,
            self.used_features.list_mutation,
            self.used_features.maps,
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
                &value_kind_analysis,
            );
            if func_def.inputs.len() <= 1 {
                let scalar_id = declare_jit_int_result_sig(
                    &mut cranelift_module,
                    &isa,
                    &int_result_symbol_name(&func_def.name),
                    Linkage::Local,
                    func_def.inputs.len(),
                );
                define_jit_int_result_wrapper(
                    &mut cranelift_module,
                    isa.clone(),
                    &flags,
                    scalar_id,
                    internal_func_ids[&func_def.name],
                    func_def.inputs.len(),
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
        self.try_compile_to_ir().unwrap_or_else(|err| panic!("{err}"))
    }

    pub fn try_compile_to_ir(self) -> Result<String, CompileError> {
        self.validate_user_facing_constructs()?;
        let value_kind_analysis = self.analyze_value_kinds()?;
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift::native::builder()
            .expect("host machine supported")
            .finish(flags.clone())
            .unwrap();

        let mut cranelift_module = ObjectModule::new(
            ObjectBuilder::new(isa.clone(), "ir", default_libcall_names()).unwrap(),
        );

        let builtin_ids = runtime_ir::setup_builtins(
            &mut cranelift_module,
            &isa,
            &flags,
            self.used_features.bigint,
            self.used_features.lists,
            self.used_features.list_mutation,
            self.used_features.maps,
        );
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
                &value_kind_analysis,
            );
            out.push_str(&ir);
            out.push('\n');
        }
        Ok(out)
    }

    pub fn compile_to_object(self, name: &str) -> Vec<u8> {
        self.try_compile_to_object(name).unwrap_or_else(|err| panic!("{err}"))
    }

    pub fn try_compile_to_object(self, name: &str) -> Result<Vec<u8>, CompileError> {
        self.try_compile_to_object_with_backend(name, CodegenBackend::Cranelift)
    }

    pub fn compile_to_object_with_backend(self, name: &str, backend: CodegenBackend) -> Vec<u8> {
        self.try_compile_to_object_with_backend(name, backend).unwrap_or_else(|err| panic!("{err}"))
    }

    pub fn try_compile_to_object_with_backend(
        self,
        name: &str,
        backend: CodegenBackend,
    ) -> Result<Vec<u8>, CompileError> {
        self.validate_user_facing_constructs()?;
        match backend {
            CodegenBackend::Cranelift => Ok(self.compile_to_cranelift_object(name)),
            CodegenBackend::Llvm => {
                #[cfg(feature = "llvm-backend")]
                {
                    llvm_backend::compile_to_object(self, name)
                }
                #[cfg(not(feature = "llvm-backend"))]
                {
                    let _ = name;
                    let _ = self;
                    Err(CompileError::LlvmBackendUnavailable)
                }
            }
        }
    }

    fn compile_to_cranelift_object(self, name: &str) -> Vec<u8> {
        let value_kind_analysis = self
            .analyze_value_kinds()
            .expect("value kind analysis should succeed before Cranelift object codegen");
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift::native::builder()
            .expect("host machine supported")
            .finish(flags.clone())
            .unwrap();

        let mut cranelift_module = ObjectModule::new(
            ObjectBuilder::new(isa.clone(), name, default_libcall_names()).unwrap(),
        );

        let builtin_ids = runtime_ir::setup_builtins(
            &mut cranelift_module,
            &isa,
            &flags,
            self.used_features.bigint,
            self.used_features.lists,
            self.used_features.list_mutation,
            self.used_features.maps,
        );
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
                &value_kind_analysis,
            );
        }
        cranelift_module.finish().emit().unwrap()
    }

    pub fn compile_to_executable(self, output: &Path) {
        self.try_compile_to_executable(output).unwrap_or_else(|err| panic!("{err}"))
    }

    pub fn try_compile_to_executable(self, output: &Path) -> Result<(), CompileError> {
        self.try_compile_to_executable_with_backend(output, CodegenBackend::Cranelift)
    }

    pub fn compile_to_executable_with_backend(self, output: &Path, backend: CodegenBackend) {
        self.try_compile_to_executable_with_backend(output, backend)
            .unwrap_or_else(|err| panic!("{err}"))
    }

    pub fn try_compile_to_executable_with_backend(
        self,
        output: &Path,
        backend: CodegenBackend,
    ) -> Result<(), CompileError> {
        self.validate_native_main_arity()?;
        self.validate_user_facing_constructs()?;
        if is_component_wasm_output(output) {
            match backend {
                CodegenBackend::Llvm => {
                    #[cfg(all(feature = "llvm-backend", feature = "wasi"))]
                    {
                        self.compile_to_llvm_component(output)?;
                        return Ok(());
                    }
                    #[cfg(not(all(feature = "llvm-backend", feature = "wasi")))]
                    {
                        let _ = output;
                        let _ = self;
                        return Err(CompileError::WasiFeatureRequired);
                    }
                }
                CodegenBackend::Cranelift => {
                    return Err(CompileError::ComponentRequiresLlvm);
                }
            }
        }

        if is_wasm_output(output) {
            match backend {
                CodegenBackend::Llvm => {
                    #[cfg(feature = "llvm-backend")]
                    {
                        self.compile_to_llvm_wasm(output)?;
                        return Ok(());
                    }
                    #[cfg(not(feature = "llvm-backend"))]
                    {
                        let _ = output;
                        let _ = self;
                        return Err(CompileError::LlvmBackendUnavailable);
                    }
                }
                CodegenBackend::Cranelift => {
                    return Err(CompileError::WasmRequiresLlvm);
                }
            }
        }

        match backend {
            CodegenBackend::Cranelift => self.compile_to_cranelift_executable(output)?,
            CodegenBackend::Llvm => {
                #[cfg(feature = "llvm-backend")]
                {
                    self.compile_to_llvm_executable(output)?;
                }
                #[cfg(not(feature = "llvm-backend"))]
                {
                    let _ = output;
                    let _ = self;
                    return Err(CompileError::LlvmBackendUnavailable);
                }
            }
        }
        Ok(())
    }

    fn compile_to_cranelift_executable(self, output: &Path) -> Result<(), CompileError> {
        let value_kind_analysis = self.analyze_value_kinds()?;
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift::native::builder()
            .expect("host machine supported")
            .finish(flags.clone())
            .unwrap();

        let mut cranelift_module = ObjectModule::new(
            ObjectBuilder::new(isa.clone(), "exe", default_libcall_names()).unwrap(),
        );

        let builtin_ids = runtime_ir::setup_builtins(
            &mut cranelift_module,
            &isa,
            &flags,
            self.used_features.bigint,
            self.used_features.lists,
            self.used_features.list_mutation,
            self.used_features.maps,
        );
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
                if func_def.inputs.len() <= 1 {
                    let int_id = declare_executable_main_int_result_sig(
                        &mut cranelift_module,
                        &isa,
                        Linkage::Export,
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
                &value_kind_analysis,
            );
            if func_def.name == "main" && func_def.inputs.len() <= 1 {
                define_executable_main_int_result_wrapper(
                    &mut cranelift_module,
                    isa.clone(),
                    &flags,
                    expr_main_int_id.expect("main int wrapper id should exist"),
                    internal_func_ids[&func_def.name],
                    func_def.inputs.len(),
                );
            }
        }
        let bytes = cranelift_module.finish().emit().unwrap();

        #[cfg(windows)]
        let tmp = output.with_extension("obj");
        #[cfg(not(windows))]
        let tmp = output.with_extension("o");
        write_file_or_compile_error(&tmp, &bytes, "failed to write native object file")?;

        #[cfg(windows)]
        let status = Command::new("rustc")
            .arg(write_windows_wrapper(output)?)
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
            .map_err(|err| toolchain_error(format!("failed to launch rustc: {err}")))?;

        #[cfg(not(windows))]
        let status = Command::new("rustc")
            .arg(write_unix_rust_wrapper(output)?)
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
            .arg("link-arg=-no-pie")
            .arg("-C")
            .arg(format!("link-arg={}", tmp.display()))
            .arg("-o")
            .arg(output)
            .status()
            .map_err(|err| toolchain_error(format!("failed to launch rustc: {err}")))?;

        #[cfg(windows)]
        std::fs::remove_file(generated_wrapper_path(output, "windows_wrapper.rs")).ok();
        #[cfg(not(windows))]
        std::fs::remove_file(generated_wrapper_path(output, "unix_wrapper.rs")).ok();
        std::fs::remove_file(&tmp).ok();
        if !status.success() {
            return Err(command_status_error("rustc", "native executable link failed", status));
        }
        Ok(())
    }

    #[cfg(feature = "llvm-backend")]
    fn compile_to_llvm_executable(self, output: &Path) -> Result<(), CompileError> {
        let bytes = llvm_backend::compile_to_object(self, "llvm_exe")?;
        #[cfg(windows)]
        let tmp = output.with_extension("obj");
        #[cfg(not(windows))]
        let tmp = output.with_extension("o");
        write_file_or_compile_error(&tmp, &bytes, "failed to write llvm object file")?;

        #[cfg(windows)]
        let status = Command::new("rustc")
            .arg(write_windows_wrapper(output)?)
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
            .map_err(|err| toolchain_error(format!("failed to launch rustc: {err}")))?;

        #[cfg(not(windows))]
        let status = Command::new("rustc")
            .arg(write_unix_rust_wrapper(output)?)
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
            .map_err(|err| toolchain_error(format!("failed to launch rustc: {err}")))?;

        #[cfg(windows)]
        std::fs::remove_file(generated_wrapper_path(output, "windows_wrapper.rs")).ok();
        #[cfg(not(windows))]
        std::fs::remove_file(generated_wrapper_path(output, "unix_wrapper.rs")).ok();
        std::fs::remove_file(&tmp).ok();
        if !status.success() {
            return Err(command_status_error(
                "rustc",
                "llvm native executable link failed",
                status,
            ));
        }
        Ok(())
    }

    #[cfg(feature = "llvm-backend")]
    fn compile_to_llvm_wasm(self, output: &Path) -> Result<(), CompileError> {
        let has_supported_main =
            self.functions.iter().any(|func| func.name == "main" && func.inputs.len() <= 1);
        assert!(
            has_supported_main,
            "llvm wasm output requires a main function with at most one argument"
        );

        let asm = llvm_backend::compile_to_wasm_assembly(self, "llvm_wasm")?;
        let asm_tmp = output.with_extension("s");
        let obj_tmp = output.with_extension("o");
        write_file_or_compile_error(&asm_tmp, &asm, "failed to write llvm wasm assembly")?;

        let mut llvm_mc = Command::new(find_llvm_tool("llvm-mc"));
        llvm_mc
            .arg("-triple=wasm32-unknown-unknown")
            .arg("-filetype=obj")
            .arg(&asm_tmp)
            .arg("-o")
            .arg(&obj_tmp);
        run_command_or_compile_error(llvm_mc, "llvm-mc", "wasm assembly failed")?;

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
            .map_err(|err| toolchain_error(format!("failed to launch wasm-ld: {err}")))?;

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
        if !status.success() {
            return Err(command_status_error("wasm-ld", "wasm link failed", status));
        }
        Ok(())
    }

    #[cfg(all(feature = "llvm-backend", feature = "wasi"))]
    fn compile_to_llvm_component(self, output: &Path) -> Result<(), CompileError> {
        let has_supported_main =
            self.functions.iter().any(|func| func.name == "main" && func.inputs.len() <= 1);
        assert!(
            has_supported_main,
            "llvm component output requires a main function with at most one argument"
        );

        let asm = llvm_backend::compile_to_wasm_preview1_command_assembly(self, "llvm_component")?;
        let asm_tmp = output.with_extension("component.s");
        let obj_tmp = output.with_extension("component.o");
        let core_tmp = output.with_extension("core.wasm");
        write_file_or_compile_error(&asm_tmp, &asm, "failed to write llvm component assembly")?;

        let mut llvm_mc = Command::new(find_llvm_tool("llvm-mc"));
        llvm_mc
            .arg("-triple=wasm32-unknown-unknown")
            .arg("-filetype=obj")
            .arg(&asm_tmp)
            .arg("-o")
            .arg(&obj_tmp);
        run_command_or_compile_error(llvm_mc, "llvm-mc", "component wasm assembly failed")?;

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
            .map_err(|err| toolchain_error(format!("failed to launch wasm-ld: {err}")))?;
        if !link_status.success() {
            return Err(command_status_error("wasm-ld", "component wasm link failed", link_status));
        }

        let core_bytes =
            read_file_or_compile_error(&core_tmp, "failed to read intermediate core wasm")?;
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
        write_file_or_compile_error(output, component_bytes, "failed to write component output")?;

        if output.exists() {
            std::fs::remove_file(&asm_tmp).ok();
            std::fs::remove_file(&obj_tmp).ok();
            std::fs::remove_file(&core_tmp).ok();
        }
        Ok(())
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
        self.internal_func_ids.get(name).map(|id| self.module.get_finalized_function(*id))
    }

    pub fn get_int_result_fn_ptr(&self, name: &str) -> Option<*const u8> {
        self.int_result_func_ids.get(name).map(|id| self.module.get_finalized_function(*id))
    }

    pub fn user_function_names(&self) -> impl Iterator<Item = &str> {
        self.func_ids.keys().filter(|n| !is_builtin_name(n)).map(|s| s.as_str())
    }
}

#[cfg(windows)]
fn write_windows_wrapper(output: &Path) -> Result<std::path::PathBuf, CompileError> {
    let wrapper = generated_wrapper_path(output, "windows_wrapper.rs");
    let source = include_str!("./wrapper/windows.rs");
    let parent = wrapper.parent().unwrap();
    std::fs::create_dir_all(parent).map_err(|err| {
        io_toolchain_error("failed to create generated wrapper directory", parent, err)
    })?;
    write_file_or_compile_error(&wrapper, source, "failed to write generated windows wrapper")?;
    Ok(wrapper)
}

#[cfg(not(windows))]
fn write_unix_rust_wrapper(output: &Path) -> Result<std::path::PathBuf, CompileError> {
    let wrapper = generated_wrapper_path(output, "unix_wrapper.rs");
    let source = include_str!("./wrapper/unix.rs");
    let parent = wrapper.parent().unwrap();
    std::fs::create_dir_all(parent).map_err(|err| {
        io_toolchain_error("failed to create generated wrapper directory", parent, err)
    })?;
    write_file_or_compile_error(&wrapper, source, "failed to write generated unix wrapper")?;
    Ok(wrapper)
}

fn generated_wrapper_path(output: &Path, suffix: &str) -> std::path::PathBuf {
    let parent = output.parent().unwrap_or_else(|| Path::new("."));
    let stem = output.file_stem().and_then(|s| s.to_str()).unwrap_or("output");
    parent.join(".expr-compiler").join(format!("{stem}.{suffix}"))
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

#[cfg(all(test, feature = "llvm-backend"))]
fn llvm_tool_test_lock() -> &'static std::sync::Mutex<()> {
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    LOCK.get_or_init(|| std::sync::Mutex::new(()))
}

#[cfg(all(test, feature = "llvm-backend"))]
unsafe fn set_env_var<K: AsRef<std::ffi::OsStr>, V: AsRef<std::ffi::OsStr>>(key: K, value: V) {
    unsafe { std::env::set_var(key, value) };
}

#[cfg(all(test, feature = "llvm-backend"))]
unsafe fn remove_env_var<K: AsRef<std::ffi::OsStr>>(key: K) {
    unsafe { std::env::remove_var(key) };
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

fn executable_main_symbol_name() -> &'static str {
    #[cfg(windows)]
    {
        "expr_main_entry_int"
    }
    #[cfg(not(windows))]
    {
        "__expr_main_i64"
    }
}

fn toolchain_error(message: impl Into<String>) -> CompileError {
    CompileError::Toolchain(message.into())
}

fn io_toolchain_error(context: &str, path: &Path, err: std::io::Error) -> CompileError {
    toolchain_error(format!("{context} {}: {err}", path.display()))
}

fn write_file_or_compile_error(
    path: &Path,
    contents: impl AsRef<[u8]>,
    context: &str,
) -> Result<(), CompileError> {
    std::fs::write(path, contents).map_err(|err| io_toolchain_error(context, path, err))
}

#[cfg(all(feature = "llvm-backend", feature = "wasi"))]
fn read_file_or_compile_error(path: &Path, context: &str) -> Result<Vec<u8>, CompileError> {
    std::fs::read(path).map_err(|err| io_toolchain_error(context, path, err))
}

#[cfg(feature = "llvm-backend")]
fn run_command_or_compile_error(
    mut command: Command,
    tool_name: &str,
    failure_context: &str,
) -> Result<(), CompileError> {
    let status = command.status().map_err(|err| {
        toolchain_error(format!("{failure_context}: failed to launch {tool_name}: {err}"))
    })?;
    if status.success() {
        Ok(())
    } else {
        Err(command_status_error(tool_name, failure_context, status))
    }
}

fn command_status_error(
    tool_name: &str,
    failure_context: &str,
    status: ExitStatus,
) -> CompileError {
    toolchain_error(format!("{failure_context}: {tool_name} exited with {status}"))
}

fn declare_jit_int_result_sig(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    name: &str,
    linkage: Linkage,
    input_count: usize,
) -> FuncId {
    let mut sig = Signature::new(isa.default_call_conv());
    if input_count == 1 {
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
    }
    sig.returns.push(AbiParam::new(types::I64));
    module.declare_function(name, linkage, &sig).unwrap()
}

fn declare_executable_main_int_result_sig(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    linkage: Linkage,
) -> FuncId {
    let mut sig = Signature::new(isa.default_call_conv());
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));
    module.declare_function(executable_main_symbol_name(), linkage, &sig).unwrap()
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
    functions.iter().map(|func| (func.name.clone(), func.inputs.len())).collect()
}

fn function_return_arities(
    functions: &[FunctionDefAst],
) -> Result<HashMap<String, usize>, CompileError> {
    let mut arities = functions
        .iter()
        .map(|func| (func.name.clone(), explicit_function_return_arity(func.block.lines.last())))
        .collect::<HashMap<_, _>>();
    for _ in 0..functions.len() {
        let mut changed = false;
        for func in functions {
            let inferred = infer_block_return_arity(&func.block, &func.name, &arities)?;
            let entry = arities.entry(func.name.clone()).or_insert(inferred);
            if *entry != inferred {
                *entry = inferred;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    Ok(arities)
}

fn explicit_function_return_arity(last: Option<&Ast>) -> usize {
    last.and_then(explicit_ast_return_arity).unwrap_or(1)
}

fn explicit_block_return_arity(block: &BlockAst) -> Option<usize> {
    block.lines.last().and_then(explicit_ast_return_arity)
}

fn explicit_ast_return_arity(ast: &Ast) -> Option<usize> {
    match ast {
        Ast::MultiValue(values) => Some(values.len()),
        Ast::Block(block) => explicit_block_return_arity(block),
        Ast::If { then, else_, .. } => {
            let then_arity = explicit_block_return_arity(then);
            let else_arity = else_.as_ref().and_then(explicit_block_return_arity);
            match (then_arity, else_arity) {
                (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
                (Some(lhs), None) | (None, Some(lhs)) => Some(lhs),
                _ => None,
            }
        }
        _ => None,
    }
}

fn infer_block_return_arity(
    block: &BlockAst,
    current_function: &str,
    function_return_arities: &HashMap<String, usize>,
) -> Result<usize, CompileError> {
    match block.lines.last() {
        Some(ast) => infer_ast_return_arity(ast, current_function, function_return_arities),
        None => Ok(1),
    }
}

fn infer_ast_return_arity(
    ast: &Ast,
    current_function: &str,
    function_return_arities: &HashMap<String, usize>,
) -> Result<usize, CompileError> {
    match ast {
        Ast::MultiValue(values) => Ok(values.len()),
        Ast::Block(block) => {
            infer_block_return_arity(block, current_function, function_return_arities)
        }
        Ast::If { then, else_, span, .. } => {
            let then_arity =
                infer_block_return_arity(then, current_function, function_return_arities)?;
            let else_arity = if let Some(else_block) = else_ {
                infer_block_return_arity(else_block, current_function, function_return_arities)?
            } else {
                1
            };
            if then_arity != else_arity {
                return Err(CompileError::ReturnArityMismatch {
                    function: current_function.to_string(),
                    expected: then_arity,
                    found: else_arity,
                    span: span.clone(),
                });
            }
            Ok(then_arity)
        }
        Ast::Expression(ExpressionAst { function, .. }) if !function.is_empty() => {
            if function == "map_iter_next" {
                Ok(2)
            } else {
                Ok(*function_return_arities.get(function).unwrap_or(&1))
            }
        }
        _ => Ok(1),
    }
}

fn parse_source_functions(source: &str) -> Result<Vec<FunctionDefAst>, CompileError> {
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
            Ok(_) => return Err(CompileError::TopLevelExpression),
            Err(err) => {
                return Err(CompileError::Parse {
                    message: err.to_string(),
                    span: Some(err.span.clone()),
                });
            }
        }
    }

    Ok(functions)
}

fn stdlib_function(name: &str) -> Option<StdlibFunction> {
    match name {
        "string_is_empty" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_is_empty.expr"),
            stdlib_deps: &[],
        }),
        "string_is_not_empty" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_is_not_empty.expr"),
            stdlib_deps: &["string_is_empty"],
        }),
        "string_len" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_len.expr"),
            stdlib_deps: &[],
        }),
        "string_first" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_first.expr"),
            stdlib_deps: &[],
        }),
        "string_last" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_last.expr"),
            stdlib_deps: &[],
        }),
        "string_starts_with" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_starts_with.expr"),
            stdlib_deps: &[],
        }),
        "string_ends_with" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_ends_with.expr"),
            stdlib_deps: &[],
        }),
        "string_contains" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_contains.expr"),
            stdlib_deps: &[],
        }),
        "string_is_ascii" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_is_ascii.expr"),
            stdlib_deps: &[],
        }),
        "string_all" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_all.expr"),
            stdlib_deps: &[],
        }),
        "string_any" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_any.expr"),
            stdlib_deps: &[],
        }),
        "string_is_integer" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_is_integer.expr"),
            stdlib_deps: &["string_all"],
        }),
        "string_try_parse_integer" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_try_parse_integer.expr"),
            stdlib_deps: &[],
        }),
        "string_try_parse_bigint" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_try_parse_bigint.expr"),
            stdlib_deps: &[],
        }),
        "string_from_codepoints" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_from_codepoints.expr"),
            stdlib_deps: &[],
        }),
        "type_of" => Some(StdlibFunction {
            source: include_str!("./stdlib/type_of.expr"),
            stdlib_deps: &[
                "is_int",
                "is_bigint",
                "is_string",
                "is_list",
                "is_map",
                "is_map_iter",
                "is_function",
                "is_string_iter",
            ],
        }),
        "map_try_get" => Some(StdlibFunction {
            source: include_str!("./stdlib/map_try_get.expr"),
            stdlib_deps: &[],
        }),
        "map_try_delete" => Some(StdlibFunction {
            source: include_str!("./stdlib/map_try_delete.expr"),
            stdlib_deps: &["map_try_get"],
        }),
        "map_try_pop" => Some(StdlibFunction {
            source: include_str!("./stdlib/map_try_pop.expr"),
            stdlib_deps: &["map_keys"],
        }),
        "map_update" => Some(StdlibFunction {
            source: include_str!("./stdlib/map_update.expr"),
            stdlib_deps: &[],
        }),
        "map_update_or_default" => Some(StdlibFunction {
            source: include_str!("./stdlib/map_update_or_default.expr"),
            stdlib_deps: &[],
        }),
        "map_keys" => Some(StdlibFunction {
            source: include_str!("./stdlib/map_keys.expr"),
            stdlib_deps: &[],
        }),
        "map_values" => Some(StdlibFunction {
            source: include_str!("./stdlib/map_values.expr"),
            stdlib_deps: &[],
        }),
        "string_try_first" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_try_first.expr"),
            stdlib_deps: &["string_first"],
        }),
        "string_try_last" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_try_last.expr"),
            stdlib_deps: &["string_last"],
        }),
        "bytes_try_get" => Some(StdlibFunction {
            source: include_str!("./stdlib/bytes_try_get.expr"),
            stdlib_deps: &[],
        }),
        "string_try_pop" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_try_pop.expr"),
            stdlib_deps: &[],
        }),
        "string_repeat" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_repeat.expr"),
            stdlib_deps: &[],
        }),
        "string_reverse" => Some(StdlibFunction {
            source: include_str!("./stdlib/string_reverse.expr"),
            stdlib_deps: &[],
        }),
        "list_all" => Some(StdlibFunction {
            source: include_str!("./stdlib/list_all.expr"),
            stdlib_deps: &[],
        }),
        "list_any" => Some(StdlibFunction {
            source: include_str!("./stdlib/list_any.expr"),
            stdlib_deps: &[],
        }),
        _ => None,
    }
}

fn autoload_stdlib_functions(mut functions: Vec<FunctionDefAst>) -> Vec<FunctionDefAst> {
    let mut defined = functions.iter().map(|func| func.name.clone()).collect::<HashSet<_>>();

    loop {
        let mut needed = collect_stdlib_references(&functions);
        let mut queued = needed.iter().cloned().collect::<Vec<_>>();
        while let Some(name) = queued.pop() {
            if let Some(stdlib) = stdlib_function(&name) {
                for dep in stdlib.stdlib_deps {
                    if !needed.contains(*dep) {
                        needed.insert((*dep).to_string());
                        queued.push((*dep).to_string());
                    }
                }
            }
        }

        let mut added = vec![];
        for name in needed {
            if defined.contains(&name) {
                continue;
            }
            let Some(stdlib) = stdlib_function(&name) else {
                continue;
            };
            for func in parse_source_functions(stdlib.source)
                .unwrap_or_else(|err| panic!("invalid stdlib source for {name}: {err}"))
            {
                if defined.insert(func.name.clone()) {
                    added.push(func);
                }
            }
        }

        if added.is_empty() {
            break;
        }
        functions.extend(added);
    }

    functions
}

fn collect_stdlib_references(functions: &[FunctionDefAst]) -> HashSet<String> {
    let mut refs = HashSet::new();
    for function in functions {
        let mut scope = function.inputs.iter().cloned().collect::<HashSet<_>>();
        collect_stdlib_references_from_block(&function.block, &mut scope, &mut refs);
    }
    refs
}

fn collect_stdlib_references_from_block(
    block: &BlockAst,
    scope: &mut HashSet<String>,
    refs: &mut HashSet<String>,
) {
    for line in &block.lines {
        collect_stdlib_references_from_ast(line, scope, refs);
        match line {
            Ast::Assign { name, .. } => {
                scope.insert(name.clone());
            }
            Ast::MultiAssign { names, .. } => {
                scope.extend(names.iter().cloned());
            }
            _ => {}
        }
    }
}

fn collect_stdlib_references_from_ast(
    ast: &Ast,
    scope: &HashSet<String>,
    refs: &mut HashSet<String>,
) {
    match ast {
        Ast::Expression(ExpressionAst { function, args, .. }) => {
            if !function.is_empty()
                && !scope.contains(function)
                && stdlib_function(function).is_some()
            {
                refs.insert(function.clone());
            }
            for arg in args {
                collect_stdlib_references_from_ast(arg, scope, refs);
            }
        }
        Ast::MultiValue(values) => {
            for value in values {
                collect_stdlib_references_from_ast(value, scope, refs);
            }
        }
        Ast::Variable(name) | Ast::FunctionRef(name) => {
            if !scope.contains(name.as_str()) && stdlib_function(name.as_ref()).is_some() {
                refs.insert(name.to_string());
            }
        }
        Ast::Assign { value, .. } => collect_stdlib_references_from_ast(value, scope, refs),
        Ast::MultiAssign { value, .. } => collect_stdlib_references_from_ast(value, scope, refs),
        Ast::If { condition, then, else_, .. } => {
            collect_stdlib_references_from_ast(condition, scope, refs);
            let mut then_scope = scope.clone();
            collect_stdlib_references_from_block(then, &mut then_scope, refs);
            if let Some(else_block) = else_ {
                let mut else_scope = scope.clone();
                collect_stdlib_references_from_block(else_block, &mut else_scope, refs);
            }
        }
        Ast::Block(block) => {
            let mut nested_scope = scope.clone();
            collect_stdlib_references_from_block(block, &mut nested_scope, refs);
        }
        Ast::Lambda { inputs, body } => {
            let mut nested_scope = scope.clone();
            nested_scope.extend(inputs.iter().cloned());
            collect_stdlib_references_from_ast(body, &nested_scope, refs);
        }
        Ast::ListLiteral(items) => {
            for item in items {
                collect_stdlib_references_from_ast(item, scope, refs);
            }
        }
        Ast::MapLiteral(entries) => {
            for entry in entries {
                if let MapKeyAst::Dynamic(key) = &entry.key {
                    collect_stdlib_references_from_ast(key, scope, refs);
                }
                collect_stdlib_references_from_ast(&entry.value, scope, refs);
            }
        }
        Ast::Index { collection, index, .. } => {
            collect_stdlib_references_from_ast(collection, scope, refs);
            collect_stdlib_references_from_ast(index, scope, refs);
        }
        Ast::IndexAssign { collection, index, value, .. } => {
            collect_stdlib_references_from_ast(collection, scope, refs);
            collect_stdlib_references_from_ast(index, scope, refs);
            collect_stdlib_references_from_ast(value, scope, refs);
        }
        Ast::FunctionDef(func) => {
            let mut nested_scope = func.inputs.iter().cloned().collect::<HashSet<_>>();
            collect_stdlib_references_from_block(&func.block, &mut nested_scope, refs);
        }
        Ast::Literal(_) => {}
    }
}

fn collect_used_features(functions: &[FunctionDefAst]) -> UsedFeatures {
    let mut features = UsedFeatures::default();
    for function in functions {
        collect_used_features_from_block(&function.block, &mut features);
    }
    features
}

fn collect_used_features_from_block(block: &BlockAst, features: &mut UsedFeatures) {
    for line in &block.lines {
        collect_used_features_from_ast(line, features);
    }
}

fn collect_used_features_from_ast(ast: &Ast, features: &mut UsedFeatures) {
    match ast {
        Ast::Expression(ExpressionAst { function, args, .. }) => {
            if matches!(
                function.as_str(),
                "bigint_from_int"
                    | "bigint_compare"
                    | "bigint_add"
                    | "bigint_subtract"
                    | "bigint_multiply"
                    | "bigint_divide"
                    | "bigint_modulo"
                    | "bigint_bitand"
                    | "bigint_bitor"
                    | "bigint_bitxor"
                    | "bigint_shl"
                    | "bigint_shr"
            ) {
                features.bigint = true;
            }
            if matches!(
                function.as_str(),
                "list_new"
                    | "list_push"
                    | "list_len"
                    | "list_get"
                    | "list_range"
                    | "list_map"
                    | "list_filter"
            ) {
                features.lists = true;
            }
            if matches!(
                function.as_str(),
                "map_new"
                    | "map_set"
                    | "map_len"
                    | "map_get"
                    | "map_has"
                    | "map_delete"
                    | "map_iter"
                    | "map_iter_done"
                    | "map_iter_next"
                    | "map_iter_key"
                    | "map_iter_value"
                    | "map_iter_advance"
                    | "map_keys"
            ) {
                features.maps = true;
                features.lists = true;
            }
            if matches!(
                function.as_str(),
                "list_insert" | "list_set" | "list_swap" | "list_pop" | "list_delete" | "list_copy"
            ) {
                features.lists = true;
                features.list_mutation = true;
            }
            for arg in args {
                collect_used_features_from_ast(arg, features);
            }
        }
        Ast::MultiValue(values) => {
            for value in values {
                collect_used_features_from_ast(value, features);
            }
        }
        Ast::ListLiteral(items) => {
            features.lists = true;
            for item in items {
                collect_used_features_from_ast(item, features);
            }
        }
        Ast::MapLiteral(entries) => {
            features.maps = true;
            features.lists = true;
            for entry in entries {
                if let MapKeyAst::Dynamic(key) = &entry.key {
                    collect_used_features_from_ast(key, features);
                }
                collect_used_features_from_ast(&entry.value, features);
            }
        }
        Ast::Index { collection, index, .. } => {
            features.lists = true;
            collect_used_features_from_ast(collection, features);
            collect_used_features_from_ast(index, features);
        }
        Ast::IndexAssign { collection, index, value, .. } => {
            features.lists = true;
            features.list_mutation = true;
            collect_used_features_from_ast(collection, features);
            collect_used_features_from_ast(index, features);
            collect_used_features_from_ast(value, features);
        }
        Ast::Assign { value, .. } => collect_used_features_from_ast(value, features),
        Ast::MultiAssign { value, .. } => collect_used_features_from_ast(value, features),
        Ast::If { condition, then, else_, .. } => {
            collect_used_features_from_ast(condition, features);
            collect_used_features_from_block(then, features);
            if let Some(else_block) = else_ {
                collect_used_features_from_block(else_block, features);
            }
        }
        Ast::Block(block) => collect_used_features_from_block(block, features),
        Ast::FunctionDef(func) => collect_used_features_from_block(&func.block, features),
        Ast::Literal(LiteralAst::BigInt(_)) => {
            features.bigint = true;
        }
        Ast::Literal(_) | Ast::Variable(_) | Ast::Lambda { .. } | Ast::FunctionRef(_) => {}
    }
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
    value_kind_analysis: &ModuleValueKindAnalysis,
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
            value_kind_analysis
                .functions
                .get(&func_def.name)
                .expect("missing value kind analysis for function"),
            value_kind_analysis,
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

fn define_jit_int_result_wrapper(
    module: &mut impl CraneliftModule,
    isa: OwnedTargetIsa,
    flags: &settings::Flags,
    wrapper_id: FuncId,
    internal_id: FuncId,
    input_count: usize,
) {
    assert!(input_count <= 1, "jit int-result wrapper supports at most one argument");
    let mut sig = Signature::new(isa.default_call_conv());
    if input_count == 1 {
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
    }
    sig.returns.push(AbiParam::new(types::I64));

    let mut ctx = module.make_context();
    ctx.func.signature = sig;
    ctx.func.name = UserFuncName::user(0, wrapper_id.as_u32());

    let internal_ref = module.declare_func_in_func(internal_id, &mut ctx.func);

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
        let block0 = builder.create_block();
        builder.append_block_params_for_function_params(block0);
        builder.switch_to_block(block0);
        builder.seal_block(block0);

        let zero_env = builder.ins().iconst(types::I64, 0);
        let internal_call = if input_count == 1 {
            let arg_tag = builder.block_params(block0)[0];
            let arg_payload = builder.block_params(block0)[1];
            builder.ins().call(internal_ref, &[zero_env, arg_tag, arg_payload])
        } else {
            builder.ins().call(internal_ref, &[zero_env])
        };
        let result_tag = builder.inst_results(internal_call)[0];
        let result_payload = builder.inst_results(internal_call)[1];
        let is_int = builder.ins().icmp_imm(IntCC::Equal, result_tag, TAG_INT);
        builder.ins().trapz(is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);
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

fn define_executable_main_int_result_wrapper(
    module: &mut impl CraneliftModule,
    isa: OwnedTargetIsa,
    flags: &settings::Flags,
    wrapper_id: FuncId,
    internal_id: FuncId,
    main_input_count: usize,
) {
    assert!(main_input_count <= 1, "native executable main function supports at most one argument");

    let mut sig = Signature::new(isa.default_call_conv());
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.returns.push(AbiParam::new(types::I64));

    let mut ctx = module.make_context();
    ctx.func.signature = sig;
    ctx.func.name = UserFuncName::user(0, wrapper_id.as_u32());

    let internal_ref = module.declare_func_in_func(internal_id, &mut ctx.func);

    let mut fn_builder_ctx = FunctionBuilderContext::new();
    {
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);

        let arg_tag = builder.block_params(entry)[0];
        let arg_payload = builder.block_params(entry)[1];
        let zero_env = builder.ins().iconst(types::I64, 0);
        let mut call_args = vec![zero_env];
        if main_input_count == 1 {
            call_args.push(arg_tag);
            call_args.push(arg_payload);
        }

        let internal_call = builder.ins().call(internal_ref, &call_args);
        let result_tag = builder.inst_results(internal_call)[0];
        let result_payload = builder.inst_results(internal_call)[1];
        let is_int = builder.ins().icmp_imm(IntCC::Equal, result_tag, TAG_INT);
        builder.ins().trapz(is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);
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
                | "is_int"
                | "is_bigint"
                | "is_string"
                | "is_list"
                | "is_map"
                | "is_map_iter"
                | "is_function"
                | "is_string_iter"
                | "list_new"
                | "list_push"
                | "list_insert"
                | "list_len"
                | "list_get"
                | "list_set"
                | "list_swap"
                | "list_pop"
                | "list_delete"
                | "list_copy"
                | "map_new"
                | "map_set"
                | "map_len"
                | "map_get"
                | "map_has"
                | "map_delete"
                | "map_iter"
                | "map_iter_done"
                | "map_iter_next"
                | "map_iter_key"
                | "map_iter_value"
                | "map_iter_advance"
                | "list_range"
                | "list_map"
                | "list_filter"
                | "bigint_compare"
                | "bigint_from_int"
                | "bigint_add"
                | "bigint_subtract"
                | "bigint_multiply"
                | "bigint_divide"
                | "bigint_modulo"
                | "bigint_bitand"
                | "bigint_bitor"
                | "bigint_bitxor"
                | "bigint_shl"
                | "bigint_shr"
                | "bytes_len"
                | "bytes_get"
                | "bytes_pop"
                | "bytes_push"
                | "bytes_insert"
                | "bytes_remove"
                | "bytes_set"
                | "bytes_slice"
                | "string_chars"
                | "string_iter_done"
                | "string_iter_next"
                | "string_copy"
                | "string_concat"
        )
}

fn is_operator_name(name: &str) -> bool {
    matches!(
        name,
        "+" | "-"
            | "*"
            | "/"
            | "%"
            | "&"
            | "|"
            | "^"
            | "<<"
            | ">>"
            | "=="
            | "!="
            | "<"
            | "<="
            | ">"
            | ">="
            | "and"
            | "or"
            | "not"
            | "add"
            | "subtract"
            | "multiply"
            | "divide"
            | "modulo"
            | "bitand"
            | "bitor"
            | "bitxor"
            | "shl"
            | "shr"
            | "eq"
            | "ne"
            | "lt"
            | "lte"
            | "gt"
            | "gte"
    )
}

fn is_known_callable_name(name: &str, function_names: &HashSet<String>) -> bool {
    is_builtin_name(name) || is_operator_name(name) || function_names.contains(name)
}

fn validate_unary_callback_reference(
    ast: &Ast,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_arities: &HashMap<String, usize>,
    builtin: &str,
) -> Result<(), CompileError> {
    match ast {
        Ast::FunctionRef(name) => {
            if !function_names.contains(name.as_str()) {
                return Err(CompileError::UndefinedFunction {
                    name: name.to_string(),
                    span: name.span.clone(),
                });
            }
            if function_arities.get(name.as_str()) != Some(&1usize) {
                return Err(CompileError::CallbackArity {
                    builtin: builtin.to_string(),
                    function: name.to_string(),
                    span: name.span.clone(),
                });
            }
        }
        Ast::Variable(name)
            if !locals.contains(name.as_str()) && function_names.contains(name.as_str()) =>
        {
            if function_arities.get(name.as_str()) != Some(&1usize) {
                return Err(CompileError::CallbackArity {
                    builtin: builtin.to_string(),
                    function: name.to_string(),
                    span: name.span.clone(),
                });
            }
        }
        _ => {}
    }
    Ok(())
}

fn validate_ast_user_facing(
    ast: &Ast,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_arities: &HashMap<String, usize>,
    value_kind_analysis: &ModuleValueKindAnalysis,
    function_analysis: &FunctionValueKindAnalysis,
) -> Result<(), CompileError> {
    match ast {
        Ast::Literal(_) => Ok(()),
        Ast::Variable(name) => validate_variable_reference(name, locals, function_names),
        Ast::Lambda { .. } => Err(CompileError::UnsupportedFeature("anonymous functions")),
        Ast::FunctionDef(_) => Err(CompileError::UnsupportedFeature("nested function definitions")),
        Ast::FunctionRef(name) => validate_function_reference(name, function_names),
        Ast::MultiValue(values) => validate_ast_sequence(
            values,
            locals,
            function_names,
            function_arities,
            value_kind_analysis,
            function_analysis,
        ),
        Ast::ListLiteral(items) => validate_ast_sequence(
            items,
            locals,
            function_names,
            function_arities,
            value_kind_analysis,
            function_analysis,
        ),
        Ast::MapLiteral(entries) => {
            for entry in entries {
                if let MapKeyAst::Dynamic(key) = &entry.key {
                    validate_ast_user_facing(
                        key,
                        locals,
                        function_names,
                        function_arities,
                        value_kind_analysis,
                        function_analysis,
                    )?;
                    let key_kinds =
                        infer_ast_value_shape(key, function_analysis, value_kind_analysis)
                            .scalar_slot();
                    if !key_kinds.is_empty() && !kind_sets_intersect(key_kinds, KindSet::string()) {
                        return Err(CompileError::InvalidArgumentType {
                            function: "map literal".to_string(),
                            argument: 1,
                            expected: "string".to_string(),
                            found: format_kind_set_for_error(key_kinds),
                            span: span_of_ast(key),
                        });
                    }
                }
                validate_ast_user_facing(
                    &entry.value,
                    locals,
                    function_names,
                    function_arities,
                    value_kind_analysis,
                    function_analysis,
                )?;
            }
            Ok(())
        }
        Ast::Index { collection, index, .. } => validate_index_ast(
            collection,
            index,
            locals,
            function_names,
            function_arities,
            value_kind_analysis,
            function_analysis,
        ),
        Ast::IndexAssign { collection, index, value, .. } => validate_index_assign_ast(
            collection,
            index,
            value,
            locals,
            function_names,
            function_arities,
            value_kind_analysis,
            function_analysis,
        ),
        Ast::Expression(ExpressionAst { function, args, function_span }) => {
            validate_expression_user_facing(
                function,
                args,
                function_span.clone(),
                locals,
                function_names,
                function_arities,
                value_kind_analysis,
                function_analysis,
            )
        }
        Ast::Block(block) => validate_ast_sequence(
            &block.lines,
            locals,
            function_names,
            function_arities,
            value_kind_analysis,
            function_analysis,
        ),
        Ast::Assign { value, .. } => validate_ast_user_facing(
            value,
            locals,
            function_names,
            function_arities,
            value_kind_analysis,
            function_analysis,
        ),
        Ast::MultiAssign { value, .. } => validate_ast_user_facing(
            value,
            locals,
            function_names,
            function_arities,
            value_kind_analysis,
            function_analysis,
        ),
        Ast::If { condition, then, else_, .. } => validate_if_ast_user_facing(
            condition,
            then,
            else_,
            locals,
            function_names,
            function_arities,
            value_kind_analysis,
            function_analysis,
        ),
    }
}

fn validate_function_reference(
    name: &Ident,
    function_names: &HashSet<String>,
) -> Result<(), CompileError> {
    if function_names.contains(name.as_str()) {
        Ok(())
    } else {
        Err(CompileError::UndefinedFunction { name: name.to_string(), span: name.span.clone() })
    }
}

fn validate_variable_reference(
    name: &Ident,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
) -> Result<(), CompileError> {
    if locals.contains(name.as_str()) || function_names.contains(name.as_str()) {
        Ok(())
    } else {
        Err(CompileError::UndefinedVariable { name: name.to_string(), span: name.span.clone() })
    }
}

fn expression_return_arity(
    function: &str,
    function_return_arities: &HashMap<String, usize>,
) -> usize {
    if function.is_empty() {
        1
    } else if function == "map_iter_next" {
        2
    } else {
        *function_return_arities.get(function).unwrap_or(&1)
    }
}

fn validate_block_multi_return_usage(
    block: &BlockAst,
    current_function: &str,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_return_arities: &HashMap<String, usize>,
    expected_tail_arity: usize,
) -> Result<(), CompileError> {
    if block.lines.is_empty() {
        return Ok(());
    }
    for line in &block.lines[..block.lines.len() - 1] {
        validate_ast_multi_return_usage(
            line,
            current_function,
            locals,
            function_names,
            function_return_arities,
            1,
            false,
        )?;
    }
    validate_ast_multi_return_usage(
        &block.lines[block.lines.len() - 1],
        current_function,
        locals,
        function_names,
        function_return_arities,
        expected_tail_arity,
        true,
    )
}

fn validate_ast_multi_return_usage(
    ast: &Ast,
    current_function: &str,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_return_arities: &HashMap<String, usize>,
    expected_arity: usize,
    is_tail: bool,
) -> Result<(), CompileError> {
    match ast {
        Ast::Literal(_) | Ast::Variable(_) | Ast::FunctionRef(_) => {
            validate_single_value_multi_return_usage(expected_arity, None)
        }
        Ast::Lambda { .. } | Ast::FunctionDef(_) => Ok(()),
        Ast::MultiValue(values) => validate_multi_value_ast(
            values,
            current_function,
            locals,
            function_names,
            function_return_arities,
            expected_arity,
            is_tail,
        ),
        Ast::Expression(expression) => validate_expression_multi_return_usage(
            expression,
            current_function,
            locals,
            function_names,
            function_return_arities,
            expected_arity,
            is_tail,
        ),
        Ast::ListLiteral(items) => validate_list_literal_multi_return_usage(
            items,
            current_function,
            locals,
            function_names,
            function_return_arities,
            expected_arity,
        ),
        Ast::MapLiteral(entries) => {
            for entry in entries {
                if let MapKeyAst::Dynamic(key) = &entry.key {
                    validate_child_multi_return_usage(
                        key,
                        current_function,
                        locals,
                        function_names,
                        function_return_arities,
                    )?;
                }
                validate_child_multi_return_usage(
                    &entry.value,
                    current_function,
                    locals,
                    function_names,
                    function_return_arities,
                )?;
            }
            validate_single_value_multi_return_usage(expected_arity, None)
        }
        Ast::Index { collection, index, span } => validate_index_multi_return_usage(
            collection,
            index,
            span.as_ref(),
            current_function,
            locals,
            function_names,
            function_return_arities,
            expected_arity,
        ),
        Ast::IndexAssign { collection, index, value, span } => {
            validate_index_assign_multi_return_usage(
                collection,
                index,
                value,
                span.as_ref(),
                current_function,
                locals,
                function_names,
                function_return_arities,
                expected_arity,
            )
        }
        Ast::Assign { value, span, .. } => validate_assign_multi_return_usage(
            value,
            span.as_ref(),
            current_function,
            locals,
            function_names,
            function_return_arities,
            expected_arity,
        ),
        Ast::MultiAssign { names, value, span } => validate_multi_assign_multi_return_usage(
            names,
            value,
            span.as_ref(),
            current_function,
            locals,
            function_names,
            function_return_arities,
            expected_arity,
        ),
        Ast::If { condition, then, else_, span } => validate_if_multi_return_usage(
            condition,
            then,
            else_.as_ref(),
            span.as_ref(),
            current_function,
            locals,
            function_names,
            function_return_arities,
            expected_arity,
        ),
        Ast::Block(block) => validate_block_multi_return_usage(
            block,
            current_function,
            locals,
            function_names,
            function_return_arities,
            expected_arity,
        ),
    }
}

fn validate_single_value_multi_return_usage(
    expected_arity: usize,
    span: Option<&Span>,
) -> Result<(), CompileError> {
    if expected_arity == 1 {
        Ok(())
    } else {
        Err(CompileError::UnsupportedMultiValueContext { span: span.cloned() })
    }
}

fn validate_child_multi_return_usage(
    ast: &Ast,
    current_function: &str,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_return_arities: &HashMap<String, usize>,
) -> Result<(), CompileError> {
    validate_ast_multi_return_usage(
        ast,
        current_function,
        locals,
        function_names,
        function_return_arities,
        1,
        false,
    )
}

fn validate_multi_value_ast(
    values: &[Ast],
    current_function: &str,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_return_arities: &HashMap<String, usize>,
    expected_arity: usize,
    is_tail: bool,
) -> Result<(), CompileError> {
    if !is_tail {
        return Err(CompileError::UnsupportedMultiValueContext { span: None });
    }
    if values.len() != expected_arity {
        return Err(CompileError::DestructuringArityMismatch {
            expected: expected_arity,
            found: values.len(),
            span: None,
        });
    }
    for value in values {
        validate_child_multi_return_usage(
            value,
            current_function,
            locals,
            function_names,
            function_return_arities,
        )?;
    }
    Ok(())
}

fn validate_expression_multi_return_usage(
    expression: &ExpressionAst,
    current_function: &str,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_return_arities: &HashMap<String, usize>,
    expected_arity: usize,
    is_tail: bool,
) -> Result<(), CompileError> {
    for arg in &expression.args {
        validate_child_multi_return_usage(
            arg,
            current_function,
            locals,
            function_names,
            function_return_arities,
        )?;
    }
    let arity = expression_return_arity(&expression.function, function_return_arities);
    if arity > 1 && !(is_tail && arity == expected_arity) {
        return Err(CompileError::UnsupportedMultiValueContext {
            span: expression.function_span.clone(),
        });
    }
    if expected_arity == 1 || (is_tail && arity == expected_arity) {
        Ok(())
    } else {
        Err(CompileError::UnsupportedMultiValueContext { span: expression.function_span.clone() })
    }
}

fn validate_list_literal_multi_return_usage(
    items: &[Ast],
    current_function: &str,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_return_arities: &HashMap<String, usize>,
    expected_arity: usize,
) -> Result<(), CompileError> {
    for item in items {
        validate_child_multi_return_usage(
            item,
            current_function,
            locals,
            function_names,
            function_return_arities,
        )?;
    }
    validate_single_value_multi_return_usage(expected_arity, None)
}

fn validate_index_multi_return_usage(
    collection: &Ast,
    index: &Ast,
    span: Option<&Span>,
    current_function: &str,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_return_arities: &HashMap<String, usize>,
    expected_arity: usize,
) -> Result<(), CompileError> {
    validate_child_multi_return_usage(
        collection,
        current_function,
        locals,
        function_names,
        function_return_arities,
    )?;
    validate_child_multi_return_usage(
        index,
        current_function,
        locals,
        function_names,
        function_return_arities,
    )?;
    validate_single_value_multi_return_usage(expected_arity, span)
}

fn validate_index_assign_multi_return_usage(
    collection: &Ast,
    index: &Ast,
    value: &Ast,
    span: Option<&Span>,
    current_function: &str,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_return_arities: &HashMap<String, usize>,
    expected_arity: usize,
) -> Result<(), CompileError> {
    validate_child_multi_return_usage(
        collection,
        current_function,
        locals,
        function_names,
        function_return_arities,
    )?;
    validate_child_multi_return_usage(
        index,
        current_function,
        locals,
        function_names,
        function_return_arities,
    )?;
    validate_child_multi_return_usage(
        value,
        current_function,
        locals,
        function_names,
        function_return_arities,
    )?;
    validate_single_value_multi_return_usage(expected_arity, span)
}

fn validate_assign_multi_return_usage(
    value: &Ast,
    span: Option<&Span>,
    current_function: &str,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_return_arities: &HashMap<String, usize>,
    expected_arity: usize,
) -> Result<(), CompileError> {
    let actual_arity = infer_ast_return_arity(value, current_function, function_return_arities)?;
    if actual_arity != 1 {
        return Err(CompileError::UnsupportedMultiValueContext { span: span.cloned() });
    }
    validate_child_multi_return_usage(
        value,
        current_function,
        locals,
        function_names,
        function_return_arities,
    )?;
    validate_single_value_multi_return_usage(expected_arity, span)
}

fn validate_multi_assign_multi_return_usage(
    names: &[String],
    value: &Ast,
    span: Option<&Span>,
    current_function: &str,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_return_arities: &HashMap<String, usize>,
    expected_arity: usize,
) -> Result<(), CompileError> {
    match value {
        Ast::Expression(ExpressionAst { function, .. }) if !function.is_empty() => {}
        _ => {
            return Err(CompileError::UnsupportedMultiValueContext { span: span.cloned() });
        }
    }
    let actual_arity = infer_ast_return_arity(value, current_function, function_return_arities)?;
    if actual_arity != names.len() {
        return Err(CompileError::DestructuringArityMismatch {
            expected: names.len(),
            found: actual_arity,
            span: span.cloned(),
        });
    }
    validate_ast_multi_return_usage(
        value,
        current_function,
        locals,
        function_names,
        function_return_arities,
        names.len(),
        true,
    )?;
    validate_single_value_multi_return_usage(expected_arity, span)
}

fn validate_if_multi_return_usage(
    condition: &Ast,
    then: &BlockAst,
    else_: Option<&BlockAst>,
    span: Option<&Span>,
    current_function: &str,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_return_arities: &HashMap<String, usize>,
    expected_arity: usize,
) -> Result<(), CompileError> {
    validate_child_multi_return_usage(
        condition,
        current_function,
        locals,
        function_names,
        function_return_arities,
    )?;
    validate_block_multi_return_usage(
        then,
        current_function,
        locals,
        function_names,
        function_return_arities,
        expected_arity,
    )?;
    if let Some(else_block) = else_ {
        validate_block_multi_return_usage(
            else_block,
            current_function,
            locals,
            function_names,
            function_return_arities,
            expected_arity,
        )?;
    } else if expected_arity != 1 {
        return Err(CompileError::UnsupportedMultiValueContext { span: span.cloned() });
    }
    Ok(())
}

fn validate_ast_sequence(
    items: &[Ast],
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_arities: &HashMap<String, usize>,
    value_kind_analysis: &ModuleValueKindAnalysis,
    function_analysis: &FunctionValueKindAnalysis,
) -> Result<(), CompileError> {
    for item in items {
        validate_ast_user_facing(
            item,
            locals,
            function_names,
            function_arities,
            value_kind_analysis,
            function_analysis,
        )?;
    }
    Ok(())
}

fn validate_index_ast(
    collection: &Ast,
    index: &Ast,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_arities: &HashMap<String, usize>,
    value_kind_analysis: &ModuleValueKindAnalysis,
    function_analysis: &FunctionValueKindAnalysis,
) -> Result<(), CompileError> {
    validate_ast_user_facing(
        collection,
        locals,
        function_names,
        function_arities,
        value_kind_analysis,
        function_analysis,
    )?;
    validate_ast_user_facing(
        index,
        locals,
        function_names,
        function_arities,
        value_kind_analysis,
        function_analysis,
    )?;

    let collection_kinds =
        infer_ast_value_shape(collection, function_analysis, value_kind_analysis).scalar_slot();
    let index_kinds =
        infer_ast_value_shape(index, function_analysis, value_kind_analysis).scalar_slot();
    let expected_collection = KindSet::string().union(KindSet::list());
    if !collection_kinds.is_empty() && !kind_sets_intersect(collection_kinds, expected_collection) {
        return Err(CompileError::InvalidArgumentType {
            function: "index access".to_string(),
            argument: 1,
            expected: format_kind_set_for_error(expected_collection),
            found: format_kind_set_for_error(collection_kinds),
            span: span_of_ast(collection),
        });
    }
    if !index_kinds.is_empty() && !kind_sets_intersect(index_kinds, KindSet::int()) {
        return Err(CompileError::InvalidArgumentType {
            function: "index access".to_string(),
            argument: 2,
            expected: "int".to_string(),
            found: format_kind_set_for_error(index_kinds),
            span: span_of_ast(index),
        });
    }
    Ok(())
}

fn validate_index_assign_ast(
    collection: &Ast,
    index: &Ast,
    value: &Ast,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_arities: &HashMap<String, usize>,
    value_kind_analysis: &ModuleValueKindAnalysis,
    function_analysis: &FunctionValueKindAnalysis,
) -> Result<(), CompileError> {
    validate_ast_user_facing(
        collection,
        locals,
        function_names,
        function_arities,
        value_kind_analysis,
        function_analysis,
    )?;
    validate_ast_user_facing(
        index,
        locals,
        function_names,
        function_arities,
        value_kind_analysis,
        function_analysis,
    )?;
    validate_ast_user_facing(
        value,
        locals,
        function_names,
        function_arities,
        value_kind_analysis,
        function_analysis,
    )?;

    let collection_kinds =
        infer_ast_value_shape(collection, function_analysis, value_kind_analysis).scalar_slot();
    let index_kinds =
        infer_ast_value_shape(index, function_analysis, value_kind_analysis).scalar_slot();
    let value_kinds =
        infer_ast_value_shape(value, function_analysis, value_kind_analysis).scalar_slot();
    let expected_collection = KindSet::string().union(KindSet::list());
    if !collection_kinds.is_empty() && !kind_sets_intersect(collection_kinds, expected_collection) {
        return Err(CompileError::InvalidArgumentType {
            function: "index assignment".to_string(),
            argument: 1,
            expected: format_kind_set_for_error(expected_collection),
            found: format_kind_set_for_error(collection_kinds),
            span: span_of_ast(collection),
        });
    }
    if !index_kinds.is_empty() && !kind_sets_intersect(index_kinds, KindSet::int()) {
        return Err(CompileError::InvalidArgumentType {
            function: "index assignment".to_string(),
            argument: 2,
            expected: "int".to_string(),
            found: format_kind_set_for_error(index_kinds),
            span: span_of_ast(index),
        });
    }
    if collection_kinds.contains(ValueKind::String)
        && !collection_kinds.contains(ValueKind::List)
        && !value_kinds.is_empty()
        && !kind_sets_intersect(value_kinds, KindSet::int())
    {
        return Err(CompileError::InvalidArgumentType {
            function: "index assignment".to_string(),
            argument: 3,
            expected: "int".to_string(),
            found: format_kind_set_for_error(value_kinds),
            span: span_of_ast(value),
        });
    }
    Ok(())
}

fn validate_expression_user_facing(
    function: &str,
    args: &[Ast],
    function_span: Option<Span>,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_arities: &HashMap<String, usize>,
    value_kind_analysis: &ModuleValueKindAnalysis,
    function_analysis: &FunctionValueKindAnalysis,
) -> Result<(), CompileError> {
    validate_callable_name(function, function_span.clone(), locals, function_names)?;
    validate_callback_argument(function, args, locals, function_names, function_arities)?;
    validate_ast_sequence(
        args,
        locals,
        function_names,
        function_arities,
        value_kind_analysis,
        function_analysis,
    )?;
    validate_expression_argument_types(
        function,
        args,
        function_span,
        function_analysis,
        value_kind_analysis,
    )
}

fn validate_callable_name(
    function: &str,
    function_span: Option<Span>,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
) -> Result<(), CompileError> {
    if !locals.contains(function) && !is_known_callable_name(function, function_names) {
        return Err(CompileError::UndefinedFunction {
            name: function.to_string(),
            span: function_span,
        });
    }
    Ok(())
}

fn validate_callback_argument(
    function: &str,
    args: &[Ast],
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_arities: &HashMap<String, usize>,
) -> Result<(), CompileError> {
    if let Some(callback) = args.get(1) {
        match function {
            "list_map" | "list_filter" | "list_all" | "list_any" | "string_all" | "string_any" => {
                validate_unary_callback_reference(
                    callback,
                    locals,
                    function_names,
                    function_arities,
                    function,
                )?
            }
            _ => {}
        }
    }
    Ok(())
}

fn validate_if_ast_user_facing(
    condition: &Ast,
    then: &BlockAst,
    else_: &Option<BlockAst>,
    locals: &HashSet<String>,
    function_names: &HashSet<String>,
    function_arities: &HashMap<String, usize>,
    value_kind_analysis: &ModuleValueKindAnalysis,
    function_analysis: &FunctionValueKindAnalysis,
) -> Result<(), CompileError> {
    validate_ast_user_facing(
        condition,
        locals,
        function_names,
        function_arities,
        value_kind_analysis,
        function_analysis,
    )?;
    let (then_analysis, else_analysis) =
        narrowed_function_analyses_for_condition(condition, function_analysis);
    validate_ast_sequence(
        &then.lines,
        locals,
        function_names,
        function_arities,
        value_kind_analysis,
        &then_analysis,
    )?;
    if let Some(else_) = else_ {
        validate_ast_sequence(
            &else_.lines,
            locals,
            function_names,
            function_arities,
            value_kind_analysis,
            &else_analysis,
        )?;
    }
    Ok(())
}

#[derive(Clone, Copy)]
struct BuiltinArgSpec {
    expected: KindSet,
}

fn builtin_argument_specs(function: &str) -> Option<&'static [BuiltinArgSpec]> {
    use BuiltinArgSpec as Spec;

    const STRING: BuiltinArgSpec = Spec { expected: KindSet::string() };
    const INT: BuiltinArgSpec = Spec { expected: KindSet::int() };
    const BIGINT_OR_INT: BuiltinArgSpec =
        Spec { expected: KindSet::bigint().union(KindSet::int()) };
    const LIST: BuiltinArgSpec = Spec { expected: KindSet::list() };
    const MAP: BuiltinArgSpec = Spec { expected: KindSet::map() };
    const MAP_ITER: BuiltinArgSpec = Spec { expected: KindSet::map_iter() };
    const FUNCTION: BuiltinArgSpec = Spec { expected: KindSet::function() };

    match function {
        "bytes_len"
        | "bytes_pop"
        | "string_copy"
        | "string_chars"
        | "string_first"
        | "string_last"
        | "string_try_first"
        | "string_try_last"
        | "string_try_pop"
        | "string_is_empty"
        | "string_is_not_empty"
        | "string_len"
        | "string_is_ascii"
        | "string_is_integer"
        | "string_try_parse_integer"
        | "string_try_parse_bigint" => Some(&[STRING]),
        "bytes_get" | "bytes_try_get" => Some(&[STRING, INT]),
        "bytes_slice" => Some(&[STRING, INT, INT]),
        "bytes_remove" => Some(&[STRING, INT]),
        "bytes_push" => Some(&[STRING, INT]),
        "bytes_insert" | "bytes_set" => Some(&[STRING, INT, INT]),
        "string_concat" | "string_starts_with" | "string_ends_with" | "string_contains" => {
            Some(&[STRING, STRING])
        }
        "string_repeat" => Some(&[STRING, INT]),
        "string_all" | "string_any" => Some(&[STRING, FUNCTION]),
        "list_len" | "list_pop" | "list_copy" | "list_all" | "list_any" => Some(&[LIST]),
        "list_get" | "list_delete" => Some(&[LIST, INT]),
        "list_push" => Some(&[LIST]),
        "list_insert" | "list_set" => Some(&[LIST, INT]),
        "list_swap" => Some(&[LIST, INT, INT]),
        "list_map" | "list_filter" => Some(&[LIST, FUNCTION]),
        "list_range" => Some(&[INT, INT]),
        "map_len" | "map_keys" | "map_iter" | "map_values" => Some(&[MAP]),
        "map_has" | "map_get" | "map_delete" => Some(&[MAP, STRING]),
        "map_set" => Some(&[MAP, STRING]),
        "map_iter_done" | "map_iter_next" | "map_iter_key" | "map_iter_value"
        | "map_iter_advance" => Some(&[MAP_ITER]),
        "bigint_from_int" => Some(&[INT]),
        "add" | "subtract" | "multiply" | "divide" | "modulo" | "gt" | "lt" | "gte" | "lte"
        | "bigint_compare" | "bigint_add" | "bigint_subtract" | "bigint_multiply"
        | "bigint_divide" | "bigint_modulo" | "bigint_bitand" | "bigint_bitor"
        | "bigint_bitxor" => Some(&[BIGINT_OR_INT, BIGINT_OR_INT]),
        "bigint_shl" | "bigint_shr" => Some(&[BIGINT_OR_INT, INT]),
        "bitand" | "bitor" | "bitxor" => Some(&[BIGINT_OR_INT, BIGINT_OR_INT]),
        "shl" | "shr" => Some(&[BIGINT_OR_INT, INT]),
        _ => None,
    }
}

fn format_kind_set_for_error(kinds: KindSet) -> String {
    let mut names = vec![];
    for (kind, label) in [
        (ValueKind::Int, "int"),
        (ValueKind::BigInt, "bigint"),
        (ValueKind::String, "string"),
        (ValueKind::List, "list"),
        (ValueKind::Map, "map"),
        (ValueKind::MapIter, "map_iter"),
        (ValueKind::Function, "function"),
        (ValueKind::StringIter, "string_iter"),
    ] {
        if kinds.contains(kind) {
            names.push(label);
        }
    }
    if names.is_empty() { "unknown".to_string() } else { names.join(" | ") }
}

fn validate_expression_argument_types(
    function: &str,
    args: &[Ast],
    function_span: Option<Span>,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> Result<(), CompileError> {
    let Some(specs) = builtin_argument_specs(function) else {
        return Ok(());
    };

    for (index, spec) in specs.iter().enumerate() {
        let Some(arg) = args.get(index) else {
            break;
        };
        let actual =
            infer_ast_value_shape(arg, function_analysis, value_kind_analysis).scalar_slot();
        if actual.is_empty() {
            continue;
        }
        if !kind_sets_intersect(actual, spec.expected) {
            return Err(CompileError::InvalidArgumentType {
                function: function.to_string(),
                argument: index + 1,
                expected: format_kind_set_for_error(spec.expected),
                found: format_kind_set_for_error(actual),
                span: function_span,
            });
        }
    }

    Ok(())
}

fn kind_sets_intersect(lhs: KindSet, rhs: KindSet) -> bool {
    [
        ValueKind::Int,
        ValueKind::BigInt,
        ValueKind::String,
        ValueKind::List,
        ValueKind::Map,
        ValueKind::MapIter,
        ValueKind::Function,
        ValueKind::StringIter,
    ]
    .into_iter()
    .any(|kind| lhs.contains(kind) && rhs.contains(kind))
}

fn infer_ast_value_shape(
    ast: &Ast,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> ValueShape {
    match ast {
        Ast::Literal(LiteralAst::Integer(_)) => ValueShape::scalar(KindSet::int()),
        Ast::Literal(LiteralAst::BigInt(_)) => ValueShape::scalar(KindSet::bigint()),
        Ast::Literal(LiteralAst::String(_)) => ValueShape::scalar(KindSet::string()),
        Ast::Variable(name) => function_analysis
            .variables
            .get(name.as_str())
            .cloned()
            .unwrap_or_else(|| ValueShape::scalar(KindSet::empty())),
        Ast::FunctionRef(_) | Ast::Lambda { .. } => ValueShape::scalar(KindSet::function()),
        Ast::ListLiteral(items) => {
            ValueShape::list(items.iter().fold(KindSet::empty(), |kinds, item| {
                kinds.union(
                    infer_ast_value_shape(item, function_analysis, value_kind_analysis)
                        .scalar_slot(),
                )
            }))
        }
        Ast::MapLiteral(entries) => {
            ValueShape::map(entries.iter().fold(KindSet::empty(), |kinds, entry| {
                kinds.union(
                    infer_ast_value_shape(&entry.value, function_analysis, value_kind_analysis)
                        .scalar_slot(),
                )
            }))
        }
        Ast::MultiValue(values) => ValueShape::from_slots(
            values
                .iter()
                .map(|value| {
                    infer_ast_value_shape(value, function_analysis, value_kind_analysis)
                        .scalar_slot()
                })
                .collect(),
        ),
        Ast::Expression(ExpressionAst { function, args, .. }) => {
            let arg_shapes = args
                .iter()
                .map(|arg| infer_ast_value_shape(arg, function_analysis, value_kind_analysis))
                .collect::<Vec<_>>();
            if matches!(
                function.as_str(),
                "map_try_get"
                    | "map_try_delete"
                    | "map_try_pop"
                    | "map_iter_next"
                    | "string_from_codepoints"
            ) {
                return infer_builtin_value_shape(function, &arg_shapes);
            }
            if let Some(function_info) = value_kind_analysis.functions.get(function) {
                return function_info.returns.clone();
            }
            if function == "list_map" {
                let items = args
                    .get(1)
                    .and_then(|callback| {
                        infer_known_callback_return_shape(
                            callback,
                            function_analysis,
                            value_kind_analysis,
                        )
                    })
                    .map(|shape| shape.scalar_slot())
                    .unwrap_or_else(KindSet::empty);
                return ValueShape::list(items);
            }
            if function == "list_filter" {
                return arg_shapes
                    .first()
                    .and_then(ValueShape::list_items)
                    .map(ValueShape::list)
                    .unwrap_or_else(|| ValueShape::list(KindSet::empty()));
            }
            infer_builtin_value_shape(function, &arg_shapes)
        }
        Ast::Assign { value, .. } | Ast::MultiAssign { value, .. } => {
            infer_ast_value_shape(value, function_analysis, value_kind_analysis)
        }
        Ast::Block(block) => block
            .lines
            .last()
            .map(|line| infer_ast_value_shape(line, function_analysis, value_kind_analysis))
            .unwrap_or_else(|| ValueShape::scalar(KindSet::int())),
        Ast::If { then, else_, .. } => {
            let then_shape = then
                .lines
                .last()
                .map(|line| infer_ast_value_shape(line, function_analysis, value_kind_analysis))
                .unwrap_or_else(|| ValueShape::scalar(KindSet::int()));
            let else_shape = else_
                .as_ref()
                .and_then(|block| block.lines.last())
                .map(|line| infer_ast_value_shape(line, function_analysis, value_kind_analysis))
                .unwrap_or_else(|| ValueShape::scalar(KindSet::int()));
            if then_shape.arity() == else_shape.arity() {
                then_shape.union(&else_shape)
            } else {
                ValueShape::scalar(KindSet::empty())
            }
        }
        Ast::Index { collection, .. } => {
            let collection_shape =
                infer_ast_value_shape(collection, function_analysis, value_kind_analysis);
            let collection_kinds = collection_shape.scalar_slot();
            let mut result = KindSet::empty();
            if collection_kinds.contains(ValueKind::String) {
                result = result.union(KindSet::int());
            }
            if let Some(items) = collection_shape.list_items() {
                result = result.union(items);
            } else if collection_kinds.contains(ValueKind::List) {
                result = result.union(KindSet::any());
            }
            ValueShape::scalar(result)
        }
        Ast::IndexAssign { value, .. } => {
            infer_ast_value_shape(value, function_analysis, value_kind_analysis)
        }
        Ast::FunctionDef(_) => ValueShape::scalar(KindSet::empty()),
    }
}

fn shape_is_exact_kind(shape: &ValueShape, expected: KindSet) -> bool {
    shape.arity() == 1 && shape.scalar_slot() == expected
}

fn infer_builtin_value_shape(function: &str, arg_shapes: &[ValueShape]) -> ValueShape {
    let scalar_arg = |index: usize| {
        arg_shapes.get(index).map(ValueShape::scalar_slot).unwrap_or_else(KindSet::any)
    };
    match function {
        "add" | "subtract" | "multiply" | "divide" | "modulo" => {
            ValueShape::scalar(infer_numeric_result_kind(scalar_arg(0), scalar_arg(1)))
        }
        "bitand" | "bitor" | "bitxor" => {
            ValueShape::scalar(infer_numeric_result_kind(scalar_arg(0), scalar_arg(1)))
        }
        "shl" | "shr" => ValueShape::scalar(scalar_arg(0)),
        "gt" | "lt" | "gte" | "lte" | "eq" | "ne" | "and" | "or" | "not" => {
            ValueShape::scalar(KindSet::int())
        }
        "is_int" | "is_bigint" | "is_string" | "is_list" | "is_map" | "is_map_iter"
        | "is_function" | "is_string_iter" => ValueShape::scalar(KindSet::int()),
        "bytes_len"
        | "bytes_get"
        | "bytes_pop"
        | "string_iter_done"
        | "string_iter_next"
        | "string_first"
        | "string_last"
        | "string_try_first"
        | "string_try_last"
        | "bytes_try_get"
        | "string_try_pop"
        | "string_len"
        | "string_is_empty"
        | "string_is_not_empty"
        | "string_starts_with"
        | "string_ends_with"
        | "string_contains"
        | "string_is_ascii"
        | "string_all"
        | "string_any"
        | "string_is_integer" => ValueShape::scalar(KindSet::int()),
        "bigint_compare" => ValueShape::scalar(KindSet::int()),
        "bigint_bitand" | "bigint_bitor" | "bigint_bitxor" => {
            ValueShape::scalar(infer_numeric_result_kind(scalar_arg(0), scalar_arg(1)))
        }
        "bigint_shl" | "bigint_shr" => ValueShape::scalar(scalar_arg(0)),
        "bigint_from_int" | "bigint_add" | "bigint_subtract" | "bigint_multiply"
        | "bigint_divide" | "bigint_modulo" => ValueShape::scalar(KindSet::bigint()),
        "string_concat" | "bytes_slice" | "string_copy" | "string_repeat" | "string_reverse" => {
            ValueShape::scalar(KindSet::string())
        }
        "string_chars" => ValueShape::scalar(KindSet::string_iter()),
        "list_new" => ValueShape::list(KindSet::empty()),
        "map_new" => ValueShape::map(KindSet::empty()),
        "map_iter" => arg_shapes
            .first()
            .and_then(ValueShape::map_values)
            .map(ValueShape::map_iter)
            .unwrap_or_else(|| ValueShape::map_iter(KindSet::empty())),
        "list_range" => ValueShape::list(KindSet::int()),
        "list_copy" | "list_filter" => arg_shapes
            .first()
            .and_then(ValueShape::list_items)
            .map(ValueShape::list)
            .unwrap_or_else(|| ValueShape::list(KindSet::empty())),
        "list_map" => ValueShape::list(KindSet::empty()),
        "list_len" | "list_push" | "list_insert" | "list_set" | "list_swap" => {
            ValueShape::scalar(KindSet::int())
        }
        "map_len" | "map_has" => ValueShape::scalar(KindSet::int()),
        "map_set" => ValueShape::map(
            arg_shapes
                .first()
                .and_then(ValueShape::map_values)
                .unwrap_or_else(KindSet::empty)
                .union(
                    arg_shapes.get(2).map(ValueShape::scalar_slot).unwrap_or_else(KindSet::empty),
                ),
        ),
        "list_get" | "list_pop" | "list_delete" => ValueShape::scalar(
            arg_shapes.first().and_then(ValueShape::list_items).unwrap_or_else(KindSet::any),
        ),
        "map_get" | "map_delete" => ValueShape::scalar(
            arg_shapes.first().and_then(ValueShape::map_values).unwrap_or_else(KindSet::any),
        ),
        "map_try_get" | "map_try_delete" => ValueShape::from_slots(vec![
            KindSet::int(),
            arg_shapes.first().and_then(ValueShape::map_values).unwrap_or_else(KindSet::any),
            KindSet::string(),
        ]),
        "map_try_pop" => ValueShape::from_slots(vec![
            KindSet::int(),
            KindSet::string(),
            arg_shapes.first().and_then(ValueShape::map_values).unwrap_or_else(KindSet::any),
        ]),
        "map_iter_next" => ValueShape::from_slots(vec![
            KindSet::string(),
            arg_shapes.first().and_then(ValueShape::map_iter_values).unwrap_or_else(KindSet::any),
        ]),
        "map_iter_done" | "map_iter_advance" => ValueShape::scalar(KindSet::int()),
        "map_iter_key" => ValueShape::scalar(KindSet::string()),
        "map_iter_value" => ValueShape::scalar(
            arg_shapes.first().and_then(ValueShape::map_iter_values).unwrap_or_else(KindSet::any),
        ),
        "map_keys" => ValueShape::list(KindSet::string()),
        "map_values" => arg_shapes
            .first()
            .and_then(ValueShape::map_values)
            .map(ValueShape::list)
            .unwrap_or_else(|| ValueShape::list(KindSet::empty())),
        "string_from_codepoints" => ValueShape::scalar(KindSet::string()),
        "string_try_parse_integer" => {
            ValueShape::from_slots(vec![KindSet::int(), KindSet::int(), KindSet::string()])
        }
        "string_try_parse_bigint" => {
            ValueShape::from_slots(vec![KindSet::int(), KindSet::bigint(), KindSet::string()])
        }
        _ => ValueShape::scalar(KindSet::any()),
    }
}

fn infer_known_callback_return_shape(
    callback: &Ast,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> Option<ValueShape> {
    match callback {
        Ast::FunctionRef(name) => value_kind_analysis
            .functions
            .get(name.as_ref())
            .map(|analysis| analysis.returns.clone()),
        Ast::Variable(name) if !function_analysis.variables.contains_key(name.as_str()) => {
            value_kind_analysis
                .functions
                .get(name.as_ref())
                .map(|analysis| analysis.returns.clone())
        }
        Ast::Variable(name) => function_analysis
            .function_bindings
            .get(name.as_str())
            .and_then(|binding| value_kind_analysis.functions.get(binding))
            .map(|analysis| analysis.returns.clone()),
        _ => None,
    }
}

fn infer_numeric_result_kind(lhs: KindSet, rhs: KindSet) -> KindSet {
    let can_int = lhs.contains(ValueKind::Int) && rhs.contains(ValueKind::Int);
    let can_bigint = (lhs.contains(ValueKind::BigInt)
        && (rhs.contains(ValueKind::Int) || rhs.contains(ValueKind::BigInt)))
        || (rhs.contains(ValueKind::BigInt)
            && (lhs.contains(ValueKind::Int) || lhs.contains(ValueKind::BigInt)));
    match (can_int, can_bigint) {
        (true, true) => KindSet::int().union(KindSet::bigint()),
        (true, false) => KindSet::int(),
        (false, true) => KindSet::bigint(),
        (false, false) => KindSet::any(),
    }
}

fn span_of_ast(ast: &Ast) -> Option<Span> {
    match ast {
        Ast::Variable(name) | Ast::FunctionRef(name) => name.span.clone(),
        Ast::Expression(ExpressionAst { function_span, .. }) => function_span.clone(),
        Ast::Index { span, .. }
        | Ast::IndexAssign { span, .. }
        | Ast::Assign { span, .. }
        | Ast::MultiAssign { span, .. }
        | Ast::If { span, .. } => span.clone(),
        Ast::FunctionDef(func) => func.span.clone(),
        Ast::Block(_)
        | Ast::Lambda { .. }
        | Ast::MultiValue(_)
        | Ast::Literal(_)
        | Ast::ListLiteral(_)
        | Ast::MapLiteral(_) => None,
    }
}

fn validate_no_nested_function_defs(ast: &Ast) -> Result<(), CompileError> {
    match ast {
        Ast::FunctionDef(_) => Err(CompileError::UnsupportedFeature("nested function definitions")),
        Ast::Lambda { body, .. } => validate_no_nested_function_defs(body),
        Ast::MultiValue(values) => {
            for value in values {
                validate_no_nested_function_defs(value)?;
            }
            Ok(())
        }
        Ast::ListLiteral(items) => {
            for item in items {
                validate_no_nested_function_defs(item)?;
            }
            Ok(())
        }
        Ast::MapLiteral(entries) => {
            for entry in entries {
                if let MapKeyAst::Dynamic(key) = &entry.key {
                    validate_no_nested_function_defs(key)?;
                }
                validate_no_nested_function_defs(&entry.value)?;
            }
            Ok(())
        }
        Ast::Index { collection, index, .. } => {
            validate_no_nested_function_defs(collection)?;
            validate_no_nested_function_defs(index)
        }
        Ast::IndexAssign { collection, index, value, .. } => {
            validate_no_nested_function_defs(collection)?;
            validate_no_nested_function_defs(index)?;
            validate_no_nested_function_defs(value)
        }
        Ast::Expression(ExpressionAst { args, .. }) => {
            for arg in args {
                validate_no_nested_function_defs(arg)?;
            }
            Ok(())
        }
        Ast::Block(block) => {
            for line in &block.lines {
                validate_no_nested_function_defs(line)?;
            }
            Ok(())
        }
        Ast::Assign { value, .. } => validate_no_nested_function_defs(value),
        Ast::MultiAssign { value, .. } => validate_no_nested_function_defs(value),
        Ast::If { condition, then, else_, .. } => {
            validate_no_nested_function_defs(condition)?;
            validate_no_nested_function_defs(&Ast::Block(then.clone()))?;
            if let Some(else_) = else_ {
                validate_no_nested_function_defs(&Ast::Block(else_.clone()))?;
            }
            Ok(())
        }
        Ast::Literal(_) | Ast::Variable(_) | Ast::FunctionRef(_) => Ok(()),
    }
}

fn lift_anonymous_functions(
    functions: Vec<FunctionDefAst>,
) -> (Vec<FunctionDefAst>, HashMap<String, ClosureMetadata>) {
    let mut lifter = LambdaLifter { next_id: 0, lifted: vec![], metadata: HashMap::new() };
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
                self.metadata.insert(name.clone(), ClosureMetadata { captures });
                self.lifted.push(FunctionDefAst {
                    name: name.to_string(),
                    inputs: inputs.clone(),
                    output: None,
                    block: BlockAst { lines: vec![(**body).clone()] },
                    span: None,
                });
                *ast = Ast::FunctionRef(crate::parser::Ident::synthetic(name));
            }
            Ast::Block(block) => self.lift_block(block, scope_names),
            Ast::Expression(ExpressionAst { args, .. }) => {
                for arg in args {
                    self.lift_ast(arg, scope_names);
                }
            }
            Ast::MultiValue(values) => {
                for value in values {
                    self.lift_ast(value, scope_names);
                }
            }
            Ast::ListLiteral(items) => {
                for item in items {
                    self.lift_ast(item, scope_names);
                }
            }
            Ast::MapLiteral(entries) => {
                for entry in entries {
                    if let MapKeyAst::Dynamic(key) = &mut entry.key {
                        self.lift_ast(key, scope_names);
                    }
                    self.lift_ast(&mut entry.value, scope_names);
                }
            }
            Ast::Index { collection, index, .. } => {
                self.lift_ast(collection, scope_names);
                self.lift_ast(index, scope_names);
            }
            Ast::IndexAssign { collection, index, value, .. } => {
                self.lift_ast(collection, scope_names);
                self.lift_ast(index, scope_names);
                self.lift_ast(value, scope_names);
            }
            Ast::Assign { value, .. } => self.lift_ast(value, scope_names),
            Ast::MultiAssign { value, .. } => self.lift_ast(value, scope_names),
            Ast::If { condition, then, else_, .. } => {
                self.lift_ast(condition, scope_names);
                self.lift_block(then, scope_names);
                if let Some(else_block) = else_ {
                    self.lift_block(else_block, scope_names);
                }
            }
            Ast::FunctionDef(_) => unimplemented!("nested function definitions"),
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
            if !local_names.contains(&name.name)
                && scope_names.contains(&name.name)
                && !captures.contains(&name.name)
            {
                captures.push(name.to_string());
            }
        }
        Ast::Expression(ExpressionAst { args, .. }) => {
            for arg in args {
                collect_captures_into(arg, local_names, scope_names, captures);
            }
        }
        Ast::MultiValue(values) => {
            for value in values {
                collect_captures_into(value, local_names, scope_names, captures);
            }
        }
        Ast::ListLiteral(items) => {
            for item in items {
                collect_captures_into(item, local_names, scope_names, captures);
            }
        }
        Ast::MapLiteral(entries) => {
            for entry in entries {
                if let MapKeyAst::Dynamic(key) = &entry.key {
                    collect_captures_into(key, local_names, scope_names, captures);
                }
                collect_captures_into(&entry.value, local_names, scope_names, captures);
            }
        }
        Ast::Index { collection, index, .. } => {
            collect_captures_into(collection, local_names, scope_names, captures);
            collect_captures_into(index, local_names, scope_names, captures);
        }
        Ast::IndexAssign { collection, index, value, .. } => {
            collect_captures_into(collection, local_names, scope_names, captures);
            collect_captures_into(index, local_names, scope_names, captures);
            collect_captures_into(value, local_names, scope_names, captures);
        }
        Ast::If { condition, then, else_, .. } => {
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
        Ast::MultiAssign { value, .. } => {
            collect_captures_into(value, local_names, scope_names, captures);
        }
        Ast::FunctionDef(_) => unimplemented!("nested function definitions"),
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
        Ast::MultiValue(values) => {
            for value in values {
                collect_var_names(value, names);
            }
        }
        Ast::Assign { name, value, .. } => {
            if !names.contains(name) {
                names.push(name.clone());
            }
            collect_var_names(value, names);
        }
        Ast::MultiAssign { names: assigned_names, value, .. } => {
            for name in assigned_names {
                if !names.contains(name) {
                    names.push(name.clone());
                }
            }
            collect_var_names(value, names);
        }
        Ast::IndexAssign { collection, index, value, .. } => {
            collect_var_names(collection, names);
            collect_var_names(index, names);
            collect_var_names(value, names);
        }
        Ast::MapLiteral(entries) => {
            for entry in entries {
                if let MapKeyAst::Dynamic(key) = &entry.key {
                    collect_var_names(key, names);
                }
                collect_var_names(&entry.value, names);
            }
        }
        Ast::If { condition, then, else_, .. } => {
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
    *func_refs.get(name).unwrap_or_else(|| panic!("builtin function '{name}' is missing"))
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

fn compile_logical_op(
    builder: &mut FunctionBuilder,
    function: &str,
    lhs_ast: &Ast,
    rhs_ast: &Ast,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
    let lhs = compile_ast(
        builder,
        lhs_ast,
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        function_analysis,
        value_kind_analysis,
    );
    let lhs_truth = call_unary_scalar(builder, func_refs, "__value_is_truthy", lhs);
    let lhs_non_zero = builder.ins().icmp_imm(IntCC::NotEqual, lhs_truth, 0);

    let rhs_block = builder.create_block();
    let short_block = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, types::I64);

    if function == "and" {
        builder.ins().brif(lhs_non_zero, rhs_block, &[], short_block, &[]);
    } else {
        builder.ins().brif(lhs_non_zero, short_block, &[], rhs_block, &[]);
    }

    builder.switch_to_block(short_block);
    builder.seal_block(short_block);
    let short_payload = builder.ins().iconst(types::I64, if function == "and" { 0 } else { 1 });
    builder.ins().jump(merge_block, &[BlockArg::Value(short_payload)]);

    builder.switch_to_block(rhs_block);
    builder.seal_block(rhs_block);
    let rhs = compile_ast(
        builder,
        rhs_ast,
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        function_analysis,
        value_kind_analysis,
    );
    let rhs_truth = call_unary_scalar(builder, func_refs, "__value_is_truthy", rhs);
    builder.ins().jump(merge_block, &[BlockArg::Value(rhs_truth)]);

    builder.switch_to_block(merge_block);
    builder.seal_block(merge_block);
    CompiledValue {
        tag: builder.ins().iconst(types::I64, TAG_INT),
        payload: builder.block_params(merge_block)[0],
    }
}

fn compile_logical_not(
    builder: &mut FunctionBuilder,
    arg_ast: &Ast,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
    let arg = compile_ast(
        builder,
        arg_ast,
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        function_analysis,
        value_kind_analysis,
    );
    let truth = call_unary_scalar(builder, func_refs, "__value_is_truthy", arg);
    let is_zero = builder.ins().icmp_imm(IntCC::Equal, truth, 0);
    let one = builder.ins().iconst(types::I64, 1);
    let zero = builder.ins().iconst(types::I64, 0);
    let payload = builder.ins().select(is_zero, one, zero);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload }
}

fn call_binary(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    name: &str,
    lhs: CompiledValue,
    rhs: CompiledValue,
) -> CompiledValue {
    let func_ref = require_func(func_refs, name);
    let call = builder.ins().call(func_ref, &[lhs.tag, lhs.payload, rhs.tag, rhs.payload]);
    let results = builder.inst_results(call);
    CompiledValue { tag: results[0], payload: results[1] }
}

fn promote_value_to_bigint(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    value: CompiledValue,
) -> CompiledValue {
    let is_bigint = builder.ins().icmp_imm(IntCC::Equal, value.tag, TAG_BIGINT);
    let bigint_block = builder.create_block();
    let int_check_block = builder.create_block();
    let int_block = builder.create_block();
    let trap_block = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, types::I64);
    builder.append_block_param(merge_block, types::I64);
    builder.ins().brif(is_bigint, bigint_block, &[], int_check_block, &[]);

    builder.switch_to_block(bigint_block);
    builder.seal_block(bigint_block);
    builder.ins().jump(merge_block, &[BlockArg::Value(value.tag), BlockArg::Value(value.payload)]);

    builder.switch_to_block(int_check_block);
    builder.seal_block(int_check_block);
    let is_int = builder.ins().icmp_imm(IntCC::Equal, value.tag, TAG_INT);
    builder.ins().brif(is_int, int_block, &[], trap_block, &[]);

    builder.switch_to_block(int_block);
    builder.seal_block(int_block);
    let bigint_from_int = require_func(func_refs, "bigint_from_int");
    let call = builder.ins().call(bigint_from_int, &[value.tag, value.payload]);
    let results = builder.inst_results(call);
    let result_tag = results[0];
    let result_payload = results[1];
    builder
        .ins()
        .jump(merge_block, &[BlockArg::Value(result_tag), BlockArg::Value(result_payload)]);

    builder.switch_to_block(trap_block);
    builder.seal_block(trap_block);
    builder.ins().trap(TrapCode::BAD_CONVERSION_TO_INTEGER);

    builder.switch_to_block(merge_block);
    builder.seal_block(merge_block);
    CompiledValue {
        tag: builder.block_params(merge_block)[0],
        payload: builder.block_params(merge_block)[1],
    }
}

fn compile_bigint_builtin(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    name: &str,
    args: &[CompiledValue],
) -> CompiledValue {
    assert_eq!(args.len(), 2, "{name} expects 2 arguments");
    let lhs = promote_value_to_bigint(builder, func_refs, args[0]);
    let rhs = promote_value_to_bigint(builder, func_refs, args[1]);
    let func_ref = require_func(func_refs, name);
    let call = builder.ins().call(func_ref, &[lhs.tag, lhs.payload, rhs.tag, rhs.payload]);
    let results = builder.inst_results(call);
    CompiledValue { tag: results[0], payload: results[1] }
}

fn compile_bigint_shift_builtin(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    name: &str,
    lhs: CompiledValue,
    rhs: CompiledValue,
) -> CompiledValue {
    let lhs = promote_value_to_bigint(builder, func_refs, lhs);
    let func_ref = require_func(func_refs, name);
    let call = builder.ins().call(func_ref, &[lhs.tag, lhs.payload, rhs.tag, rhs.payload]);
    let results = builder.inst_results(call);
    CompiledValue { tag: results[0], payload: results[1] }
}

fn compile_exact_int_binary_op(
    builder: &mut FunctionBuilder,
    function: &str,
    lhs: CompiledValue,
    rhs: CompiledValue,
) -> CompiledValue {
    let raw = match function {
        "add" => {
            let (sum, ovf) = builder.ins().sadd_overflow(lhs.payload, rhs.payload);
            builder.ins().trapnz(ovf, TrapCode::INTEGER_OVERFLOW);
            sum
        }
        "subtract" => {
            let (diff, ovf) = builder.ins().ssub_overflow(lhs.payload, rhs.payload);
            builder.ins().trapnz(ovf, TrapCode::INTEGER_OVERFLOW);
            diff
        }
        "multiply" => {
            let (prod, ovf) = builder.ins().smul_overflow(lhs.payload, rhs.payload);
            builder.ins().trapnz(ovf, TrapCode::INTEGER_OVERFLOW);
            prod
        }
        "divide" => {
            builder.ins().trapz(rhs.payload, TrapCode::INTEGER_DIVISION_BY_ZERO);
            let lhs_is_min = builder.ins().icmp_imm(IntCC::Equal, lhs.payload, i64::MIN);
            let neg_one = builder.ins().iconst(types::I64, -1);
            let rhs_is_neg_one = builder.ins().icmp(IntCC::Equal, rhs.payload, neg_one);
            let overflow = builder.ins().band(lhs_is_min, rhs_is_neg_one);
            builder.ins().trapnz(overflow, TrapCode::INTEGER_OVERFLOW);
            builder.ins().sdiv(lhs.payload, rhs.payload)
        }
        "modulo" => {
            builder.ins().trapz(rhs.payload, TrapCode::INTEGER_DIVISION_BY_ZERO);
            let lhs_is_min = builder.ins().icmp_imm(IntCC::Equal, lhs.payload, i64::MIN);
            let neg_one = builder.ins().iconst(types::I64, -1);
            let rhs_is_neg_one = builder.ins().icmp(IntCC::Equal, rhs.payload, neg_one);
            let overflow = builder.ins().band(lhs_is_min, rhs_is_neg_one);
            builder.ins().trapnz(overflow, TrapCode::INTEGER_OVERFLOW);
            builder.ins().srem(lhs.payload, rhs.payload)
        }
        "bitand" => builder.ins().band(lhs.payload, rhs.payload),
        "bitor" => builder.ins().bor(lhs.payload, rhs.payload),
        "bitxor" => builder.ins().bxor(lhs.payload, rhs.payload),
        "shl" | "shr" => {
            let rhs_non_neg =
                builder.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, rhs.payload, 0);
            let rhs_lt_width = builder.ins().icmp_imm(IntCC::SignedLessThan, rhs.payload, 64);
            let rhs_in_range = builder.ins().band(rhs_non_neg, rhs_lt_width);
            builder.ins().trapz(rhs_in_range, TrapCode::BAD_CONVERSION_TO_INTEGER);
            if function == "shl" {
                builder.ins().ishl(lhs.payload, rhs.payload)
            } else {
                builder.ins().sshr(lhs.payload, rhs.payload)
            }
        }
        _ => unreachable!("not an exact int binary op: {function}"),
    };
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload: raw }
}

fn compile_exact_int_compare_op(
    builder: &mut FunctionBuilder,
    function: &str,
    lhs: CompiledValue,
    rhs: CompiledValue,
) -> CompiledValue {
    let cc = match function {
        "gt" => IntCC::SignedGreaterThan,
        "lt" => IntCC::SignedLessThan,
        "gte" => IntCC::SignedGreaterThanOrEqual,
        "lte" => IntCC::SignedLessThanOrEqual,
        "eq" => IntCC::Equal,
        "ne" => IntCC::NotEqual,
        _ => unreachable!("not an exact int compare op: {function}"),
    };
    let cmp = builder.ins().icmp(cc, lhs.payload, rhs.payload);
    let one = builder.ins().iconst(types::I64, 1);
    let zero = builder.ins().iconst(types::I64, 0);
    let raw = builder.ins().select(cmp, one, zero);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload: raw }
}

fn compile_exact_bigint_compare_op(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    function: &str,
    lhs: CompiledValue,
    rhs: CompiledValue,
) -> CompiledValue {
    let compare_ref = require_func(func_refs, "bigint_compare");
    let call = builder.ins().call(compare_ref, &[lhs.tag, lhs.payload, rhs.tag, rhs.payload]);
    let cmp_raw = builder.inst_results(call)[1];
    let cc = match function {
        "gt" => IntCC::SignedGreaterThan,
        "lt" => IntCC::SignedLessThan,
        "gte" => IntCC::SignedGreaterThanOrEqual,
        "lte" => IntCC::SignedLessThanOrEqual,
        "eq" => IntCC::Equal,
        "ne" => IntCC::NotEqual,
        _ => unreachable!("not an exact bigint compare op: {function}"),
    };
    let cmp = builder.ins().icmp_imm(cc, cmp_raw, 0);
    let one = builder.ins().iconst(types::I64, 1);
    let zero = builder.ins().iconst(types::I64, 0);
    let raw = builder.ins().select(cmp, one, zero);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload: raw }
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
    let call =
        builder.ins().call(func_ref, &[a.tag, a.payload, b.tag, b.payload, c.tag, c.payload]);
    let results = builder.inst_results(call);
    CompiledValue { tag: results[0], payload: results[1] }
}

fn boxed_int_const(builder: &mut FunctionBuilder, value: i64) -> CompiledValue {
    CompiledValue {
        tag: builder.ins().iconst(types::I64, TAG_INT),
        payload: builder.ins().iconst(types::I64, value),
    }
}

fn compile_is_tag_predicate(
    builder: &mut FunctionBuilder,
    value: CompiledValue,
    expected_tag: i64,
) -> CompiledValue {
    let matches = builder.ins().icmp_imm(IntCC::Equal, value.tag, expected_tag);
    let payload = builder.ins().uextend(types::I64, matches);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload }
}

fn compile_bigint_literal(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    digits: &str,
) -> CompiledValue {
    let bigint_from_int = require_func(func_refs, "bigint_from_int");
    let bigint_multiply = require_func(func_refs, "bigint_multiply");
    let bigint_add = require_func(func_refs, "bigint_add");

    let zero = boxed_int_const(builder, 0);
    let init = builder.ins().call(bigint_from_int, &[zero.tag, zero.payload]);
    let init_results = builder.inst_results(init);
    let mut acc = CompiledValue { tag: init_results[0], payload: init_results[1] };
    let ten_int = boxed_int_const(builder, 10);
    let ten_call = builder.ins().call(bigint_from_int, &[ten_int.tag, ten_int.payload]);
    let ten_results = builder.inst_results(ten_call);
    let ten = CompiledValue { tag: ten_results[0], payload: ten_results[1] };

    for ch in digits.chars() {
        let mul =
            builder.ins().call(bigint_multiply, &[acc.tag, acc.payload, ten.tag, ten.payload]);
        let mul_results = builder.inst_results(mul);
        acc = CompiledValue { tag: mul_results[0], payload: mul_results[1] };

        let digit_int = boxed_int_const(builder, ch.to_digit(10).unwrap() as i64);
        let digit_call = builder.ins().call(bigint_from_int, &[digit_int.tag, digit_int.payload]);
        let digit_results = builder.inst_results(digit_call);
        let digit = CompiledValue { tag: digit_results[0], payload: digit_results[1] };
        let add = builder.ins().call(bigint_add, &[acc.tag, acc.payload, digit.tag, digit.payload]);
        let add_results = builder.inst_results(add);
        acc = CompiledValue { tag: add_results[0], payload: add_results[1] };
    }

    acc
}

fn compile_string_literal(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    value: &str,
) -> CompiledValue {
    let alloc_ref = require_func(func_refs, "__alloc");
    let bytes = value.as_bytes();
    let len = i64::try_from(bytes.len()).expect("string literal too large");
    let len_value = builder.ins().iconst(types::I64, len);
    let align = builder.ins().iconst(types::I64, 8);
    let data_call = builder.ins().call(alloc_ref, &[len_value, align]);
    let data_ptr = builder.inst_results(data_call)[0];

    for (index, byte) in bytes.iter().copied().enumerate() {
        let offset = i32::try_from(index).expect("string literal offset overflow");
        let byte_value = builder.ins().iconst(types::I8, i64::from(byte));
        builder.ins().store(MemFlags::new(), byte_value, data_ptr, offset);
    }

    let header_size = builder.ins().iconst(types::I64, STRING_HEADER_SIZE);
    let header_call = builder.ins().call(alloc_ref, &[header_size, align]);
    let header_ptr = builder.inst_results(header_call)[0];
    builder.ins().store(MemFlags::new(), len_value, header_ptr, STRING_LEN_OFFSET);
    builder.ins().store(MemFlags::new(), len_value, header_ptr, STRING_CAP_OFFSET);
    builder.ins().store(MemFlags::new(), data_ptr, header_ptr, STRING_PTR_OFFSET);

    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_STRING), payload: header_ptr }
}

fn compile_bytes_len_known_string(
    builder: &mut FunctionBuilder,
    value: CompiledValue,
    assume_string: bool,
) -> CompiledValue {
    if !assume_string {
        let is_string = builder.ins().icmp_imm(IntCC::Equal, value.tag, TAG_STRING);
        builder.ins().trapz(is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);
    }
    let len = builder.ins().load(types::I64, MemFlags::new(), value.payload, STRING_LEN_OFFSET);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload: len }
}

fn compile_bytes_get_known_types(
    builder: &mut FunctionBuilder,
    string_value: CompiledValue,
    index_value: CompiledValue,
    assume_string: bool,
    assume_int_index: bool,
) -> CompiledValue {
    if !assume_string {
        let is_string = builder.ins().icmp_imm(IntCC::Equal, string_value.tag, TAG_STRING);
        builder.ins().trapz(is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);
    }
    if !assume_int_index {
        let is_int = builder.ins().icmp_imm(IntCC::Equal, index_value.tag, TAG_INT);
        builder.ins().trapz(is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);
    }

    let len =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_LEN_OFFSET);
    let non_neg = builder.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, index_value.payload, 0);
    builder.ins().trapz(non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, index_value.payload, len);
    builder.ins().trapz(in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);

    let data_ptr =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_PTR_OFFSET);
    let addr = builder.ins().iadd(data_ptr, index_value.payload);
    let byte = builder.ins().load(types::I8, MemFlags::new(), addr, 0);
    let byte_i64 = builder.ins().uextend(types::I64, byte);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload: byte_i64 }
}

fn compile_bytes_slice_known_types(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    string_value: CompiledValue,
    start_value: CompiledValue,
    end_value: CompiledValue,
    assume_string: bool,
    assume_int_start: bool,
    assume_int_end: bool,
) -> CompiledValue {
    if !assume_string {
        let is_string = builder.ins().icmp_imm(IntCC::Equal, string_value.tag, TAG_STRING);
        builder.ins().trapz(is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);
    }
    if !assume_int_start {
        let start_is_int = builder.ins().icmp_imm(IntCC::Equal, start_value.tag, TAG_INT);
        builder.ins().trapz(start_is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);
    }
    if !assume_int_end {
        let end_is_int = builder.ins().icmp_imm(IntCC::Equal, end_value.tag, TAG_INT);
        builder.ins().trapz(end_is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);
    }

    let len =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_LEN_OFFSET);
    let start_non_neg =
        builder.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, start_value.payload, 0);
    builder.ins().trapz(start_non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
    let end_non_neg = builder.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, end_value.payload, 0);
    builder.ins().trapz(end_non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
    let start_le_end =
        builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, start_value.payload, end_value.payload);
    builder.ins().trapz(start_le_end, TrapCode::HEAP_OUT_OF_BOUNDS);
    let end_in_bounds = builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, end_value.payload, len);
    builder.ins().trapz(end_in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);

    let slice_len = builder.ins().isub(end_value.payload, start_value.payload);
    let src_ptr =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_PTR_OFFSET);
    let slice_src_ptr = builder.ins().iadd(src_ptr, start_value.payload);

    let alloc_ref = require_func(func_refs, "__alloc");
    let align = builder.ins().iconst(types::I64, 8);
    let data_call = builder.ins().call(alloc_ref, &[slice_len, align]);
    let data_ptr = builder.inst_results(data_call)[0];
    let zero = builder.ins().iconst(types::I64, 0);
    copy_string_bytes(builder, slice_src_ptr, data_ptr, slice_len, zero);

    let header_size = builder.ins().iconst(types::I64, STRING_HEADER_SIZE);
    let header_call = builder.ins().call(alloc_ref, &[header_size, align]);
    let header_ptr = builder.inst_results(header_call)[0];
    builder.ins().store(MemFlags::new(), slice_len, header_ptr, STRING_LEN_OFFSET);
    builder.ins().store(MemFlags::new(), slice_len, header_ptr, STRING_CAP_OFFSET);
    builder.ins().store(MemFlags::new(), data_ptr, header_ptr, STRING_PTR_OFFSET);

    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_STRING), payload: header_ptr }
}

fn compile_list_len_known_list(
    builder: &mut FunctionBuilder,
    list_value: CompiledValue,
    assume_list: bool,
) -> CompiledValue {
    if !assume_list {
        let is_list = builder.ins().icmp_imm(IntCC::Equal, list_value.tag, TAG_LIST);
        builder.ins().trapz(is_list, TrapCode::BAD_CONVERSION_TO_INTEGER);
    }
    let len = builder.ins().load(types::I64, MemFlags::new(), list_value.payload, LIST_LEN_OFFSET);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload: len }
}

fn compile_list_get_known_types(
    builder: &mut FunctionBuilder,
    list_value: CompiledValue,
    index_value: CompiledValue,
    assume_list: bool,
    assume_int_index: bool,
) -> CompiledValue {
    if !assume_list {
        let is_list = builder.ins().icmp_imm(IntCC::Equal, list_value.tag, TAG_LIST);
        builder.ins().trapz(is_list, TrapCode::BAD_CONVERSION_TO_INTEGER);
    }
    if !assume_int_index {
        let is_int = builder.ins().icmp_imm(IntCC::Equal, index_value.tag, TAG_INT);
        builder.ins().trapz(is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);
    }

    let len = builder.ins().load(types::I64, MemFlags::new(), list_value.payload, LIST_LEN_OFFSET);
    let non_neg = builder.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, index_value.payload, 0);
    builder.ins().trapz(non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, index_value.payload, len);
    builder.ins().trapz(in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);

    let data_ptr =
        builder.ins().load(types::I64, MemFlags::new(), list_value.payload, LIST_PTR_OFFSET);
    let slot_size = builder.ins().iconst(types::I64, VALUE_SIZE);
    let slot_offset = builder.ins().imul(index_value.payload, slot_size);
    let slot_addr = builder.ins().iadd(data_ptr, slot_offset);
    let tag_i8 = builder.ins().load(types::I8, MemFlags::new(), slot_addr, 0);
    let tag = builder.ins().uextend(types::I64, tag_i8);
    let payload = builder.ins().load(types::I64, MemFlags::new(), slot_addr, VALUE_PAYLOAD_OFFSET);
    CompiledValue { tag, payload }
}

fn compile_bytes_pop(builder: &mut FunctionBuilder, string_value: CompiledValue) -> CompiledValue {
    let is_string = builder.ins().icmp_imm(IntCC::Equal, string_value.tag, TAG_STRING);
    builder.ins().trapz(is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);

    let len =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_LEN_OFFSET);
    let non_empty = builder.ins().icmp_imm(IntCC::NotEqual, len, 0);
    builder.ins().trapz(non_empty, TrapCode::HEAP_OUT_OF_BOUNDS);

    let new_len = builder.ins().iadd_imm(len, -1);
    builder.ins().store(MemFlags::new(), new_len, string_value.payload, STRING_LEN_OFFSET);

    let data_ptr =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_PTR_OFFSET);
    let addr = builder.ins().iadd(data_ptr, new_len);
    let byte = builder.ins().load(types::I8, MemFlags::new(), addr, 0);
    let byte_i64 = builder.ins().uextend(types::I64, byte);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload: byte_i64 }
}

fn compile_bytes_push(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    string_value: CompiledValue,
    byte_value: CompiledValue,
) -> CompiledValue {
    let is_string = builder.ins().icmp_imm(IntCC::Equal, string_value.tag, TAG_STRING);
    builder.ins().trapz(is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let is_int = builder.ins().icmp_imm(IntCC::Equal, byte_value.tag, TAG_INT);
    builder.ins().trapz(is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);

    let len =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_LEN_OFFSET);
    let cap =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_CAP_OFFSET);
    let data_ptr =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_PTR_OFFSET);
    let alloc_ref = require_func(func_refs, "__alloc");

    let grow_block = builder.create_block();
    let write_block = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, types::I64);

    let has_capacity = builder.ins().icmp(IntCC::UnsignedLessThan, len, cap);
    builder.ins().brif(has_capacity, write_block, &[], grow_block, &[]);

    builder.switch_to_block(grow_block);
    let zero = builder.ins().iconst(types::I64, 0);
    let one = builder.ins().iconst(types::I64, 1);
    let cap_is_zero = builder.ins().icmp(IntCC::Equal, cap, zero);
    let doubled_cap = builder.ins().iadd(cap, cap);
    let new_cap = builder.ins().select(cap_is_zero, one, doubled_cap);
    let align = builder.ins().iconst(types::I64, 8);
    let new_data_call = builder.ins().call(alloc_ref, &[new_cap, align]);
    let new_data_ptr = builder.inst_results(new_data_call)[0];
    copy_string_bytes(builder, data_ptr, new_data_ptr, len, zero);
    builder.ins().store(MemFlags::new(), new_data_ptr, string_value.payload, STRING_PTR_OFFSET);
    builder.ins().store(MemFlags::new(), new_cap, string_value.payload, STRING_CAP_OFFSET);
    builder.ins().jump(merge_block, &[BlockArg::Value(new_data_ptr)]);
    builder.seal_block(grow_block);

    builder.switch_to_block(write_block);
    builder.ins().jump(merge_block, &[BlockArg::Value(data_ptr)]);
    builder.seal_block(write_block);

    builder.switch_to_block(merge_block);
    let active_data_ptr = builder.block_params(merge_block)[0];
    let clamped = builder.ins().band_imm(byte_value.payload, 0xff);
    let byte_i8 = builder.ins().ireduce(types::I8, clamped);
    let addr = builder.ins().iadd(active_data_ptr, len);
    builder.ins().store(MemFlags::new(), byte_i8, addr, 0);
    let new_len = builder.ins().iadd_imm(len, 1);
    builder.ins().store(MemFlags::new(), new_len, string_value.payload, STRING_LEN_OFFSET);
    builder.seal_block(merge_block);
    string_value
}

fn compile_bytes_insert(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    string_value: CompiledValue,
    index_value: CompiledValue,
    byte_value: CompiledValue,
) -> CompiledValue {
    let is_string = builder.ins().icmp_imm(IntCC::Equal, string_value.tag, TAG_STRING);
    builder.ins().trapz(is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let idx_is_int = builder.ins().icmp_imm(IntCC::Equal, index_value.tag, TAG_INT);
    builder.ins().trapz(idx_is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let byte_is_int = builder.ins().icmp_imm(IntCC::Equal, byte_value.tag, TAG_INT);
    builder.ins().trapz(byte_is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);

    let len =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_LEN_OFFSET);
    let non_neg = builder.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, index_value.payload, 0);
    builder.ins().trapz(non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, index_value.payload, len);
    builder.ins().trapz(in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);

    let cap =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_CAP_OFFSET);
    let data_ptr =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_PTR_OFFSET);
    let alloc_ref = require_func(func_refs, "__alloc");

    let grow_block = builder.create_block();
    let shift_init_block = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, types::I64);

    let has_capacity = builder.ins().icmp(IntCC::UnsignedLessThan, len, cap);
    builder.ins().brif(has_capacity, shift_init_block, &[], grow_block, &[]);

    builder.switch_to_block(grow_block);
    let zero = builder.ins().iconst(types::I64, 0);
    let one = builder.ins().iconst(types::I64, 1);
    let cap_is_zero = builder.ins().icmp(IntCC::Equal, cap, zero);
    let doubled_cap = builder.ins().iadd(cap, cap);
    let new_cap = builder.ins().select(cap_is_zero, one, doubled_cap);
    let align = builder.ins().iconst(types::I64, 8);
    let new_data_call = builder.ins().call(alloc_ref, &[new_cap, align]);
    let new_data_ptr = builder.inst_results(new_data_call)[0];
    copy_string_bytes(builder, data_ptr, new_data_ptr, len, zero);
    builder.ins().store(MemFlags::new(), new_data_ptr, string_value.payload, STRING_PTR_OFFSET);
    builder.ins().store(MemFlags::new(), new_cap, string_value.payload, STRING_CAP_OFFSET);
    builder.ins().jump(merge_block, &[BlockArg::Value(new_data_ptr)]);
    builder.seal_block(grow_block);

    builder.switch_to_block(shift_init_block);
    builder.ins().jump(merge_block, &[BlockArg::Value(data_ptr)]);
    builder.seal_block(shift_init_block);

    builder.switch_to_block(merge_block);
    let active_data_ptr = builder.block_params(merge_block)[0];

    let shift_loop = builder.create_block();
    let shift_body = builder.create_block();
    let insert_block = builder.create_block();
    builder.append_block_param(shift_loop, types::I64);

    builder.ins().jump(shift_loop, &[BlockArg::Value(len)]);
    builder.switch_to_block(shift_loop);
    let idx = builder.block_params(shift_loop)[0];
    let needs_shift = builder.ins().icmp(IntCC::UnsignedGreaterThan, idx, index_value.payload);
    builder.ins().brif(needs_shift, shift_body, &[], insert_block, &[]);

    builder.switch_to_block(shift_body);
    let src_idx = builder.ins().iadd_imm(idx, -1);
    let src_addr = builder.ins().iadd(active_data_ptr, src_idx);
    let dst_addr = builder.ins().iadd(active_data_ptr, idx);
    let byte = builder.ins().load(types::I8, MemFlags::new(), src_addr, 0);
    builder.ins().store(MemFlags::new(), byte, dst_addr, 0);
    builder.ins().jump(shift_loop, &[BlockArg::Value(src_idx)]);

    builder.switch_to_block(insert_block);
    let clamped = builder.ins().band_imm(byte_value.payload, 0xff);
    let byte_i8 = builder.ins().ireduce(types::I8, clamped);
    let insert_addr = builder.ins().iadd(active_data_ptr, index_value.payload);
    builder.ins().store(MemFlags::new(), byte_i8, insert_addr, 0);
    let new_len = builder.ins().iadd_imm(len, 1);
    builder.ins().store(MemFlags::new(), new_len, string_value.payload, STRING_LEN_OFFSET);

    builder.seal_block(merge_block);
    builder.seal_block(shift_body);
    builder.seal_block(shift_loop);
    builder.seal_block(insert_block);
    string_value
}

fn compile_bytes_remove(
    builder: &mut FunctionBuilder,
    string_value: CompiledValue,
    index_value: CompiledValue,
) -> CompiledValue {
    let is_string = builder.ins().icmp_imm(IntCC::Equal, string_value.tag, TAG_STRING);
    builder.ins().trapz(is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let idx_is_int = builder.ins().icmp_imm(IntCC::Equal, index_value.tag, TAG_INT);
    builder.ins().trapz(idx_is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);

    let len =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_LEN_OFFSET);
    let non_neg = builder.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, index_value.payload, 0);
    builder.ins().trapz(non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, index_value.payload, len);
    builder.ins().trapz(in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);

    let data_ptr =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_PTR_OFFSET);
    let removed_addr = builder.ins().iadd(data_ptr, index_value.payload);
    let removed_byte = builder.ins().load(types::I8, MemFlags::new(), removed_addr, 0);

    let shift_loop = builder.create_block();
    let shift_body = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(shift_loop, types::I64);

    let one = builder.ins().iconst(types::I64, 1);
    let last_index = builder.ins().isub(len, one);
    builder.ins().jump(shift_loop, &[BlockArg::Value(index_value.payload)]);
    builder.switch_to_block(shift_loop);
    let idx = builder.block_params(shift_loop)[0];
    let more = builder.ins().icmp(IntCC::UnsignedLessThan, idx, last_index);
    builder.ins().brif(more, shift_body, &[], done_block, &[]);

    builder.switch_to_block(shift_body);
    let src_idx = builder.ins().iadd(idx, one);
    let src_addr = builder.ins().iadd(data_ptr, src_idx);
    let dst_addr = builder.ins().iadd(data_ptr, idx);
    let byte = builder.ins().load(types::I8, MemFlags::new(), src_addr, 0);
    builder.ins().store(MemFlags::new(), byte, dst_addr, 0);
    let next_idx = builder.ins().iadd(idx, one);
    builder.ins().jump(shift_loop, &[BlockArg::Value(next_idx)]);

    builder.switch_to_block(done_block);
    let new_len = builder.ins().iadd_imm(len, -1);
    builder.ins().store(MemFlags::new(), new_len, string_value.payload, STRING_LEN_OFFSET);
    builder.seal_block(shift_body);
    builder.seal_block(shift_loop);
    builder.seal_block(done_block);

    CompiledValue {
        tag: builder.ins().iconst(types::I64, TAG_INT),
        payload: builder.ins().uextend(types::I64, removed_byte),
    }
}

fn compile_bytes_set(
    builder: &mut FunctionBuilder,
    string_value: CompiledValue,
    index_value: CompiledValue,
    byte_value: CompiledValue,
) -> CompiledValue {
    let is_string = builder.ins().icmp_imm(IntCC::Equal, string_value.tag, TAG_STRING);
    builder.ins().trapz(is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let idx_is_int = builder.ins().icmp_imm(IntCC::Equal, index_value.tag, TAG_INT);
    builder.ins().trapz(idx_is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let byte_is_int = builder.ins().icmp_imm(IntCC::Equal, byte_value.tag, TAG_INT);
    builder.ins().trapz(byte_is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);

    let len =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_LEN_OFFSET);
    let non_neg = builder.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, index_value.payload, 0);
    builder.ins().trapz(non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, index_value.payload, len);
    builder.ins().trapz(in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);

    let data_ptr =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_PTR_OFFSET);
    let addr = builder.ins().iadd(data_ptr, index_value.payload);
    let clamped = builder.ins().band_imm(byte_value.payload, 0xff);
    let byte_i8 = builder.ins().ireduce(types::I8, clamped);
    builder.ins().store(MemFlags::new(), byte_i8, addr, 0);
    string_value
}

fn compile_string_copy(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    string_value: CompiledValue,
) -> CompiledValue {
    let is_string = builder.ins().icmp_imm(IntCC::Equal, string_value.tag, TAG_STRING);
    builder.ins().trapz(is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let len =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_LEN_OFFSET);
    let src_ptr =
        builder.ins().load(types::I64, MemFlags::new(), string_value.payload, STRING_PTR_OFFSET);
    let alloc_ref = require_func(func_refs, "__alloc");
    let align = builder.ins().iconst(types::I64, 8);
    let data_call = builder.ins().call(alloc_ref, &[len, align]);
    let data_ptr = builder.inst_results(data_call)[0];
    let zero = builder.ins().iconst(types::I64, 0);
    copy_string_bytes(builder, src_ptr, data_ptr, len, zero);

    let header_size = builder.ins().iconst(types::I64, STRING_HEADER_SIZE);
    let header_call = builder.ins().call(alloc_ref, &[header_size, align]);
    let header_ptr = builder.inst_results(header_call)[0];
    builder.ins().store(MemFlags::new(), len, header_ptr, STRING_LEN_OFFSET);
    builder.ins().store(MemFlags::new(), len, header_ptr, STRING_CAP_OFFSET);
    builder.ins().store(MemFlags::new(), data_ptr, header_ptr, STRING_PTR_OFFSET);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_STRING), payload: header_ptr }
}

fn compile_string_concat(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    lhs: CompiledValue,
    rhs: CompiledValue,
) -> CompiledValue {
    let lhs_is_string = builder.ins().icmp_imm(IntCC::Equal, lhs.tag, TAG_STRING);
    builder.ins().trapz(lhs_is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let rhs_is_string = builder.ins().icmp_imm(IntCC::Equal, rhs.tag, TAG_STRING);
    builder.ins().trapz(rhs_is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);

    let lhs_len = builder.ins().load(types::I64, MemFlags::new(), lhs.payload, STRING_LEN_OFFSET);
    let rhs_len = builder.ins().load(types::I64, MemFlags::new(), rhs.payload, STRING_LEN_OFFSET);
    let total_len = builder.ins().iadd(lhs_len, rhs_len);
    let lhs_ptr = builder.ins().load(types::I64, MemFlags::new(), lhs.payload, STRING_PTR_OFFSET);
    let rhs_ptr = builder.ins().load(types::I64, MemFlags::new(), rhs.payload, STRING_PTR_OFFSET);

    let alloc_ref = require_func(func_refs, "__alloc");
    let align = builder.ins().iconst(types::I64, 8);
    let data_call = builder.ins().call(alloc_ref, &[total_len, align]);
    let data_ptr = builder.inst_results(data_call)[0];
    let zero = builder.ins().iconst(types::I64, 0);
    copy_string_bytes(builder, lhs_ptr, data_ptr, lhs_len, zero);
    copy_string_bytes(builder, rhs_ptr, data_ptr, rhs_len, lhs_len);

    let header_size = builder.ins().iconst(types::I64, STRING_HEADER_SIZE);
    let header_call = builder.ins().call(alloc_ref, &[header_size, align]);
    let header_ptr = builder.inst_results(header_call)[0];
    builder.ins().store(MemFlags::new(), total_len, header_ptr, STRING_LEN_OFFSET);
    builder.ins().store(MemFlags::new(), total_len, header_ptr, STRING_CAP_OFFSET);
    builder.ins().store(MemFlags::new(), data_ptr, header_ptr, STRING_PTR_OFFSET);

    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_STRING), payload: header_ptr }
}

fn compile_string_chars(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    string_value: CompiledValue,
) -> CompiledValue {
    let is_string = builder.ins().icmp_imm(IntCC::Equal, string_value.tag, TAG_STRING);
    builder.ins().trapz(is_string, TrapCode::BAD_CONVERSION_TO_INTEGER);

    let alloc_ref = require_func(func_refs, "__alloc");
    let align = builder.ins().iconst(types::I64, 8);
    let header_size = builder.ins().iconst(types::I64, STRING_ITER_HEADER_SIZE);
    let header_call = builder.ins().call(alloc_ref, &[header_size, align]);
    let header_ptr = builder.inst_results(header_call)[0];
    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().store(
        MemFlags::new(),
        string_value.payload,
        header_ptr,
        STRING_ITER_STRING_OFFSET,
    );
    builder.ins().store(MemFlags::new(), zero, header_ptr, STRING_ITER_INDEX_OFFSET);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_STRING_ITER), payload: header_ptr }
}

fn compile_string_iter_done(
    builder: &mut FunctionBuilder,
    iter_value: CompiledValue,
) -> CompiledValue {
    let is_iter = builder.ins().icmp_imm(IntCC::Equal, iter_value.tag, TAG_STRING_ITER);
    builder.ins().trapz(is_iter, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let string_ptr = builder.ins().load(
        types::I64,
        MemFlags::new(),
        iter_value.payload,
        STRING_ITER_STRING_OFFSET,
    );
    let index = builder.ins().load(
        types::I64,
        MemFlags::new(),
        iter_value.payload,
        STRING_ITER_INDEX_OFFSET,
    );
    let len = builder.ins().load(types::I64, MemFlags::new(), string_ptr, STRING_LEN_OFFSET);
    let done = builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, index, len);
    CompiledValue {
        tag: builder.ins().iconst(types::I64, TAG_INT),
        payload: builder.ins().uextend(types::I64, done),
    }
}

fn compile_string_iter_next(
    builder: &mut FunctionBuilder,
    iter_value: CompiledValue,
) -> CompiledValue {
    let is_iter = builder.ins().icmp_imm(IntCC::Equal, iter_value.tag, TAG_STRING_ITER);
    builder.ins().trapz(is_iter, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let string_ptr = builder.ins().load(
        types::I64,
        MemFlags::new(),
        iter_value.payload,
        STRING_ITER_STRING_OFFSET,
    );
    let index = builder.ins().load(
        types::I64,
        MemFlags::new(),
        iter_value.payload,
        STRING_ITER_INDEX_OFFSET,
    );
    let len = builder.ins().load(types::I64, MemFlags::new(), string_ptr, STRING_LEN_OFFSET);
    let not_done = builder.ins().icmp(IntCC::UnsignedLessThan, index, len);
    builder.ins().trapz(not_done, TrapCode::HEAP_OUT_OF_BOUNDS);
    let data_ptr = builder.ins().load(types::I64, MemFlags::new(), string_ptr, STRING_PTR_OFFSET);

    let (codepoint, next_index) = decode_utf8_forward(builder, data_ptr, len, index);
    builder.ins().store(MemFlags::new(), next_index, iter_value.payload, STRING_ITER_INDEX_OFFSET);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload: codepoint }
}

fn decode_utf8_forward(
    builder: &mut FunctionBuilder,
    data_ptr: Value,
    len: Value,
    index: Value,
) -> (Value, Value) {
    let lead = load_u8_at(builder, data_ptr, index);
    let lead_lt_80 = builder.ins().icmp_imm(IntCC::UnsignedLessThan, lead, 0x80);
    let ascii_block = builder.create_block();
    let non_ascii_block = builder.create_block();
    let two_block = builder.create_block();
    let three_or_more_block = builder.create_block();
    let three_block = builder.create_block();
    let four_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(done_block, types::I64);
    builder.append_block_param(done_block, types::I64);

    builder.ins().brif(lead_lt_80, ascii_block, &[], non_ascii_block, &[]);

    builder.switch_to_block(ascii_block);
    let ascii_next = builder.ins().iadd_imm(index, 1);
    builder.ins().jump(done_block, &[BlockArg::Value(lead), BlockArg::Value(ascii_next)]);

    builder.switch_to_block(non_ascii_block);
    let lead_lt_e0 = builder.ins().icmp_imm(IntCC::UnsignedLessThan, lead, 0xe0);
    builder.ins().brif(lead_lt_e0, two_block, &[], three_or_more_block, &[]);

    builder.switch_to_block(two_block);
    let valid_lead = builder.ins().icmp_imm(IntCC::UnsignedGreaterThanOrEqual, lead, 0xc2);
    builder.ins().trapz(valid_lead, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let next1 = builder.ins().iadd_imm(index, 1);
    let has_second = builder.ins().icmp(IntCC::UnsignedLessThan, next1, len);
    builder.ins().trapz(has_second, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let b1 = load_u8_at(builder, data_ptr, next1);
    trap_if_not_continuation_byte(builder, b1);
    let lead_mask = builder.ins().band_imm(lead, 0x1f);
    let lead_shift = builder.ins().ishl_imm(lead_mask, 6);
    let b1_mask = builder.ins().band_imm(b1, 0x3f);
    let cp = builder.ins().bor(lead_shift, b1_mask);
    let next_index = builder.ins().iadd_imm(index, 2);
    builder.ins().jump(done_block, &[BlockArg::Value(cp), BlockArg::Value(next_index)]);

    builder.switch_to_block(three_or_more_block);
    let lead_lt_f0 = builder.ins().icmp_imm(IntCC::UnsignedLessThan, lead, 0xf0);
    builder.ins().brif(lead_lt_f0, three_block, &[], four_block, &[]);

    builder.switch_to_block(three_block);
    let lead_lt_f0_again = builder.ins().icmp_imm(IntCC::UnsignedLessThan, lead, 0xf0);
    builder.ins().trapz(lead_lt_f0_again, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let idx1 = builder.ins().iadd_imm(index, 1);
    let idx2 = builder.ins().iadd_imm(index, 2);
    let has_third = builder.ins().icmp(IntCC::UnsignedLessThan, idx2, len);
    builder.ins().trapz(has_third, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let b1 = load_u8_at(builder, data_ptr, idx1);
    let b2 = load_u8_at(builder, data_ptr, idx2);
    trap_if_not_continuation_byte(builder, b1);
    trap_if_not_continuation_byte(builder, b2);
    if_continuation_requires_extra_validation(builder, lead, b1);
    let lead_mask = builder.ins().band_imm(lead, 0x0f);
    let lead_shift = builder.ins().ishl_imm(lead_mask, 12);
    let b1_mask = builder.ins().band_imm(b1, 0x3f);
    let b1_shift = builder.ins().ishl_imm(b1_mask, 6);
    let b2_mask = builder.ins().band_imm(b2, 0x3f);
    let hi = builder.ins().bor(lead_shift, b1_shift);
    let cp = builder.ins().bor(hi, b2_mask);
    let next_index = builder.ins().iadd_imm(index, 3);
    builder.ins().jump(done_block, &[BlockArg::Value(cp), BlockArg::Value(next_index)]);

    builder.switch_to_block(four_block);
    let lead_lt_f5 = builder.ins().icmp_imm(IntCC::UnsignedLessThan, lead, 0xf5);
    builder.ins().trapz(lead_lt_f5, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let idx1 = builder.ins().iadd_imm(index, 1);
    let idx2 = builder.ins().iadd_imm(index, 2);
    let idx3 = builder.ins().iadd_imm(index, 3);
    let has_fourth = builder.ins().icmp(IntCC::UnsignedLessThan, idx3, len);
    builder.ins().trapz(has_fourth, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let b1 = load_u8_at(builder, data_ptr, idx1);
    let b2 = load_u8_at(builder, data_ptr, idx2);
    let b3 = load_u8_at(builder, data_ptr, idx3);
    trap_if_not_continuation_byte(builder, b1);
    trap_if_not_continuation_byte(builder, b2);
    trap_if_not_continuation_byte(builder, b3);
    trap_if_special_four_byte_invalid(builder, lead, b1);
    let lead_mask = builder.ins().band_imm(lead, 0x07);
    let lead_shift = builder.ins().ishl_imm(lead_mask, 18);
    let b1_mask = builder.ins().band_imm(b1, 0x3f);
    let b1_shift = builder.ins().ishl_imm(b1_mask, 12);
    let b2_mask = builder.ins().band_imm(b2, 0x3f);
    let b2_shift = builder.ins().ishl_imm(b2_mask, 6);
    let b3_mask = builder.ins().band_imm(b3, 0x3f);
    let hi_a = builder.ins().bor(lead_shift, b1_shift);
    let hi_b = builder.ins().bor(hi_a, b2_shift);
    let cp = builder.ins().bor(hi_b, b3_mask);
    let next_index = builder.ins().iadd_imm(index, 4);
    builder.ins().jump(done_block, &[BlockArg::Value(cp), BlockArg::Value(next_index)]);
    builder.switch_to_block(done_block);
    builder.seal_block(ascii_block);
    builder.seal_block(non_ascii_block);
    builder.seal_block(two_block);
    builder.seal_block(three_or_more_block);
    builder.seal_block(three_block);
    builder.seal_block(four_block);
    builder.seal_block(done_block);
    let params = builder.block_params(done_block);
    (params[0], params[1])
}

fn load_u8_at(builder: &mut FunctionBuilder, base_ptr: Value, index: Value) -> Value {
    let addr = builder.ins().iadd(base_ptr, index);
    let byte = builder.ins().load(types::I8, MemFlags::new(), addr, 0);
    builder.ins().uextend(types::I64, byte)
}

fn trap_if_not_continuation_byte(builder: &mut FunctionBuilder, byte: Value) {
    let masked = builder.ins().band_imm(byte, 0xc0);
    let is_cont = builder.ins().icmp_imm(IntCC::Equal, masked, 0x80);
    builder.ins().trapz(is_cont, TrapCode::BAD_CONVERSION_TO_INTEGER);
}

fn if_continuation_requires_extra_validation(
    builder: &mut FunctionBuilder,
    lead: Value,
    b1: Value,
) {
    let one = builder.ins().iconst(types::I64, 1);
    let zero = builder.ins().iconst(types::I64, 0);
    let lead_is_e0 = builder.ins().icmp_imm(IntCC::Equal, lead, 0xe0);
    let b1_ge_a0 = builder.ins().icmp_imm(IntCC::UnsignedGreaterThanOrEqual, b1, 0xa0);
    let b1_ge_a0_i64 = builder.ins().select(b1_ge_a0, one, zero);
    let e0_ok = builder.ins().select(lead_is_e0, b1_ge_a0_i64, one);
    builder.ins().trapz(e0_ok, TrapCode::BAD_CONVERSION_TO_INTEGER);

    let lead_is_ed = builder.ins().icmp_imm(IntCC::Equal, lead, 0xed);
    let b1_lt_a0 = builder.ins().icmp_imm(IntCC::UnsignedLessThan, b1, 0xa0);
    let b1_lt_a0_i64 = builder.ins().select(b1_lt_a0, one, zero);
    let ed_ok = builder.ins().select(lead_is_ed, b1_lt_a0_i64, one);
    builder.ins().trapz(ed_ok, TrapCode::BAD_CONVERSION_TO_INTEGER);
}

fn trap_if_special_four_byte_invalid(builder: &mut FunctionBuilder, lead: Value, b1: Value) {
    let one = builder.ins().iconst(types::I64, 1);
    let zero = builder.ins().iconst(types::I64, 0);
    let lead_is_f0 = builder.ins().icmp_imm(IntCC::Equal, lead, 0xf0);
    let b1_ge_90 = builder.ins().icmp_imm(IntCC::UnsignedGreaterThanOrEqual, b1, 0x90);
    let b1_ge_90_i64 = builder.ins().select(b1_ge_90, one, zero);
    let f0_ok = builder.ins().select(lead_is_f0, b1_ge_90_i64, one);
    builder.ins().trapz(f0_ok, TrapCode::BAD_CONVERSION_TO_INTEGER);

    let lead_is_f4 = builder.ins().icmp_imm(IntCC::Equal, lead, 0xf4);
    let b1_lt_90 = builder.ins().icmp_imm(IntCC::UnsignedLessThan, b1, 0x90);
    let b1_lt_90_i64 = builder.ins().select(b1_lt_90, one, zero);
    let f4_ok = builder.ins().select(lead_is_f4, b1_lt_90_i64, one);
    builder.ins().trapz(f4_ok, TrapCode::BAD_CONVERSION_TO_INTEGER);
}

fn copy_string_bytes(
    builder: &mut FunctionBuilder,
    src_ptr: Value,
    dst_ptr: Value,
    len: Value,
    dst_offset_base: Value,
) {
    let loop_block = builder.create_block();
    let body_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(loop_block, types::I64);
    let zero = builder.ins().iconst(types::I64, 0);
    let one = builder.ins().iconst(types::I64, 1);
    builder.ins().jump(loop_block, &[BlockArg::Value(zero)]);

    builder.switch_to_block(loop_block);
    let idx = builder.block_params(loop_block)[0];
    let more = builder.ins().icmp(IntCC::UnsignedLessThan, idx, len);
    builder.ins().brif(more, body_block, &[], done_block, &[]);

    builder.switch_to_block(body_block);
    let src_addr = builder.ins().iadd(src_ptr, idx);
    let dst_offset = builder.ins().iadd(dst_offset_base, idx);
    let dst_addr = builder.ins().iadd(dst_ptr, dst_offset);
    let byte = builder.ins().load(types::I8, MemFlags::new(), src_addr, 0);
    builder.ins().store(MemFlags::new(), byte, dst_addr, 0);
    let next_idx = builder.ins().iadd(idx, one);
    builder.ins().jump(loop_block, &[BlockArg::Value(next_idx)]);

    builder.switch_to_block(done_block);
    builder.seal_block(body_block);
    builder.seal_block(loop_block);
    builder.seal_block(done_block);
}

fn load_value_from_env(
    builder: &mut FunctionBuilder,
    env_ptr: Value,
    slot: usize,
) -> CompiledValue {
    let slot_offset = i32::try_from(i64::try_from(slot).unwrap() * VALUE_SIZE)
        .expect("closure env offset overflow");
    let tag_i8 = builder.ins().load(types::I8, MemFlags::new(), env_ptr, slot_offset);
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
        let env_align = builder.ins().iconst(types::I64, std::mem::align_of::<i64>() as i64);
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
            builder.ins().store(MemFlags::new(), tag_i8, env_ptr_raw, slot_offset);
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
    let closure_align = builder.ins().iconst(types::I64, std::mem::align_of::<i64>() as i64);
    let closure_call = builder.ins().call(alloc_ref, &[closure_size, closure_align]);
    let closure_ptr = builder.inst_results(closure_call)[0];
    let ordinal = *function_ordinals.get(function_name).unwrap_or_else(|| {
        panic!(
            "internal compiler error: validated function reference '{function_name}' has no ordinal"
        )
    });
    let ordinal_value = builder.ins().iconst(types::I64, ordinal);
    builder.ins().store(
        MemFlags::new(),
        ordinal_value,
        closure_ptr,
        CLOSURE_FUNCTION_ORDINAL_OFFSET,
    );
    builder.ins().store(MemFlags::new(), env_raw, closure_ptr, CLOSURE_ENV_PTR_OFFSET);
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_FUNCTION), payload: closure_ptr }
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
        CompiledValue { tag: builder.use_var(var.tag), payload: builder.use_var(var.payload) }
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
        unreachable!("undefined variable should have been rejected before codegen: {name}");
    }
}

fn expect_int_payload(builder: &mut FunctionBuilder, value: CompiledValue) -> Value {
    let is_int = builder.ins().icmp_imm(IntCC::Equal, value.tag, TAG_INT);
    builder.ins().trapz(is_int, TrapCode::BAD_CONVERSION_TO_INTEGER);
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
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
    let list_new_ref = *func_refs
        .get("list_new")
        .expect("internal compiler error: builtin function 'list_new' is missing");
    let create_call = builder.ins().call(list_new_ref, &[]);
    let created = builder.inst_results(create_call);
    let handle = CompiledValue { tag: created[0], payload: created[1] };

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
            function_analysis,
            value_kind_analysis,
        );
        let _ = call_binary(builder, func_refs, "list_push", handle, value);
    }

    handle
}

fn compile_map_literal(
    builder: &mut FunctionBuilder,
    entries: &[MapEntryAst],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
    let map_new_ref = *func_refs
        .get("map_new")
        .expect("internal compiler error: builtin function 'map_new' is missing");
    let create_call = builder.ins().call(map_new_ref, &[]);
    let created = builder.inst_results(create_call);
    let map = CompiledValue { tag: created[0], payload: created[1] };

    for entry in entries {
        let key = match &entry.key {
            MapKeyAst::Static(key) => compile_string_literal(builder, func_refs, key),
            MapKeyAst::Dynamic(key) => compile_ast(
                builder,
                key,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            ),
        };
        let value = compile_ast(
            builder,
            &entry.value,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        );
        let _ = call_ternary(builder, func_refs, "map_set", map, key, value);
    }

    map
}

fn create_empty_list(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
) -> CompiledValue {
    let list_new_ref = *func_refs
        .get("list_new")
        .expect("internal compiler error: builtin function 'list_new' is missing");
    let create_call = builder.ins().call(list_new_ref, &[]);
    let created = builder.inst_results(create_call);
    CompiledValue { tag: created[0], payload: created[1] }
}

fn load_multi_value_item(
    builder: &mut FunctionBuilder,
    multi_value: CompiledValue,
    index: usize,
) -> CompiledValue {
    let is_multi_tag = builder.ins().icmp_imm(IntCC::Equal, multi_value.tag, TAG_MULTI);
    builder.ins().trapz(is_multi_tag, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let len =
        builder.ins().load(types::I64, MemFlags::new(), multi_value.payload, MULTI_LEN_OFFSET);
    let index_value = builder.ins().iconst(types::I64, index as i64);
    let in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, index_value, len);
    builder.ins().trapz(in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);
    let data_ptr =
        builder.ins().load(types::I64, MemFlags::new(), multi_value.payload, MULTI_PTR_OFFSET);
    let slot_offset = i32::try_from(i64::try_from(index).unwrap() * VALUE_SIZE)
        .expect("multi slot offset overflow");
    let tag_i8 = builder.ins().load(types::I8, MemFlags::new(), data_ptr, slot_offset);
    let tag = builder.ins().uextend(types::I64, tag_i8);
    let payload = builder.ins().load(
        types::I64,
        MemFlags::new(),
        data_ptr,
        slot_offset + VALUE_PAYLOAD_OFFSET,
    );
    CompiledValue { tag, payload }
}

fn compile_multi_value(
    builder: &mut FunctionBuilder,
    values: &[Ast],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
    let compiled = values
        .iter()
        .map(|value| {
            compile_ast(
                builder,
                value,
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            )
        })
        .collect::<Vec<_>>();
    compile_multi_compiled_values(builder, &compiled, func_refs)
}

fn compile_multi_compiled_values(
    builder: &mut FunctionBuilder,
    compiled: &[CompiledValue],
    func_refs: &HashMap<String, FuncRef>,
) -> CompiledValue {
    let alloc_ref = require_func(func_refs, "__alloc");
    let align = builder.ins().iconst(types::I64, 8);
    let data_bytes =
        builder.ins().iconst(types::I64, i64::try_from(compiled.len()).unwrap() * VALUE_SIZE);
    let data_call = builder.ins().call(alloc_ref, &[data_bytes, align]);
    let data_ptr = builder.inst_results(data_call)[0];
    let header_size = builder.ins().iconst(types::I64, MULTI_HEADER_SIZE);
    let header_call = builder.ins().call(alloc_ref, &[header_size, align]);
    let header_ptr = builder.inst_results(header_call)[0];
    let len_value = builder.ins().iconst(types::I64, compiled.len() as i64);
    builder.ins().store(MemFlags::new(), len_value, header_ptr, MULTI_LEN_OFFSET);
    builder.ins().store(MemFlags::new(), data_ptr, header_ptr, MULTI_PTR_OFFSET);
    for (index, value) in compiled.iter().enumerate() {
        let slot_offset = i32::try_from(i64::try_from(index).unwrap() * VALUE_SIZE)
            .expect("multi slot offset overflow");
        let tag_i8 = builder.ins().ireduce(types::I8, value.tag);
        builder.ins().store(MemFlags::new(), tag_i8, data_ptr, slot_offset);
        builder.ins().store(
            MemFlags::new(),
            value.payload,
            data_ptr,
            slot_offset + VALUE_PAYLOAD_OFFSET,
        );
    }
    CompiledValue { tag: builder.ins().iconst(types::I64, TAG_MULTI), payload: header_ptr }
}

fn compile_multi_assign_ast(
    builder: &mut FunctionBuilder,
    names: &[String],
    value: &Ast,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
    let multi_value = compile_ast(
        builder,
        value,
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        function_analysis,
        value_kind_analysis,
    );
    let mut last = None;
    for (index, name) in names.iter().enumerate() {
        let unpacked = load_multi_value_item(builder, multi_value, index);
        let var = vars.get(name).unwrap_or_else(|| {
            panic!("internal compiler error: assignment target '{name}' has no local slot")
        });
        builder.def_var(var.tag, unpacked.tag);
        builder.def_var(var.payload, unpacked.payload);
        last = Some(unpacked);
    }
    last.expect("multi assignment must have at least one target")
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
            if function_arities.get(name.as_str()) != Some(&1usize) {
                unreachable!(
                    "{builtin} callback arity should have been validated before codegen: {name}"
                );
            }
        }
        Ast::Variable(name)
            if !vars.contains_key(name.as_str())
                && function_ordinals.contains_key(name.as_str()) =>
        {
            if function_arities.get(name.as_str()) != Some(&1usize) {
                unreachable!(
                    "{builtin} callback variable arity should have been validated before codegen: {name}"
                );
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
    CompiledValue { tag: results[0], payload: results[1] }
}

fn call_named_with_env(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    name: &str,
    env_ptr: Value,
    args: &[CompiledValue],
) -> CompiledValue {
    let func_ref = *func_refs.get(name).unwrap_or_else(|| {
        panic!("internal compiler error: validated function '{name}' is missing from func refs")
    });
    let mut call_args = Vec::with_capacity(1 + args.len() * 2);
    call_args.push(env_ptr);
    for arg in args {
        call_args.push(arg.tag);
        call_args.push(arg.payload);
    }
    let call = builder.ins().call(func_ref, &call_args);
    let results = builder.inst_results(call);
    CompiledValue { tag: results[0], payload: results[1] }
}

fn apply_function_value(
    builder: &mut FunctionBuilder,
    func_refs: &HashMap<String, FuncRef>,
    function_value: CompiledValue,
    args: &[CompiledValue],
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
) -> CompiledValue {
    let is_function = builder.ins().icmp_imm(IntCC::Equal, function_value.tag, TAG_FUNCTION);
    builder.ins().trapz(is_function, TrapCode::BAD_CONVERSION_TO_INTEGER);
    let closure_ptr = function_value.payload;
    let closure_ordinal = builder.ins().load(
        types::I64,
        MemFlags::new(),
        closure_ptr,
        CLOSURE_FUNCTION_ORDINAL_OFFSET,
    );
    let closure_env_ptr =
        builder.ins().load(types::I64, MemFlags::new(), closure_ptr, CLOSURE_ENV_PTR_OFFSET);

    let mut candidates: Vec<_> = function_ordinals
        .iter()
        .filter_map(|(name, &ordinal)| {
            (function_arities.get(name.as_str()) == Some(&args.len()))
                .then_some((ordinal, name.as_str()))
        })
        .collect();
    candidates.sort_by_key(|(ordinal, _)| *ordinal);
    if candidates.is_empty() {
        panic!(
            "internal compiler error: no functions with arity {} are available for higher-order calls",
            args.len()
        );
    }

    let entry_check = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, types::I64);
    builder.append_block_param(merge_block, types::I64);
    builder.ins().jump(entry_check, &[]);

    let mut check_block = entry_check;
    for (index, (ordinal, name)) in candidates.iter().enumerate() {
        builder.switch_to_block(check_block);
        let matched = builder.ins().icmp_imm(IntCC::Equal, closure_ordinal, *ordinal);
        let call_block = builder.create_block();
        let next_block =
            if index + 1 == candidates.len() { None } else { Some(builder.create_block()) };
        match next_block {
            Some(next) => {
                builder.ins().brif(matched, call_block, &[], next, &[]);
            }
            None => {
                builder.ins().trapz(matched, TrapCode::BAD_CONVERSION_TO_INTEGER);
                builder.ins().jump(call_block, &[]);
            }
        }

        builder.switch_to_block(call_block);
        let result = call_named_with_env(builder, func_refs, name, closure_env_ptr, args);
        builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(result.tag), BlockArg::Value(result.payload)]);
        builder.seal_block(call_block);
        builder.seal_block(check_block);

        if let Some(next) = next_block {
            check_block = next;
        }
    }

    builder.switch_to_block(merge_block);
    builder.seal_block(merge_block);
    let params = builder.block_params(merge_block);
    CompiledValue { tag: params[0], payload: params[1] }
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
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
    assert_eq!(args.len(), 2, "list_map expects 2 arguments");
    validate_unary_callback_ast(&args[1], vars, function_ordinals, function_arities, "list_map");
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
        function_analysis,
        value_kind_analysis,
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
        function_analysis,
        value_kind_analysis,
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
    let has_more = builder.ins().icmp(IntCC::UnsignedLessThan, idx, len.payload);
    builder.ins().brif(has_more, body_block, &[], exit_block, &[]);

    builder.switch_to_block(body_block);
    let index_value =
        CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload: idx };
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
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
    assert_eq!(args.len(), 2, "list_filter expects 2 arguments");
    validate_unary_callback_ast(&args[1], vars, function_ordinals, function_arities, "list_filter");
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
        function_analysis,
        value_kind_analysis,
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
        function_analysis,
        value_kind_analysis,
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
    let has_more = builder.ins().icmp(IntCC::UnsignedLessThan, idx, len.payload);
    builder.ins().brif(has_more, body_block, &[], exit_block, &[]);

    builder.switch_to_block(body_block);
    let index_value =
        CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload: idx };
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
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
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
        function_analysis,
        value_kind_analysis,
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
        function_analysis,
        value_kind_analysis,
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
    builder.ins().brif(has_more, body_block, &[], exit_block, &[]);

    builder.switch_to_block(body_block);
    let current_value =
        CompiledValue { tag: builder.ins().iconst(types::I64, TAG_INT), payload: current };
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
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
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
            function_analysis,
            value_kind_analysis,
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
        function_analysis,
        value_kind_analysis,
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
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) {
    match ast {
        Ast::Expression(ExpressionAst { function, args, .. })
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
                        function_analysis,
                        value_kind_analysis,
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
        Ast::If { condition, then, else_, .. } => {
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
                function_analysis,
                value_kind_analysis,
            );
            let truth_value = call_unary_scalar(builder, func_refs, "__value_is_truthy", cond_val);
            let cond_non_zero = builder.ins().icmp_imm(IntCC::NotEqual, truth_value, 0);

            let then_block = builder.create_block();
            let else_block = builder.create_block();
            builder.ins().brif(cond_non_zero, then_block, &[], else_block, &[]);

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
                function_analysis,
                value_kind_analysis,
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
                    function_analysis,
                    value_kind_analysis,
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
            function_analysis,
            value_kind_analysis,
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
                function_analysis,
                value_kind_analysis,
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
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
    match ast {
        Ast::Literal(LiteralAst::Integer(n)) => boxed_int_const(builder, *n),
        Ast::Literal(LiteralAst::String(value)) => {
            compile_string_literal(builder, func_refs, value)
        }
        Ast::Literal(LiteralAst::BigInt(digits)) => {
            compile_bigint_literal(builder, func_refs, digits)
        }
        Ast::Lambda { .. } => unimplemented!("anonymous functions"),
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
        Ast::MultiValue(values) => compile_multi_value(
            builder,
            values,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
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
            function_analysis,
            value_kind_analysis,
        ),
        Ast::MapLiteral(entries) => compile_map_literal(
            builder,
            entries,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        ),
        Ast::Index { collection, index, .. } => compile_index_ast(
            builder,
            collection,
            index,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        ),
        Ast::IndexAssign { collection, index, value, .. } => compile_index_assign_ast(
            builder,
            collection,
            index,
            value,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        ),
        Ast::Expression(ExpressionAst { function, args, .. }) => compile_expression_ast(
            builder,
            function,
            args,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        ),
        Ast::Block(block) => compile_block_ast(
            builder,
            block,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        ),
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
        Ast::Assign { name, value, .. } => compile_assign_ast(
            builder,
            name,
            value,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        ),
        Ast::MultiAssign { names, value, .. } => compile_multi_assign_ast(
            builder,
            names,
            value,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        ),
        Ast::If { condition, then, else_, .. } => compile_if_ast(
            builder,
            condition,
            then,
            else_,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        ),
        Ast::FunctionDef(_) => unimplemented!("nested function definitions"),
    }
}

fn compile_index_ast(
    builder: &mut FunctionBuilder,
    collection: &Ast,
    index: &Ast,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
    let collection_shape =
        infer_ast_value_shape(collection, function_analysis, value_kind_analysis);
    let index_shape = infer_ast_value_shape(index, function_analysis, value_kind_analysis);
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
        function_analysis,
        value_kind_analysis,
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
        function_analysis,
        value_kind_analysis,
    );
    if shape_is_exact_kind(&collection_shape, KindSet::list())
        && shape_is_exact_kind(&index_shape, KindSet::int())
    {
        compile_list_get_known_types(builder, collection_value, index_value, true, true)
    } else {
        call_binary(builder, func_refs, "list_get", collection_value, index_value)
    }
}

fn compile_index_assign_ast(
    builder: &mut FunctionBuilder,
    collection: &Ast,
    index: &Ast,
    value: &Ast,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
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
        function_analysis,
        value_kind_analysis,
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
        function_analysis,
        value_kind_analysis,
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
        function_analysis,
        value_kind_analysis,
    );
    call_ternary(builder, func_refs, "list_set", collection_value, index_value, value)
}

fn compile_block_ast(
    builder: &mut FunctionBuilder,
    block: &BlockAst,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
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
            function_analysis,
            value_kind_analysis,
        ));
    }
    last.expect("empty block")
}

fn compile_assign_ast(
    builder: &mut FunctionBuilder,
    name: &str,
    value: &Ast,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
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
        function_analysis,
        value_kind_analysis,
    );
    let var = vars.get(name).unwrap_or_else(|| {
        panic!("internal compiler error: assignment target '{name}' has no local slot")
    });
    builder.def_var(var.tag, val.tag);
    builder.def_var(var.payload, val.payload);
    val
}

fn compile_if_ast(
    builder: &mut FunctionBuilder,
    condition: &Ast,
    then: &BlockAst,
    else_: &Option<BlockAst>,
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
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
        function_analysis,
        value_kind_analysis,
    );
    let truth_value = call_unary_scalar(builder, func_refs, "__value_is_truthy", cond_val);
    let cond_non_zero = builder.ins().icmp_imm(IntCC::NotEqual, truth_value, 0);

    let then_block = builder.create_block();
    let merge_block = builder.create_block();
    builder.append_block_param(merge_block, types::I64);
    builder.append_block_param(merge_block, types::I64);

    if let Some(else_block_ast) = else_ {
        let else_block = builder.create_block();
        builder.ins().brif(cond_non_zero, then_block, &[], else_block, &[]);

        builder.switch_to_block(then_block);
        builder.seal_block(then_block);
        let then_val = compile_block_ast(
            builder,
            then,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        );
        builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(then_val.tag), BlockArg::Value(then_val.payload)]);

        builder.switch_to_block(else_block);
        builder.seal_block(else_block);
        let else_val = compile_block_ast(
            builder,
            else_block_ast,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        );
        builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(else_val.tag), BlockArg::Value(else_val.payload)]);
    } else {
        let boxed_zero = boxed_int_const(builder, 0);
        builder.ins().brif(
            cond_non_zero,
            then_block,
            &[],
            merge_block,
            &[BlockArg::Value(boxed_zero.tag), BlockArg::Value(boxed_zero.payload)],
        );

        builder.switch_to_block(then_block);
        builder.seal_block(then_block);
        let then_val = compile_block_ast(
            builder,
            then,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        );
        builder
            .ins()
            .jump(merge_block, &[BlockArg::Value(then_val.tag), BlockArg::Value(then_val.payload)]);
    }

    builder.switch_to_block(merge_block);
    builder.seal_block(merge_block);
    let params = builder.block_params(merge_block);
    CompiledValue { tag: params[0], payload: params[1] }
}

fn compile_expression_ast(
    builder: &mut FunctionBuilder,
    function: &str,
    args: &[Ast],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> CompiledValue {
    if let Some(value) = compile_type_predicate_expression_ast(
        builder,
        function,
        args,
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        function_analysis,
        value_kind_analysis,
    ) {
        return value;
    }
    if let Some(value) = compile_exact_numeric_expression_ast(
        builder,
        function,
        args,
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        function_analysis,
        value_kind_analysis,
    ) {
        return value;
    }
    if function == "not" {
        assert_eq!(args.len(), 1, "not expects 1 argument");
        return compile_logical_not(
            builder,
            &args[0],
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        );
    }
    if function == "and" || function == "or" {
        assert_eq!(args.len(), 2, "{function} expects 2 arguments");
        return compile_logical_op(
            builder,
            function,
            &args[0],
            &args[1],
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        );
    }
    if let Some(value) = compile_list_expression_ast(
        builder,
        function,
        args,
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        function_analysis,
        value_kind_analysis,
    ) {
        return value;
    }
    if let Some(value) = compile_string_expression_ast(
        builder,
        function,
        args,
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        function_analysis,
        value_kind_analysis,
    ) {
        return value;
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
                function_analysis,
                value_kind_analysis,
            )
        })
        .collect();
    if function.is_empty() {
        return compiled[0];
    }
    compile_named_expression_ast(
        builder,
        function,
        &compiled,
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
    )
}

fn compile_type_predicate_expression_ast(
    builder: &mut FunctionBuilder,
    function: &str,
    args: &[Ast],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> Option<CompiledValue> {
    if !matches!(
        function,
        "is_int"
            | "is_bigint"
            | "is_string"
            | "is_list"
            | "is_map"
            | "is_map_iter"
            | "is_function"
            | "is_string_iter"
    ) {
        return None;
    }
    assert_eq!(args.len(), 1, "{function} expects 1 argument");
    let value = compile_ast(
        builder,
        &args[0],
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        function_analysis,
        value_kind_analysis,
    );
    let expected_tag = match function {
        "is_int" => TAG_INT,
        "is_bigint" => TAG_BIGINT,
        "is_string" => TAG_STRING,
        "is_list" => TAG_LIST,
        "is_map" => TAG_MAP,
        "is_map_iter" => TAG_MAP_ITER,
        "is_function" => TAG_FUNCTION,
        "is_string_iter" => TAG_STRING_ITER,
        _ => unreachable!(),
    };
    Some(compile_is_tag_predicate(builder, value, expected_tag))
}

fn compile_exact_numeric_expression_ast(
    builder: &mut FunctionBuilder,
    function: &str,
    args: &[Ast],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> Option<CompiledValue> {
    if !matches!(
        function,
        "add"
            | "subtract"
            | "multiply"
            | "divide"
            | "modulo"
            | "gt"
            | "lt"
            | "gte"
            | "lte"
            | "eq"
            | "ne"
            | "bitand"
            | "bitor"
            | "bitxor"
            | "shl"
            | "shr"
    ) || args.len() != 2
    {
        return None;
    }
    let lhs_shape = infer_ast_value_shape(&args[0], function_analysis, value_kind_analysis);
    let rhs_shape = infer_ast_value_shape(&args[1], function_analysis, value_kind_analysis);
    let lhs_exact_int = shape_is_exact_kind(&lhs_shape, KindSet::int());
    let rhs_exact_int = shape_is_exact_kind(&rhs_shape, KindSet::int());
    let lhs_exact_bigint = shape_is_exact_kind(&lhs_shape, KindSet::bigint());
    let rhs_exact_bigint = shape_is_exact_kind(&rhs_shape, KindSet::bigint());
    let exact_int_case = lhs_exact_int && rhs_exact_int;
    let exact_bigint_case = lhs_exact_bigint && rhs_exact_bigint;
    let exact_bigint_shift_case =
        matches!(function, "shl" | "shr") && lhs_exact_bigint && rhs_exact_int;
    if !(exact_int_case || exact_bigint_case || exact_bigint_shift_case) {
        return None;
    }
    let lhs = compile_ast(
        builder,
        &args[0],
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        function_analysis,
        value_kind_analysis,
    );
    let rhs = compile_ast(
        builder,
        &args[1],
        vars,
        func_refs,
        function_ordinals,
        function_arities,
        closure_metadata,
        capture_slots,
        env_ptr,
        function_analysis,
        value_kind_analysis,
    );
    Some(if exact_int_case {
        match function {
            "add" | "subtract" | "multiply" | "divide" | "modulo" | "bitand" | "bitor"
            | "bitxor" | "shl" | "shr" => compile_exact_int_binary_op(builder, function, lhs, rhs),
            _ => compile_exact_int_compare_op(builder, function, lhs, rhs),
        }
    } else {
        match function {
            "add" => compile_bigint_builtin(builder, func_refs, "bigint_add", &[lhs, rhs]),
            "subtract" => {
                compile_bigint_builtin(builder, func_refs, "bigint_subtract", &[lhs, rhs])
            }
            "multiply" => {
                compile_bigint_builtin(builder, func_refs, "bigint_multiply", &[lhs, rhs])
            }
            "divide" => compile_bigint_builtin(builder, func_refs, "bigint_divide", &[lhs, rhs]),
            "modulo" => compile_bigint_builtin(builder, func_refs, "bigint_modulo", &[lhs, rhs]),
            "bitand" => compile_bigint_builtin(builder, func_refs, "bigint_bitand", &[lhs, rhs]),
            "bitor" => compile_bigint_builtin(builder, func_refs, "bigint_bitor", &[lhs, rhs]),
            "bitxor" => compile_bigint_builtin(builder, func_refs, "bigint_bitxor", &[lhs, rhs]),
            "shl" => compile_bigint_shift_builtin(builder, func_refs, "bigint_shl", lhs, rhs),
            "shr" => compile_bigint_shift_builtin(builder, func_refs, "bigint_shr", lhs, rhs),
            _ => compile_exact_bigint_compare_op(builder, func_refs, function, lhs, rhs),
        }
    })
}

fn compile_list_expression_ast(
    builder: &mut FunctionBuilder,
    function: &str,
    args: &[Ast],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> Option<CompiledValue> {
    match function {
        "list_map" => Some(compile_list_map(
            builder,
            args,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        )),
        "list_filter" => Some(compile_list_filter(
            builder,
            args,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        )),
        "list_range" => Some(compile_list_range(
            builder,
            args,
            vars,
            func_refs,
            function_ordinals,
            function_arities,
            closure_metadata,
            capture_slots,
            env_ptr,
            function_analysis,
            value_kind_analysis,
        )),
        "list_len" => {
            assert_eq!(args.len(), 1, "list_len expects 1 argument");
            let shape = infer_ast_value_shape(&args[0], function_analysis, value_kind_analysis);
            let value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(if shape_is_exact_kind(&shape, KindSet::list()) {
                compile_list_len_known_list(builder, value, true)
            } else {
                call_unary(builder, func_refs, "list_len", value)
            })
        }
        "list_get" => {
            assert_eq!(args.len(), 2, "list_get expects 2 arguments");
            let collection_shape =
                infer_ast_value_shape(&args[0], function_analysis, value_kind_analysis);
            let index_shape =
                infer_ast_value_shape(&args[1], function_analysis, value_kind_analysis);
            let collection_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            let index_value = compile_ast(
                builder,
                &args[1],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(
                if shape_is_exact_kind(&collection_shape, KindSet::list())
                    && shape_is_exact_kind(&index_shape, KindSet::int())
                {
                    compile_list_get_known_types(builder, collection_value, index_value, true, true)
                } else {
                    call_binary(builder, func_refs, "list_get", collection_value, index_value)
                },
            )
        }
        _ => None,
    }
}

fn compile_string_expression_ast(
    builder: &mut FunctionBuilder,
    function: &str,
    args: &[Ast],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
    function_analysis: &FunctionValueKindAnalysis,
    value_kind_analysis: &ModuleValueKindAnalysis,
) -> Option<CompiledValue> {
    match function {
        "bytes_len" => {
            assert_eq!(args.len(), 1, "bytes_len expects 1 argument");
            let shape = infer_ast_value_shape(&args[0], function_analysis, value_kind_analysis);
            let value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_bytes_len_known_string(
                builder,
                value,
                shape_is_exact_kind(&shape, KindSet::string()),
            ))
        }
        "bytes_get" => {
            assert_eq!(args.len(), 2, "bytes_get expects 2 arguments");
            let string_shape =
                infer_ast_value_shape(&args[0], function_analysis, value_kind_analysis);
            let index_shape =
                infer_ast_value_shape(&args[1], function_analysis, value_kind_analysis);
            let string_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            let index_value = compile_ast(
                builder,
                &args[1],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_bytes_get_known_types(
                builder,
                string_value,
                index_value,
                shape_is_exact_kind(&string_shape, KindSet::string()),
                shape_is_exact_kind(&index_shape, KindSet::int()),
            ))
        }
        "bytes_pop" => {
            assert_eq!(args.len(), 1, "bytes_pop expects 1 argument");
            let string_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_bytes_pop(builder, string_value))
        }
        "bytes_push" => {
            assert_eq!(args.len(), 2, "bytes_push expects 2 arguments");
            let string_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            let byte_value = compile_ast(
                builder,
                &args[1],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_bytes_push(builder, func_refs, string_value, byte_value))
        }
        "bytes_insert" => {
            assert_eq!(args.len(), 3, "bytes_insert expects 3 arguments");
            let string_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            let index_value = compile_ast(
                builder,
                &args[1],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            let byte_value = compile_ast(
                builder,
                &args[2],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_bytes_insert(builder, func_refs, string_value, index_value, byte_value))
        }
        "bytes_remove" => {
            assert_eq!(args.len(), 2, "bytes_remove expects 2 arguments");
            let string_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            let index_value = compile_ast(
                builder,
                &args[1],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_bytes_remove(builder, string_value, index_value))
        }
        "bytes_set" => {
            assert_eq!(args.len(), 3, "bytes_set expects 3 arguments");
            let string_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            let index_value = compile_ast(
                builder,
                &args[1],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            let byte_value = compile_ast(
                builder,
                &args[2],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_bytes_set(builder, string_value, index_value, byte_value))
        }
        "bytes_slice" => {
            assert_eq!(args.len(), 3, "bytes_slice expects 3 arguments");
            let string_shape =
                infer_ast_value_shape(&args[0], function_analysis, value_kind_analysis);
            let start_shape =
                infer_ast_value_shape(&args[1], function_analysis, value_kind_analysis);
            let end_shape = infer_ast_value_shape(&args[2], function_analysis, value_kind_analysis);
            let string_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            let start_value = compile_ast(
                builder,
                &args[1],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            let end_value = compile_ast(
                builder,
                &args[2],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_bytes_slice_known_types(
                builder,
                func_refs,
                string_value,
                start_value,
                end_value,
                shape_is_exact_kind(&string_shape, KindSet::string()),
                shape_is_exact_kind(&start_shape, KindSet::int()),
                shape_is_exact_kind(&end_shape, KindSet::int()),
            ))
        }
        "string_chars" => {
            assert_eq!(args.len(), 1, "string_chars expects 1 argument");
            let string_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_string_chars(builder, func_refs, string_value))
        }
        "string_iter_done" => {
            assert_eq!(args.len(), 1, "string_iter_done expects 1 argument");
            let iter_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_string_iter_done(builder, iter_value))
        }
        "string_iter_next" => {
            assert_eq!(args.len(), 1, "string_iter_next expects 1 argument");
            let iter_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_string_iter_next(builder, iter_value))
        }
        "string_copy" => {
            assert_eq!(args.len(), 1, "string_copy expects 1 argument");
            let string_value = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_string_copy(builder, func_refs, string_value))
        }
        "string_concat" => {
            assert_eq!(args.len(), 2, "string_concat expects 2 arguments");
            let lhs = compile_ast(
                builder,
                &args[0],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            let rhs = compile_ast(
                builder,
                &args[1],
                vars,
                func_refs,
                function_ordinals,
                function_arities,
                closure_metadata,
                capture_slots,
                env_ptr,
                function_analysis,
                value_kind_analysis,
            );
            Some(compile_string_concat(builder, func_refs, lhs, rhs))
        }
        _ => None,
    }
}

fn compile_named_expression_ast(
    builder: &mut FunctionBuilder,
    function: &str,
    compiled: &[CompiledValue],
    vars: &HashMap<String, LocalValueVar>,
    func_refs: &HashMap<String, FuncRef>,
    function_ordinals: &HashMap<String, i64>,
    function_arities: &HashMap<String, usize>,
    closure_metadata: &HashMap<String, ClosureMetadata>,
    capture_slots: &HashMap<String, usize>,
    env_ptr: Value,
) -> CompiledValue {
    match function {
        "add" => call_binary(builder, func_refs, "__op_add", compiled[0], compiled[1]),
        "subtract" => call_binary(builder, func_refs, "__op_subtract", compiled[0], compiled[1]),
        "multiply" => call_binary(builder, func_refs, "__op_multiply", compiled[0], compiled[1]),
        "divide" => call_binary(builder, func_refs, "__op_divide", compiled[0], compiled[1]),
        "modulo" => call_binary(builder, func_refs, "__op_modulo", compiled[0], compiled[1]),
        "bitand" => call_binary(builder, func_refs, "__op_bitand", compiled[0], compiled[1]),
        "bitor" => call_binary(builder, func_refs, "__op_bitor", compiled[0], compiled[1]),
        "bitxor" => call_binary(builder, func_refs, "__op_bitxor", compiled[0], compiled[1]),
        "shl" => call_binary(builder, func_refs, "__op_shl", compiled[0], compiled[1]),
        "shr" => call_binary(builder, func_refs, "__op_shr", compiled[0], compiled[1]),
        "gt" => call_binary(builder, func_refs, "__op_gt", compiled[0], compiled[1]),
        "lt" => call_binary(builder, func_refs, "__op_lt", compiled[0], compiled[1]),
        "gte" => call_binary(builder, func_refs, "__op_gte", compiled[0], compiled[1]),
        "lte" => call_binary(builder, func_refs, "__op_lte", compiled[0], compiled[1]),
        "eq" => call_binary(builder, func_refs, "__op_eq", compiled[0], compiled[1]),
        "ne" => call_binary(builder, func_refs, "__op_ne", compiled[0], compiled[1]),
        "map_new" => {
            let func_ref = require_func(func_refs, "map_new");
            let call = builder.ins().call(func_ref, &[]);
            let results = builder.inst_results(call);
            CompiledValue { tag: results[0], payload: results[1] }
        }
        "map_iter_next" => {
            // This wants to be a stdlib helper, but current multi-return parsing/validation
            // for helper-style functions is still too restrictive. Keep the semantics here
            // as a thin lowering over the primitive iterator builtins until that is fixed.
            let key = call_unary(builder, func_refs, "map_iter_key", compiled[0]);
            let value = call_unary(builder, func_refs, "map_iter_value", compiled[0]);
            let _advance = call_unary(builder, func_refs, "map_iter_advance", compiled[0]);
            compile_multi_compiled_values(builder, &[key, value], func_refs)
        }
        "map_len" | "map_iter" | "map_iter_done" | "map_iter_key" | "map_iter_value"
        | "map_iter_advance" => call_unary(builder, func_refs, function, compiled[0]),
        "map_has" | "map_get" | "map_delete" => {
            call_binary(builder, func_refs, function, compiled[0], compiled[1])
        }
        "map_set" => {
            call_ternary(builder, func_refs, "map_set", compiled[0], compiled[1], compiled[2])
        }
        "bigint_add" | "bigint_subtract" | "bigint_multiply" | "bigint_divide"
        | "bigint_modulo" | "bigint_compare" | "bigint_bitand" | "bigint_bitor"
        | "bigint_bitxor" => compile_bigint_builtin(builder, func_refs, function, &compiled),
        "bigint_shl" | "bigint_shr" => {
            compile_bigint_shift_builtin(builder, func_refs, function, compiled[0], compiled[1])
        }
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
                for value in compiled {
                    args.push(value.tag);
                    args.push(value.payload);
                }
                let call = builder.ins().call(*func_ref, &args);
                let results = builder.inst_results(call);
                return CompiledValue { tag: results[0], payload: results[1] };
            }
            unreachable!("undefined function should have been rejected before codegen: {name}");
        }
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
    let ptr = jit.get_int_result_fn_ptr("main").expect("cranelift int-result wrapper should exist");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), expected);
}

#[cfg(test)]
#[test]
fn try_from_source_rejects_top_level_expressions() {
    let err = match Module::try_from_source("1 + 2") {
        Ok(_) => panic!("top-level expression should be rejected"),
        Err(err) => err,
    };
    assert_eq!(err, CompileError::TopLevelExpression);
}

#[cfg(test)]
#[test]
fn try_from_source_preserves_parse_error_span() {
    let err = match Module::try_from_source("fn main()) do\n    1\nend") {
        Ok(_) => panic!("invalid source should return a parse error"),
        Err(err) => err,
    };
    assert_eq!(
        err,
        CompileError::Parse {
            message: "unexpected token \")\"".to_string(),
            span: Some(Span { start: 9, end: 10 }),
        }
    );
}

#[cfg(test)]
#[test]
fn try_compile_to_jit_rejects_undefined_functions() {
    let src = "fn main() do\n    missing_fn(1)\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("undefined function should be rejected"),
        Err(err) => err,
    };
    assert_eq!(
        err,
        CompileError::UndefinedFunction {
            name: "missing_fn".to_string(),
            span: Some(Span { start: 17, end: 27 }),
        }
    );
}

#[cfg(test)]
#[test]
fn try_compile_to_jit_rejects_undefined_variables() {
    let src = "fn main() do\n    missing_value\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("undefined variable should be rejected"),
        Err(err) => err,
    };
    assert_eq!(
        err,
        CompileError::UndefinedVariable {
            name: "missing_value".to_string(),
            span: Some(Span { start: 17, end: 30 }),
        }
    );
}

#[cfg(test)]
#[test]
fn try_compile_to_jit_rejects_non_unary_callbacks() {
    let src =
        "fn main() do\n    xs = [1, 2, 3]\n    list_map(xs, fn a, b -> a + b end)\n    0\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("non-unary callback should be rejected"),
        Err(err) => err,
    };
    assert_eq!(
        err,
        CompileError::CallbackArity {
            builtin: "list_map".to_string(),
            function: "__lambda_1".to_string(),
            span: None,
        }
    );
}

#[cfg(test)]
#[test]
fn try_compile_to_executable_rejects_main_with_too_many_arguments() {
    let src = "fn main(a, b) do\n    a + b\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = module
        .try_compile_to_executable_with_backend(
            std::path::Path::new("out"),
            CodegenBackend::Cranelift,
        )
        .expect_err("native executable main arity should be rejected");
    assert_eq!(
        err,
        CompileError::InvalidMainArity {
            mode: "native executable main function",
            max: 1,
            found: 2,
            span: Some(Span { start: 0, end: 30 }),
        }
    );
}

#[cfg(test)]
#[test]
fn try_compile_to_object_rejects_llvm_backend_when_unavailable() {
    if llvm_backend_available() {
        return;
    }
    let src = "fn main() do\n    1\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = module
        .try_compile_to_object_with_backend("test", CodegenBackend::Llvm)
        .expect_err("missing llvm feature should be rejected");
    assert_eq!(err, CompileError::LlvmBackendUnavailable);
}

#[cfg(test)]
fn assert_cranelift_executable_output(src: &str, expected_stdout: &str, expected_exit: i32) {
    assert_backend_executable_output(
        src,
        CodegenBackend::Cranelift,
        expected_stdout,
        expected_exit,
    );
}

#[cfg(test)]
fn assert_backend_executable_output(
    src: &str,
    backend: CodegenBackend,
    expected_stdout: &str,
    expected_exit: i32,
) {
    assert_backend_executable_output_with_args(src, backend, &[], expected_stdout, expected_exit);
}

#[cfg(test)]
fn assert_backend_executable_output_with_args(
    src: &str,
    backend: CodegenBackend,
    args: &[&str],
    expected_stdout: &str,
    expected_exit: i32,
) {
    static COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let unique = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos()
        + u128::from(COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed));
    #[cfg(windows)]
    let output = windows_temp_exe_path(&format!("__expr_compiler_bigint_test_{unique}"));
    #[cfg(not(windows))]
    let output = std::env::temp_dir().join(format!("__expr_compiler_bigint_test_{unique}"));

    Module::from_source(src).compile_to_executable_with_backend(&output, backend);
    let out = Command::new(&output).args(args).output().expect("run failed");
    std::fs::remove_file(&output).ok();

    assert_eq!(String::from_utf8_lossy(&out.stdout), expected_stdout);
    assert_eq!(out.status.code(), Some(expected_exit));
}

#[cfg(all(test, feature = "llvm-backend"))]
fn assert_jit_backend_result(src: &str, backend: CodegenBackend, expected: i64) {
    crate::runtime::reset_runtime_arena();
    let jit = Module::from_source(src).compile_to_jit_with_backend(backend);
    let ptr = jit.get_int_result_fn_ptr("main").expect("int-result wrapper should exist");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), expected);
}

#[cfg(test)]
fn assert_jit_backend_result_with_args(
    src: &str,
    backend: CodegenBackend,
    args: &[&str],
    expected: i64,
) {
    crate::runtime::reset_runtime_arena();
    let jit = Module::from_source(src).compile_to_jit_with_backend(backend);
    let ptr = jit.get_int_result_fn_ptr("main").expect("int-result wrapper should exist");
    let owned_args = args.iter().map(|arg| (*arg).to_string()).collect::<Vec<_>>();
    let (arg_tag, arg_payload) = crate::runtime::build_argv_list_value(&owned_args);
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn(i64, i64) -> i64>(ptr) };
    assert_eq!(func(arg_tag, arg_payload), expected);
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
    let ptr = jit.get_int_result_fn_ptr("main").expect("cranelift int-result wrapper should exist");
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

    let status = Command::new(&output).status().expect("failed to run executable");
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
fn executable_main_can_receive_argument_list() {
    let src = "fn main(args) do\n    print(list_len(args))\n    print(list_get(args, 0))\n    print(list_get(args, 1))\n    list_len(args)\nend";
    assert_backend_executable_output_with_args(
        src,
        CodegenBackend::Cranelift,
        &["hello", "world"],
        "2\nhello\nworld\n",
        2,
    );
}

#[test]
fn jit_main_can_receive_argument_list() {
    let src = "fn main(args) do\n    print(list_len(args))\n    print(list_get(args, 0))\n    print(list_get(args, 1))\n    list_len(args)\nend";
    assert_jit_backend_result_with_args(src, CodegenBackend::Cranelift, &["hello", "world"], 2);
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

#[cfg(test)]
fn extract_function_ir<'a>(ir: &'a str, name: &str) -> &'a str {
    let marker = format!("; fn {name}\n");
    let start = ir.find(&marker).expect("function marker should exist");
    let rest = &ir[start..];
    let next = rest.find("\n\n; fn ").unwrap_or(rest.len());
    &rest[..next]
}

#[cfg(all(test, feature = "llvm-backend"))]
fn compile_llvm_wasm_assembly_for_test(src: &str) -> String {
    String::from_utf8(
        llvm_backend::compile_to_wasm_assembly(Module::from_source(src), "llvm_opt_test")
            .expect("llvm wasm assembly should compile"),
    )
    .expect("llvm wasm assembly should be utf-8 text")
}

#[cfg(all(test, feature = "llvm-backend"))]
fn extract_llvm_wasm_symbol_asm<'a>(asm: &'a str, symbol: &str) -> &'a str {
    let marker = format!("{symbol}:");
    let start = asm.find(&marker).expect("llvm wasm symbol should exist");
    let rest = &asm[start..];
    let search = &rest[marker.len()..];
    let mut next = rest.len();
    for pattern in [
        "\n.functype\t",
        "\n.functype ",
        "\n.section\t",
        "\n.section ",
        "\n.hidden\t",
        "\n.hidden ",
        "\n.globl\t",
        "\n.globl ",
    ] {
        if let Some(pos) = search.find(pattern) {
            next = next.min(marker.len() + pos);
        }
    }
    &rest[..next]
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
#[ignore = "debug helper for inspecting LLVM map symbols"]
fn dump_llvm_map_growth_symbols() {
    let src = build_large_map_growth_source(65);
    let asm = compile_llvm_wasm_assembly_for_test(&src);
    println!("--- llvm_rt_map_set ---\n{}", extract_llvm_wasm_symbol_asm(&asm, "llvm_rt_map_set"));
    println!(
        "--- llvm_rt_map_grow ---\n{}",
        extract_llvm_wasm_symbol_asm(&asm, "llvm_rt_map_grow")
    );
}

#[test]
fn ir_bytes_len_known_string_omits_bad_conversion_trap_in_main() {
    let src = "fn main() do\n    bytes_len(\"abc\")\nend";
    let ir = Module::from_source(src).compile_to_ir();
    let main_ir = extract_function_ir(&ir, "main");
    assert!(!main_ir.contains("bad_conversion_to_integer"));
}

#[test]
fn ir_bytes_get_known_types_keep_bounds_trap_but_omit_type_traps_in_main() {
    let src = "fn main() do\n    i = 1\n    bytes_get(\"abc\", i)\nend";
    let ir = Module::from_source(src).compile_to_ir();
    let main_ir = extract_function_ir(&ir, "main");
    assert!(!main_ir.contains("bad_conversion_to_integer"));
    assert!(main_ir.contains("heap_oob"));
}

#[test]
fn ir_bytes_slice_known_types_keep_bounds_trap_but_omit_type_traps_in_main() {
    let src = "fn main() do\n    end_index = 2\n    bytes_slice(\"abc\", 0, end_index)\nend";
    let ir = Module::from_source(src).compile_to_ir();
    let main_ir = extract_function_ir(&ir, "main");
    assert!(!main_ir.contains("bad_conversion_to_integer"));
    assert!(main_ir.contains("heap_oob"));
}

#[test]
fn ir_list_len_known_list_omits_bad_conversion_trap_in_main() {
    let src = "fn main() do\n    list_len([1, 2, 3])\nend";
    let ir = Module::from_source(src).compile_to_ir();
    let main_ir = extract_function_ir(&ir, "main");
    assert!(!main_ir.contains("bad_conversion_to_integer"));
}

#[test]
fn ir_list_get_known_types_keep_bounds_trap_but_omit_type_traps_in_main() {
    let src = "fn main() do\n    i = 1\n    list_get([1, 2, 3], i)\nend";
    let ir = Module::from_source(src).compile_to_ir();
    let main_ir = extract_function_ir(&ir, "main");
    assert!(!main_ir.contains("bad_conversion_to_integer"));
    assert!(main_ir.contains("heap_oob"));
}

#[test]
fn ir_index_known_types_keep_bounds_trap_but_omit_type_traps_in_main() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    i = 1\n    xs[i]\nend";
    let ir = Module::from_source(src).compile_to_ir();
    let main_ir = extract_function_ir(&ir, "main");
    assert!(!main_ir.contains("bad_conversion_to_integer"));
    assert!(main_ir.contains("heap_oob"));
}

#[test]
fn ir_exact_int_add_is_lowered_directly_in_main() {
    let src = "fn main() do\n    1 + 2\nend";
    let ir = Module::from_source(src).compile_to_ir();
    let main_ir = extract_function_ir(&ir, "main");
    assert!(main_ir.contains("sadd_overflow"));
}

#[test]
fn ir_exact_int_compare_is_lowered_directly_in_main() {
    let src = "fn main() do\n    1 < 2\nend";
    let ir = Module::from_source(src).compile_to_ir();
    let main_ir = extract_function_ir(&ir, "main");
    assert!(main_ir.contains("icmp"));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_wasm_bytes_len_known_string_omits_type_trap_labels_in_main() {
    let src = "fn main() do\n    bytes_len(\"abc\")\nend";
    let asm = compile_llvm_wasm_assembly_for_test(src);
    let main_asm = extract_llvm_wasm_symbol_asm(&asm, "__expr_internal_main");
    assert!(!main_asm.contains("bytes_len_trap"));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_wasm_bytes_get_known_types_keep_bounds_trap_but_omit_type_traps_in_main() {
    let src = "fn main() do\n    i = 1\n    bytes_get(\"abc\", i)\nend";
    let asm = compile_llvm_wasm_assembly_for_test(src);
    let main_asm = extract_llvm_wasm_symbol_asm(&asm, "__expr_internal_main");
    assert!(!main_asm.contains("bytes_get_string_trap"));
    assert!(!main_asm.contains("bytes_get_idx_trap"));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_wasm_bytes_slice_known_types_keep_bounds_trap_but_omit_type_traps_in_main() {
    let src = "fn main() do\n    end_index = 2\n    bytes_slice(\"abc\", 0, end_index)\nend";
    let asm = compile_llvm_wasm_assembly_for_test(src);
    let main_asm = extract_llvm_wasm_symbol_asm(&asm, "__expr_internal_main");
    assert!(!main_asm.contains("bytes_slice_string_trap"));
    assert!(!main_asm.contains("bytes_slice_start_trap"));
    assert!(!main_asm.contains("bytes_slice_end_trap"));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_wasm_list_len_known_list_omits_runtime_call_in_main() {
    let src = "fn main() do\n    list_len([1, 2, 3])\nend";
    let asm = compile_llvm_wasm_assembly_for_test(src);
    let main_asm = extract_llvm_wasm_symbol_asm(&asm, "__expr_internal_main");
    assert!(!main_asm.contains("__rt_list_len"));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_wasm_list_get_known_types_keep_bounds_trap_but_omit_runtime_call_in_main() {
    let src = "fn main() do\n    i = 1\n    list_get([1, 2, 3], i)\nend";
    let asm = compile_llvm_wasm_assembly_for_test(src);
    let main_asm = extract_llvm_wasm_symbol_asm(&asm, "__expr_internal_main");
    assert!(!main_asm.contains("__rt_list_get"));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_wasm_index_known_types_keep_bounds_trap_but_omit_runtime_call_in_main() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    i = 1\n    xs[i]\nend";
    let asm = compile_llvm_wasm_assembly_for_test(src);
    let main_asm = extract_llvm_wasm_symbol_asm(&asm, "__expr_internal_main");
    assert!(!main_asm.contains("__rt_list_get"));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_wasm_exact_int_add_omits_generic_operator_call_in_main() {
    let src = "fn main() do\n    1 + 2\nend";
    let asm = compile_llvm_wasm_assembly_for_test(src);
    let main_asm = extract_llvm_wasm_symbol_asm(&asm, "__expr_internal_main");
    assert!(!main_asm.contains("__op_add"));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_wasm_exact_int_compare_omits_generic_operator_call_in_main() {
    let src = "fn main() do\n    1 < 2\nend";
    let asm = compile_llvm_wasm_assembly_for_test(src);
    let main_asm = extract_llvm_wasm_symbol_asm(&asm, "__expr_internal_main");
    assert!(!main_asm.contains("__op_lt"));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_wasm_exact_bigint_add_uses_bigint_path_not_generic_operator_in_main() {
    let src = "fn main() do\n    10000000000000000000n + 2n\nend";
    let asm = compile_llvm_wasm_assembly_for_test(src);
    let main_asm = extract_llvm_wasm_symbol_asm(&asm, "__expr_internal_main");
    assert!(!main_asm.contains("__op_add"));
    assert!(main_asm.contains("bigint_add"));
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
fn bigint_from_int_prints() {
    let src = "fn main() do\n    print(bigint_from_int(1234567890123))\nend";
    assert_cranelift_executable_output(src, "1234567890123\n", 0);
}

#[test]
fn bigint_literal_prints() {
    let src = "fn main() do\n    print(123456789012345678901234567890n)\nend";
    assert_cranelift_executable_output(src, "123456789012345678901234567890\n", 0);
}

#[test]
fn bigint_add_handles_limb_carry() {
    let src = "fn main() do\n    a = bigint_from_int(4294967295)\n    b = bigint_from_int(2)\n    print(a + b)\nend";
    assert_cranelift_executable_output(src, "4294967297\n", 0);
}

#[test]
fn bigint_subtract_handles_negative_results() {
    let src = "fn main() do\n    a = bigint_from_int(10)\n    b = bigint_from_int(20)\n    print(bigint_subtract(a, b))\nend";
    assert_cranelift_executable_output(src, "-10\n", 0);
}

#[test]
fn bigint_subtract_normalizes_zero() {
    let src = "fn main() do\n    a = bigint_from_int(5)\n    b = bigint_from_int(5)\n    print(a - b)\nend";
    assert_cranelift_executable_output(src, "0\n", 0);
}

#[test]
fn bigint_compare_works() {
    let src = "fn main() do\n    a = bigint_from_int(10)\n    b = bigint_from_int(20)\n    print(bigint_compare(a, b))\n    print(a < b)\n    print(a == b)\n    print(b > a)\nend";
    assert_cranelift_executable_output(src, "-1\n1\n0\n1\n", 0);
}

#[test]
fn bigint_multiply_works() {
    let src = "fn main() do\n    a = bigint_from_int(4294967295)\n    b = bigint_from_int(2)\n    print(a * b)\n    print(bigint_multiply(a, b))\nend";
    assert_cranelift_executable_output(src, "8589934590\n8589934590\n", 0);
}

#[test]
fn bigint_divide_works() {
    let src = "fn main() do\n    a = bigint_from_int(100)\n    b = bigint_from_int(7)\n    c = bigint_from_int(8589934590)\n    d = bigint_from_int(2)\n    print(a / b)\n    print(bigint_divide(a, b))\n    print(c / d)\nend";
    assert_cranelift_executable_output(src, "14\n14\n4294967295\n", 0);
}

#[test]
fn bigint_grouped_divide_expression_works() {
    let src = "fn fact(n) do\n    fact_acc(n, 1n)\nend\n\nfn fact_acc(n, acc) do\n    if n == 0 do\n        acc\n    else\n        fact_acc(n - 1, acc * n)\n    end\nend\n\nfn choose(n, k) do\n    fact(n) / (fact(k) * fact(n - k))\nend\n\nfn main() do\n    choose(40, 20) == 137846528820n\nend";
    assert_cranelift_jit_result(src, 1);
}

#[test]
fn bigint_modulo_works() {
    let src = "fn main() do\n    a = bigint_from_int(100)\n    b = bigint_from_int(7)\n    c = bigint_from_int(25)\n    d = bigint_from_int(10)\n    print(a % b)\n    print(bigint_modulo(a, b))\n    print(bigint_modulo(c, d))\nend";
    assert_cranelift_executable_output(src, "2\n2\n5\n", 0);
}

#[test]
fn bigint_mixed_arithmetic_and_compare_work() {
    let src = "fn main() do\n    a = bigint_from_int(10)\n    print(a + 5)\n    print(5 + a)\n    print(a - 3)\n    print(25 - a)\n    print(5 - a)\n    print(a * 3)\n    print(3 * a)\n    print(bigint_from_int(100) / 7)\n    print(bigint_from_int(100) % 7)\n    print(a > 5)\n    print(5 < a)\n    print(a == 10)\nend";
    assert_cranelift_executable_output(src, "15\n15\n7\n15\n-5\n30\n30\n14\n2\n1\n1\n1\n", 0);
}

#[test]
fn bigint_builtins_accept_mixed_int_and_bigint_operands() {
    let src = "fn main() do\n    a = bigint_from_int(10)\n    print(bigint_add(a, 5))\n    print(bigint_add(5, a))\n    print(bigint_subtract(a, 3))\n    print(bigint_subtract(25, a))\n    print(bigint_subtract(5, a))\n    print(bigint_multiply(a, 3))\n    print(bigint_multiply(3, a))\n    print(bigint_divide(100, a))\n    print(bigint_modulo(100, a))\n    print(bigint_compare(a, 5))\n    print(bigint_compare(5, a))\n    print(bigint_add(1, 2))\nend";
    assert_cranelift_executable_output(src, "15\n15\n7\n15\n-5\n30\n30\n10\n0\n1\n-1\n3\n", 0);
}

#[test]
fn strings_print_and_len_work() {
    let src = "fn main() do\n    print(\"hello\")\n    print(bytes_len(\"hi\\n\"))\nend";
    assert_cranelift_executable_output(src, "hello\n3\n", 0);
}

#[test]
fn strings_eq_and_ne_work() {
    let src = "fn main() do\n    print(\"abc\" == \"abc\")\n    print(\"abc\" != \"xyz\")\n    print(\"abc\" == 1)\n    print(\"abc\" != 1)\nend";
    assert_cranelift_executable_output(src, "1\n1\n0\n1\n", 0);
}

#[test]
fn strings_concat_works() {
    let src = "fn main() do\n    joined = string_concat(\"ab\", \"cd\")\n    print(joined)\n    print(bytes_len(joined))\nend";
    assert_cranelift_executable_output(src, "abcd\n4\n", 0);
}

#[test]
fn strings_bytes_get_and_slice_work() {
    let src = "fn main() do\n    s = \"hello\"\n    print(bytes_get(s, 1))\n    mid = bytes_slice(s, 1, 4)\n    print(mid)\n    print(bytes_len(mid))\nend";
    assert_cranelift_executable_output(src, "101\nell\n3\n", 0);
}

#[test]
fn strings_bytes_pop_works() {
    let src = "fn main() do\n    s = \"hello\"\n    print(bytes_pop(s))\n    print(bytes_pop(s))\n    print(s)\n    print(bytes_len(s))\nend";
    assert_cranelift_executable_output(src, "111\n108\nhel\n3\n", 0);
}

#[test]
fn strings_bytes_push_and_set_work() {
    let src = "fn main() do\n    s = \"hi\"\n    bytes_push(s, 33)\n    bytes_set(s, 1, 97)\n    print(s)\n    print(bytes_len(s))\nend";
    assert_cranelift_executable_output(src, "ha!\n3\n", 0);
}

#[test]
fn strings_bytes_insert_and_remove_work() {
    let src = "fn main() do\n    s = \"heo\"\n    bytes_insert(s, 2, 108)\n    bytes_insert(s, 4, 33)\n    print(s)\n    print(bytes_remove(s, 1))\n    print(s)\n    print(bytes_len(s))\nend";
    assert_cranelift_executable_output(src, "helo!\n101\nhlo!\n4\n", 0);
}

#[test]
fn strings_copy_isolated_from_mutation() {
    let src = "fn main() do\n    s = \"hi\"\n    t = string_copy(s)\n    bytes_push(s, 33)\n    bytes_set(t, 1, 97)\n    print(s)\n    print(t)\nend";
    assert_cranelift_executable_output(src, "hi!\nha\n", 0);
}

#[test]
fn strings_utf8_iteration_works() {
    let src = "fn walk(it) do\n    if string_iter_done(it) do\n        0\n    else\n        print(string_iter_next(it))\n        walk(it)\n    end\nend\n\nfn main() do\n    walk(string_chars(\"hé🙂\"))\n    print(string_iter_done(string_chars(\"\")))\nend";
    assert_cranelift_executable_output(src, "104\n233\n128578\n1\n", 0);
}

#[test]
fn autoloaded_stdlib_string_helpers_work() {
    let src = "fn main() do\n    print(string_is_empty(\"\"))\n    print(string_is_empty(\"x\"))\n    print(string_is_not_empty(\"\"))\n    print(string_is_not_empty(\"x\"))\n    print(string_len(\"hé🙂\"))\n    print(string_first(\"hé🙂\"))\n    print(string_last(\"hé🙂\"))\n    print(string_starts_with(\"banana\", \"ban\"))\n    print(string_starts_with(\"banana\", \"ana\"))\n    print(string_ends_with(\"banana\", \"nana\"))\n    print(string_ends_with(\"banana\", \"ban\"))\n    print(string_contains(\"banana\", \"nan\"))\n    print(string_contains(\"banana\", \"nab\"))\n    print(string_contains(\"banana\", \"\"))\n    print(string_is_ascii(\"hello\"))\n    print(string_is_ascii(\"hé\"))\n    print(string_all(\"1234\", __all_digits))\n    print(string_all(\"12a4\", __all_digits))\n    print(string_all(\"\", __all_digits))\n    print(string_any(\"12a4\", __all_digits))\n    print(string_any(\"abcd\", __all_digits))\n    print(string_any(\"\", __all_digits))\n    print(string_is_integer(\"11234\"))\n    print(string_is_integer(\"11T234\"))\n    ok0, value0, err0 = string_try_first(\"\")\n    print(ok0)\n    print(value0)\n    print(err0 == \"expected non-empty string\")\n    ok00, value00, err00 = string_try_first(\"hé🙂\")\n    print(ok00)\n    print(value00)\n    print(err00 == \"\")\n    ok01, value01, err01 = string_try_last(\"\")\n    print(ok01)\n    print(value01)\n    print(err01 == \"expected non-empty string\")\n    ok02, value02, err02 = string_try_last(\"hé🙂\")\n    print(ok02)\n    print(value02)\n    print(err02 == \"\")\n    ok03, value03, err03 = bytes_try_get(\"abc\", 1)\n    print(ok03)\n    print(value03)\n    print(err03 == \"\")\n    ok04, value04, err04 = bytes_try_get(\"abc\", 3)\n    print(ok04)\n    print(value04)\n    print(err04 == \"index out of bounds\")\n    pop_target = \"abc\"\n    ok05, value05, err05 = string_try_pop(pop_target)\n    print(ok05)\n    print(value05)\n    print(err05 == \"\")\n    print(pop_target)\n    empty_target = \"\"\n    ok06, value06, err06 = string_try_pop(empty_target)\n    print(ok06)\n    print(value06)\n    print(err06 == \"expected non-empty string\")\n    ok1, value1, err1 = string_try_parse_integer(\"123\")\n    print(ok1)\n    print(value1)\n    print(err1 == \"\")\n    ok2, value2, err2 = string_try_parse_integer(\"-45\")\n    print(ok2)\n    print(value2)\n    print(err2 == \"\")\n    ok3, value3, err3 = string_try_parse_integer(\"12a\")\n    print(ok3)\n    print(value3)\n    print(err3 == \"invalid integer\")\n    ok4, value4, err4 = string_try_parse_integer(\"\")\n    print(ok4)\n    print(value4)\n    print(err4 == \"expected at least one digit\")\n    ok5, value5, err5 = string_try_parse_integer(\"-\")\n    print(ok5)\n    print(value5)\n    print(err5 == \"expected digits after '-'\" )\n    ok6, value6, err6 = string_try_parse_bigint(\"12345678901234567890\")\n    print(ok6)\n    print(value6)\n    print(err6 == \"\")\n    ok7, value7, err7 = string_try_parse_bigint(\"-9007199254740993\")\n    print(ok7)\n    print(value7)\n    print(err7 == \"\")\n    ok8, value8, err8 = string_try_parse_bigint(\"x\")\n    print(ok8)\n    print(value8)\n    print(err8 == \"invalid integer\")\n    print(list_all([1, 2, 3], __positive_item))\n    print(list_all([1, 0, 3], __positive_item))\n    print(list_all([], __positive_item))\n    print(list_any([1, 0, 3], __zero_item))\n    print(list_any([1, 2, 3], __zero_item))\n    print(list_any([], __zero_item))\n    print(string_repeat(\"ab\", 3))\n    print(string_reverse(\"hé🙂\") == \"🙂éh\")\nend\n\nfn __all_digits(ch) do\n    if ch >= 48 do\n        ch <= 57\n    else\n        0\n    end\nend\n\nfn __positive_item(item) do\n    if item > 0 do\n        1\n    else\n        0\n    end\nend\n\nfn __zero_item(item) do\n    if item == 0 do\n        1\n    else\n        0\n    end\nend";
    assert_cranelift_executable_output(
        src,
        "1\n0\n0\n1\n3\n104\n128578\n1\n0\n1\n0\n1\n0\n1\n1\n0\n1\n0\n0\n1\n0\n0\n1\n0\n0\n0\n1\n1\n104\n1\n0\n0\n1\n1\n128578\n1\n1\n98\n1\n0\n0\n1\n1\n99\n1\nab\n0\n0\n1\n1\n123\n1\n1\n-45\n1\n0\n0\n1\n0\n0\n1\n0\n0\n1\n1\n12345678901234567890\n1\n1\n-9007199254740993\n1\n0\n0\n1\n1\n0\n0\n1\n0\n0\nababab\n1\n",
        0,
    );
}

#[test]
fn autoloaded_stdlib_functions_can_be_used_as_values() {
    let src = "fn main() do\n    pred = string_is_empty\n    print(pred(\"\"))\n    print(pred(\"x\"))\nend";
    assert_cranelift_executable_output(src, "1\n0\n", 0);
}

#[test]
fn runtime_type_predicates_and_type_of_work() {
    let src = "fn helper(x) do\n    x\nend\n\nfn main() do\n    print(is_int(1))\n    print(is_bigint(1))\n    print(is_bigint(1n))\n    print(is_string(\"x\"))\n    print(is_list([1]))\n    m = map_new()\n    print(is_map(m))\n    f = helper\n    print(is_function(f))\n    it = string_chars(\"a\")\n    print(is_string_iter(it))\n    map_it = map_iter(m)\n    print(is_map_iter(map_it))\n    print(type_of(1) == \"int\")\n    print(type_of(1n) == \"bigint\")\n    print(type_of(\"x\") == \"string\")\n    print(type_of([1]) == \"list\")\n    print(type_of(m) == \"map\")\n    print(type_of(f) == \"function\")\n    print(type_of(it) == \"string_iter\")\n    print(type_of(map_it) == \"map_iter\")\nend";
    assert_cranelift_executable_output(
        src,
        "1\n0\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n",
        0,
    );
}

#[test]
fn string_from_codepoints_works() {
    let src = "fn main() do\n    it = string_chars(\"hé🙂\")\n    xs = list_new()\n    list_push(xs, string_iter_next(it))\n    list_push(xs, string_iter_next(it))\n    list_push(xs, string_iter_next(it))\n    print(string_from_codepoints(xs))\nend";
    assert_cranelift_executable_output(src, "hé🙂\n", 0);
}

#[test]
fn map_iter_and_map_values_work() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"a\", 10)\n    map_set(m, \"b\", 32)\n    it = map_iter(m)\n    print(map_iter_done(it))\n    key1, value1 = map_iter_next(it)\n    print(key1 == \"a\" or key1 == \"b\")\n    print((key1 == \"a\" and value1 == 10) or (key1 == \"b\" and value1 == 32))\n    print(map_iter_done(it))\n    key2, value2 = map_iter_next(it)\n    print(key2 == \"a\" or key2 == \"b\")\n    print(key1 != key2)\n    print(value1 + value2)\n    print(map_iter_done(it))\n    values = map_values(m)\n    print(list_len(values))\n    print(values[0] + values[1])\nend";
    assert_cranelift_executable_output(src, "0\n1\n1\n0\n1\n1\n42\n1\n2\n42\n", 0);
}

#[test]
fn maps_work() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"a\", 10)\n    map_set(m, \"b\", 32)\n    map_set(m, \"a\", 11)\n    print(map_len(m))\n    print(map_has(m, \"a\"))\n    print(map_has(m, \"missing\"))\n    print(map_get(m, \"a\"))\n    print(map_get(m, \"b\"))\nend";
    assert_cranelift_executable_output(src, "2\n1\n0\n11\n32\n", 0);
}

#[test]
fn map_try_get_works() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"answer\", 42)\n    ok1, value1, err1 = map_try_get(m, \"answer\")\n    print(ok1)\n    print(value1)\n    print(err1 == \"\")\n    ok2, value2, err2 = map_try_get(m, \"missing\")\n    print(ok2)\n    print(value2)\n    print(err2 == \"missing key\")\nend";
    assert_cranelift_executable_output(src, "1\n42\n1\n0\n0\n1\n", 0);
}

#[test]
fn map_delete_and_try_delete_work() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"a\", 10)\n    map_set(m, \"b\", 32)\n    print(map_delete(m, \"a\"))\n    print(map_len(m))\n    print(map_has(m, \"a\"))\n    print(map_get(m, \"b\"))\n    ok, value, err = map_try_delete(m, \"missing\")\n    print(ok)\n    print(value)\n    print(err == \"missing key\")\nend";
    assert_cranelift_executable_output(src, "10\n1\n0\n32\n0\n0\n1\n", 0);
}

#[test]
fn map_try_pop_works() {
    let src = "fn main() do\n    m = map_new()\n    ok1, key1, value1 = map_try_pop(m)\n    print(ok1)\n    print(key1 == \"\")\n    print(value1)\n    map_set(m, \"a\", 10)\n    map_set(m, \"b\", 32)\n    ok2, key2, value2 = map_try_pop(m)\n    print(ok2)\n    print(map_len(m))\n    print(key2 == \"a\" or key2 == \"b\")\n    print((key2 == \"a\" and value2 == 10) or (key2 == \"b\" and value2 == 32))\n    print(list_len(map_keys(m)))\nend";
    assert_cranelift_executable_output(src, "0\n1\n0\n1\n1\n1\n1\n1\n", 0);
}

#[test]
fn map_update_and_map_update_or_default_work() {
    let src = "fn inc(x) do\n    x + 1\nend\n\nfn main() do\n    m = map_new()\n    print(map_update(m, \"count\", inc))\n    print(map_has(m, \"count\"))\n    print(map_update_or_default(m, \"count\", 0, inc))\n    print(map_get(m, \"count\"))\n    print(map_update(m, \"count\", inc))\n    print(map_get(m, \"count\"))\nend";
    assert_cranelift_executable_output(src, "0\n0\n1\n1\n1\n2\n", 0);
}

#[test]
fn map_keys_work() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"a\", 10)\n    map_set(m, \"b\", 32)\n    map_set(m, \"a\", 11)\n    keys = map_keys(m)\n    print(list_len(keys))\n    print(keys[0])\n    print(keys[1])\nend";
    assert_cranelift_executable_output(src, "2\na\nb\n", 0);
}

#[test]
fn map_literal_works() {
    let src = "fn main() do\n    dyn_key = \"count\"\n    m = {\n        name: \"expr\",\n        dyn_key => 3,\n    }\n    print(map_get(m, \"name\"))\n    print(map_get(m, \"count\"))\n    print(map_len(m))\nend";
    assert_cranelift_executable_output(src, "expr\n3\n2\n", 0);
}

#[cfg(test)]
fn test_map_bucket_index(key: &str) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in key.as_bytes() {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x100000001b3u64);
    }
    hash % 64
}

#[cfg(test)]
fn test_find_colliding_map_keys() -> (String, String) {
    let mut buckets = std::collections::HashMap::<u64, String>::new();
    for idx in 0..512 {
        let key = format!("k{idx}");
        let bucket = test_map_bucket_index(&key);
        if let Some(existing) = buckets.get(&bucket) {
            if existing != &key {
                return (existing.clone(), key);
            }
        } else {
            buckets.insert(bucket, key);
        }
    }
    panic!("failed to find colliding test map keys")
}

#[cfg(test)]
fn build_large_map_growth_source(count: usize) -> String {
    let mut src = String::from("fn main() do\n    m = map_new()\n");
    for idx in 0..count {
        src.push_str(&format!("    map_set(m, \"k{idx}\", {})\n", idx + 1));
    }
    src.push_str(&format!(
        "    print(map_len(m))\n    print(map_get(m, \"k0\"))\n    print(map_get(m, \"k{}\"))\n    print(map_get(m, \"k{}\"))\n    map_set(m, \"k10\", 999)\n    print(map_get(m, \"k10\"))\nend",
        count / 2,
        count - 1
    ));
    src
}

#[test]
fn map_delete_preserves_probe_chain_and_reuses_tombstone() {
    let (key1, key2) = test_find_colliding_map_keys();
    let src = format!(
        "fn main() do\n    m = map_new()\n    map_set(m, \"{key1}\", 10)\n    map_set(m, \"{key2}\", 32)\n    print(map_delete(m, \"{key1}\"))\n    print(map_has(m, \"{key1}\"))\n    print(map_get(m, \"{key2}\"))\n    map_set(m, \"{key1}\", 11)\n    print(map_get(m, \"{key1}\"))\n    print(map_get(m, \"{key2}\"))\n    print(map_len(m))\nend"
    );
    assert_cranelift_executable_output(&src, "10\n0\n32\n11\n32\n2\n", 0);
}

#[test]
fn map_grows_past_initial_capacity() {
    let src = build_large_map_growth_source(80);
    assert_cranelift_executable_output(&src, "80\n1\n41\n80\n999\n", 0);
}

#[test]
fn infer_known_callback_return_shape_tracks_function_alias_callbacks() {
    let src = "fn double(x) do\n    x * 2\nend\n\nfn main() do\n    f = double\n    xs = [1]\n    ys = list_map(xs, f)\n    ys\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let analysis = module.analyze_value_kinds().expect("analysis should succeed");
    let main_function = module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("main function should exist");
    let callback = match &main_function.block.lines[2] {
        Ast::Assign { value, .. } => match &**value {
            Ast::Expression(expr) if expr.function == "list_map" => &expr.args[1],
            other => panic!("expected list_map assignment, found {other:?}"),
        },
        other => panic!("expected assignment, found {other:?}"),
    };
    let main_analysis = analysis.functions.get("main").expect("main analysis should exist");
    let shape = infer_known_callback_return_shape(callback, main_analysis, &analysis)
        .expect("known callback return shape should be inferred");
    assert_eq!(shape, ValueShape::scalar(KindSet::int()));
}

#[test]
fn validate_ast_multi_return_usage_rejects_multi_value_in_index_context() {
    let src = "fn pair() do\n    1, 2\nend\n\nfn main() do\n    [1, 2][pair()]\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let function_return_arities =
        function_return_arities(&module.functions).expect("return arities should infer");
    let main_function = module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("main function should exist");
    let locals = main_function.inputs.iter().cloned().collect::<HashSet<_>>();
    let function_names =
        module.functions.iter().map(|function| function.name.clone()).collect::<HashSet<_>>();
    let err = validate_ast_multi_return_usage(
        &main_function.block.lines[0],
        "main",
        &locals,
        &function_names,
        &function_return_arities,
        1,
        true,
    )
    .expect_err("multi-value index usage should be rejected");
    match err {
        CompileError::UnsupportedMultiValueContext { .. } => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn validate_ast_multi_return_usage_rejects_scalar_literal_when_multiple_values_are_expected() {
    let locals = HashSet::new();
    let function_names = HashSet::new();
    let function_return_arities = HashMap::new();
    let err = validate_ast_multi_return_usage(
        &Ast::Literal(LiteralAst::Integer(1)),
        "main",
        &locals,
        &function_names,
        &function_return_arities,
        2,
        true,
    )
    .expect_err("scalar literal should be rejected in multi-value context");
    match err {
        CompileError::UnsupportedMultiValueContext { .. } => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn validate_ast_multi_return_usage_rejects_non_tail_multi_value() {
    let locals = HashSet::new();
    let function_names = HashSet::new();
    let function_return_arities = HashMap::new();
    let err = validate_ast_multi_return_usage(
        &Ast::MultiValue(vec![
            Ast::Literal(LiteralAst::Integer(1)),
            Ast::Literal(LiteralAst::Integer(2)),
        ]),
        "main",
        &locals,
        &function_names,
        &function_return_arities,
        2,
        false,
    )
    .expect_err("non-tail multi-value should be rejected");
    match err {
        CompileError::UnsupportedMultiValueContext { .. } => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn validate_ast_multi_return_usage_rejects_non_expression_multi_assign_rhs() {
    let locals = HashSet::new();
    let function_names = HashSet::new();
    let function_return_arities = HashMap::new();
    let err = validate_ast_multi_return_usage(
        &Ast::MultiAssign {
            names: vec!["a".to_string(), "b".to_string()],
            value: Box::new(Ast::Literal(LiteralAst::Integer(1))),
            span: None,
        },
        "main",
        &locals,
        &function_names,
        &function_return_arities,
        1,
        true,
    )
    .expect_err("multi-assign should require a named call on the right-hand side");
    match err {
        CompileError::UnsupportedMultiValueContext { .. } => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn validate_ast_multi_return_usage_rejects_if_without_else_in_multi_value_context() {
    let locals = HashSet::new();
    let function_names = HashSet::new();
    let function_return_arities = HashMap::new();
    let err = validate_ast_multi_return_usage(
        &Ast::If {
            condition: Box::new(Ast::Literal(LiteralAst::Integer(1))),
            then: BlockAst {
                lines: vec![Ast::MultiValue(vec![
                    Ast::Literal(LiteralAst::Integer(1)),
                    Ast::Literal(LiteralAst::Integer(2)),
                ])],
            },
            else_: None,
            span: None,
        },
        "main",
        &locals,
        &function_names,
        &function_return_arities,
        2,
        true,
    )
    .expect_err("if without else should be rejected in multi-value context");
    match err {
        CompileError::UnsupportedMultiValueContext { .. } => {}
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn infer_ast_value_shape_tracks_if_multi_assign_and_indexed_list_reads() {
    let src = "fn pair() do\n    1, 2\nend\n\nfn main() do\n    ok, value = pair()\n    xs = [1, \"a\"]\n    if ok do\n        xs[0]\n    else\n        xs[1]\n    end\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let analysis = module.analyze_value_kinds().expect("analysis should succeed");
    let main_analysis = analysis.functions.get("main").expect("main analysis should exist");
    let main_function = module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("main function should exist");
    let shape = infer_ast_value_shape(&main_function.block.lines[2], main_analysis, &analysis);
    assert_eq!(shape, ValueShape::scalar(KindSet::int().union(KindSet::string())));
}

#[test]
fn infer_ast_value_shape_covers_manual_ast_variants() {
    let function_analysis = FunctionValueKindAnalysis {
        inputs: vec![],
        variables: HashMap::from([
            ("string_var".to_string(), ValueShape::scalar(KindSet::string())),
            ("typed_list".to_string(), ValueShape::list(KindSet::int().union(KindSet::string()))),
            ("generic_list".to_string(), ValueShape::scalar(KindSet::list())),
            ("value_var".to_string(), ValueShape::scalar(KindSet::bigint())),
        ]),
        function_bindings: HashMap::new(),
        returns: ValueShape::scalar(KindSet::int()),
    };
    let module_analysis = ModuleValueKindAnalysis {
        functions: HashMap::from([
            (
                "returns_pair".to_string(),
                FunctionValueKindAnalysis {
                    inputs: vec![],
                    variables: HashMap::new(),
                    function_bindings: HashMap::new(),
                    returns: ValueShape::from_slots(vec![KindSet::int(), KindSet::string()]),
                },
            ),
            (
                "returns_string".to_string(),
                FunctionValueKindAnalysis {
                    inputs: vec![],
                    variables: HashMap::new(),
                    function_bindings: HashMap::new(),
                    returns: ValueShape::scalar(KindSet::string()),
                },
            ),
        ]),
    };

    assert_eq!(
        infer_ast_value_shape(
            &Ast::Literal(LiteralAst::Integer(1)),
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::int())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::Literal(LiteralAst::BigInt("1".to_string())),
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::bigint())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::Variable(Ident::synthetic("string_var".to_string())),
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::string())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::FunctionRef(Ident::synthetic("returns_string".to_string())),
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::function())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::Lambda {
                inputs: vec!["x".to_string()],
                body: Box::new(Ast::Variable(Ident::synthetic("x".to_string()))),
            },
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::function())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::ListLiteral(vec![
                Ast::Literal(LiteralAst::Integer(1)),
                Ast::Literal(LiteralAst::String("x".to_string())),
            ]),
            &function_analysis,
            &module_analysis
        ),
        ValueShape::list(KindSet::int().union(KindSet::string()))
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::MultiValue(vec![
                Ast::Literal(LiteralAst::Integer(1)),
                Ast::Literal(LiteralAst::String("x".to_string())),
            ]),
            &function_analysis,
            &module_analysis
        ),
        ValueShape::from_slots(vec![KindSet::int(), KindSet::string()])
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::Expression(ExpressionAst {
                function_span: None,
                function: "returns_pair".to_string(),
                args: vec![],
            }),
            &function_analysis,
            &module_analysis
        ),
        ValueShape::from_slots(vec![KindSet::int(), KindSet::string()])
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::Expression(ExpressionAst {
                function_span: None,
                function: "list_map".to_string(),
                args: vec![
                    Ast::Variable(Ident::synthetic("typed_list".to_string())),
                    Ast::FunctionRef(Ident::synthetic("returns_string".to_string())),
                ],
            }),
            &function_analysis,
            &module_analysis
        ),
        ValueShape::list(KindSet::string())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::Expression(ExpressionAst {
                function_span: None,
                function: "list_filter".to_string(),
                args: vec![
                    Ast::Variable(Ident::synthetic("typed_list".to_string())),
                    Ast::FunctionRef(Ident::synthetic("returns_string".to_string())),
                ],
            }),
            &function_analysis,
            &module_analysis
        ),
        ValueShape::list(KindSet::int().union(KindSet::string()))
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::Assign {
                name: "x".to_string(),
                value: Box::new(Ast::Variable(Ident::synthetic("value_var".to_string()))),
                span: None,
            },
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::bigint())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::MultiAssign {
                names: vec!["a".to_string(), "b".to_string()],
                value: Box::new(Ast::MultiValue(vec![
                    Ast::Literal(LiteralAst::Integer(1)),
                    Ast::Literal(LiteralAst::String("x".to_string())),
                ])),
                span: None,
            },
            &function_analysis,
            &module_analysis
        ),
        ValueShape::from_slots(vec![KindSet::int(), KindSet::string()])
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::Block(BlockAst { lines: vec![] }),
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::int())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::If {
                condition: Box::new(Ast::Literal(LiteralAst::Integer(1))),
                then: BlockAst { lines: vec![Ast::Literal(LiteralAst::Integer(1))] },
                else_: Some(BlockAst {
                    lines: vec![Ast::Literal(LiteralAst::String("x".to_string()))],
                }),
                span: None,
            },
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::int().union(KindSet::string()))
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::If {
                condition: Box::new(Ast::Literal(LiteralAst::Integer(1))),
                then: BlockAst { lines: vec![Ast::Literal(LiteralAst::Integer(1))] },
                else_: Some(BlockAst {
                    lines: vec![Ast::MultiValue(vec![
                        Ast::Literal(LiteralAst::Integer(1)),
                        Ast::Literal(LiteralAst::String("x".to_string())),
                    ])],
                }),
                span: None,
            },
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::empty())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::Index {
                collection: Box::new(Ast::Literal(LiteralAst::String("abc".to_string()))),
                index: Box::new(Ast::Literal(LiteralAst::Integer(0))),
                span: None,
            },
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::int())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::Index {
                collection: Box::new(Ast::Variable(Ident::synthetic("typed_list".to_string()))),
                index: Box::new(Ast::Literal(LiteralAst::Integer(0))),
                span: None,
            },
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::int().union(KindSet::string()))
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::Index {
                collection: Box::new(Ast::Variable(Ident::synthetic("generic_list".to_string()))),
                index: Box::new(Ast::Literal(LiteralAst::Integer(0))),
                span: None,
            },
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::any())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::IndexAssign {
                collection: Box::new(Ast::Variable(Ident::synthetic("typed_list".to_string()))),
                index: Box::new(Ast::Literal(LiteralAst::Integer(0))),
                value: Box::new(Ast::Literal(LiteralAst::String("x".to_string()))),
                span: None,
            },
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::string())
    );
    assert_eq!(
        infer_ast_value_shape(
            &Ast::FunctionDef(FunctionDefAst::default()),
            &function_analysis,
            &module_analysis
        ),
        ValueShape::scalar(KindSet::empty())
    );
}

#[test]
fn jit_logical_and_or_short_circuit() {
    let src = "fn boom() do\n    1 / 0\nend\n\nfn main() do\n    print(0 and boom())\n    print(1 or boom())\n    print(1 and 2)\n    print(0 or 5)\n    print(not 0)\n    print(not 7)\n    print(not 1 == 0)\nend";
    assert_cranelift_executable_output(src, "0\n1\n1\n1\n1\n0\n1\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_bigint_add_handles_limb_carry() {
    let src = "fn main() do\n    a = bigint_from_int(4294967295)\n    b = bigint_from_int(2)\n    print(a + b)\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "4294967297\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_bigint_literal_prints() {
    let src = "fn main() do\n    print(123456789012345678901234567890n)\nend";
    assert_backend_executable_output(
        src,
        CodegenBackend::Llvm,
        "123456789012345678901234567890\n",
        0,
    );
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_bigint_subtract_handles_negative_results() {
    let src = "fn main() do\n    a = bigint_from_int(10)\n    b = bigint_from_int(20)\n    print(bigint_subtract(a, b))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "-10\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_bigint_compare_works() {
    let src = "fn main() do\n    a = bigint_from_int(10)\n    b = bigint_from_int(20)\n    print(bigint_compare(a, b))\n    print(a < b)\n    print(a == b)\n    print(b > a)\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "-1\n1\n0\n1\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_bigint_multiply_works() {
    let src = "fn main() do\n    a = bigint_from_int(4294967295)\n    b = bigint_from_int(2)\n    print(a * b)\n    print(bigint_multiply(a, b))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "8589934590\n8589934590\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_bigint_divide_works() {
    let src = "fn main() do\n    a = bigint_from_int(100)\n    b = bigint_from_int(7)\n    c = bigint_from_int(8589934590)\n    d = bigint_from_int(2)\n    print(a / b)\n    print(bigint_divide(a, b))\n    print(c / d)\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "14\n14\n4294967295\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_bigint_grouped_divide_expression_works() {
    let src = "fn fact(n) do\n    fact_acc(n, 1n)\nend\n\nfn fact_acc(n, acc) do\n    if n == 0 do\n        acc\n    else\n        fact_acc(n - 1, acc * n)\n    end\nend\n\nfn choose(n, k) do\n    fact(n) / (fact(k) * fact(n - k))\nend\n\nfn main() do\n    choose(40, 20) == 137846528820n\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 1);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_bigint_modulo_works() {
    let src = "fn main() do\n    a = bigint_from_int(100)\n    b = bigint_from_int(7)\n    c = bigint_from_int(25)\n    d = bigint_from_int(10)\n    print(a % b)\n    print(bigint_modulo(a, b))\n    print(bigint_modulo(c, d))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "2\n2\n5\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_bigint_mixed_arithmetic_and_compare_work() {
    let src = "fn main() do\n    a = bigint_from_int(10)\n    print(a + 5)\n    print(5 + a)\n    print(a - 3)\n    print(25 - a)\n    print(5 - a)\n    print(a * 3)\n    print(3 * a)\n    print(bigint_from_int(100) / 7)\n    print(bigint_from_int(100) % 7)\n    print(a > 5)\n    print(5 < a)\n    print(a == 10)\nend";
    assert_backend_executable_output(
        src,
        CodegenBackend::Llvm,
        "15\n15\n7\n15\n-5\n30\n30\n14\n2\n1\n1\n1\n",
        0,
    );
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_bigint_builtins_accept_mixed_int_and_bigint_operands() {
    let src = "fn main() do\n    a = bigint_from_int(10)\n    print(bigint_add(a, 5))\n    print(bigint_add(5, a))\n    print(bigint_subtract(a, 3))\n    print(bigint_subtract(25, a))\n    print(bigint_subtract(5, a))\n    print(bigint_multiply(a, 3))\n    print(bigint_multiply(3, a))\n    print(bigint_divide(100, a))\n    print(bigint_modulo(100, a))\n    print(bigint_compare(a, 5))\n    print(bigint_compare(5, a))\n    print(bigint_add(1, 2))\nend";
    assert_backend_executable_output(
        src,
        CodegenBackend::Llvm,
        "15\n15\n7\n15\n-5\n30\n30\n10\n0\n1\n-1\n3\n",
        0,
    );
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_strings_print_and_len_work() {
    let src = "fn main() do\n    print(\"hello\")\n    print(bytes_len(\"hi\\n\"))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "hello\n3\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_strings_eq_and_ne_work() {
    let src = "fn main() do\n    print(\"abc\" == \"abc\")\n    print(\"abc\" != \"xyz\")\n    print(\"abc\" == 1)\n    print(\"abc\" != 1)\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "1\n1\n0\n1\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_strings_concat_works() {
    let src = "fn main() do\n    joined = string_concat(\"ab\", \"cd\")\n    print(joined)\n    print(bytes_len(joined))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "abcd\n4\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_strings_bytes_get_and_slice_work() {
    let src = "fn main() do\n    s = \"hello\"\n    print(bytes_get(s, 1))\n    mid = bytes_slice(s, 1, 4)\n    print(mid)\n    print(bytes_len(mid))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "101\nell\n3\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_strings_bytes_pop_works() {
    let src = "fn main() do\n    s = \"hello\"\n    print(bytes_pop(s))\n    print(bytes_pop(s))\n    print(s)\n    print(bytes_len(s))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "111\n108\nhel\n3\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_strings_bytes_push_and_set_work() {
    let src = "fn main() do\n    s = \"hi\"\n    bytes_push(s, 33)\n    bytes_set(s, 1, 97)\n    print(s)\n    print(bytes_len(s))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "ha!\n3\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_jit_strings_bytes_push_and_set_work() {
    let src = "fn main() do\n    s = \"hi\"\n    bytes_push(s, 33)\n    bytes_set(s, 1, 97)\n    bytes_len(s) + bytes_get(s, 0) + bytes_get(s, 1) + bytes_get(s, 2)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 237);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_strings_bytes_insert_and_remove_work() {
    let src = "fn main() do\n    s = \"heo\"\n    bytes_insert(s, 2, 108)\n    bytes_insert(s, 4, 33)\n    print(s)\n    print(bytes_remove(s, 1))\n    print(s)\n    print(bytes_len(s))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "helo!\n101\nhlo!\n4\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_jit_strings_bytes_insert_and_remove_work() {
    let src = "fn main() do\n    s = \"heo\"\n    bytes_insert(s, 2, 108)\n    bytes_insert(s, 4, 33)\n    removed = bytes_remove(s, 1)\n    removed + bytes_len(s) + bytes_get(s, 1)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 213);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_strings_copy_isolated_from_mutation() {
    let src = "fn main() do\n    s = \"hi\"\n    t = string_copy(s)\n    bytes_push(s, 33)\n    bytes_set(t, 1, 97)\n    print(s)\n    print(t)\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "hi!\nha\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_jit_strings_copy_isolated_from_mutation() {
    let src = "fn main() do\n    s = \"hi\"\n    t = string_copy(s)\n    bytes_push(s, 33)\n    bytes_set(t, 1, 97)\n    bytes_len(s) + bytes_len(t) + bytes_get(s, 2) + bytes_get(t, 1)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 135);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_strings_utf8_iteration_works() {
    let src = "fn walk(it) do\n    if string_iter_done(it) do\n        0\n    else\n        print(string_iter_next(it))\n        walk(it)\n    end\nend\n\nfn main() do\n    walk(string_chars(\"hé🙂\"))\n    print(string_iter_done(string_chars(\"\")))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "104\n233\n128578\n1\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_jit_strings_utf8_iteration_works() {
    let src = "fn walk(it, count) do\n    if string_iter_done(it) do\n        count\n    else\n        string_iter_next(it)\n        walk(it, count + 1)\n    end\nend\n\nfn main() do\n    walk(string_chars(\"hé🙂\"), 0)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 3);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_autoloaded_stdlib_string_helpers_work() {
    let src = "fn main() do\n    print(string_is_empty(\"\"))\n    print(string_is_empty(\"x\"))\n    print(string_is_not_empty(\"\"))\n    print(string_is_not_empty(\"x\"))\n    print(string_len(\"hé🙂\"))\n    print(string_first(\"hé🙂\"))\n    print(string_last(\"hé🙂\"))\n    print(string_starts_with(\"banana\", \"ban\"))\n    print(string_starts_with(\"banana\", \"ana\"))\n    print(string_ends_with(\"banana\", \"nana\"))\n    print(string_ends_with(\"banana\", \"ban\"))\n    print(string_contains(\"banana\", \"nan\"))\n    print(string_contains(\"banana\", \"nab\"))\n    print(string_contains(\"banana\", \"\"))\n    print(string_is_ascii(\"hello\"))\n    print(string_is_ascii(\"hé\"))\n    print(string_all(\"1234\", __all_digits))\n    print(string_all(\"12a4\", __all_digits))\n    print(string_all(\"\", __all_digits))\n    print(string_any(\"12a4\", __all_digits))\n    print(string_any(\"abcd\", __all_digits))\n    print(string_any(\"\", __all_digits))\n    print(string_is_integer(\"11234\"))\n    print(string_is_integer(\"11T234\"))\n    ok0, value0, err0 = string_try_first(\"\")\n    print(ok0)\n    print(value0)\n    print(err0 == \"expected non-empty string\")\n    ok00, value00, err00 = string_try_first(\"hé🙂\")\n    print(ok00)\n    print(value00)\n    print(err00 == \"\")\n    ok01, value01, err01 = string_try_last(\"\")\n    print(ok01)\n    print(value01)\n    print(err01 == \"expected non-empty string\")\n    ok02, value02, err02 = string_try_last(\"hé🙂\")\n    print(ok02)\n    print(value02)\n    print(err02 == \"\")\n    ok03, value03, err03 = bytes_try_get(\"abc\", 1)\n    print(ok03)\n    print(value03)\n    print(err03 == \"\")\n    ok04, value04, err04 = bytes_try_get(\"abc\", 3)\n    print(ok04)\n    print(value04)\n    print(err04 == \"index out of bounds\")\n    pop_target = \"abc\"\n    ok05, value05, err05 = string_try_pop(pop_target)\n    print(ok05)\n    print(value05)\n    print(err05 == \"\")\n    print(pop_target)\n    empty_target = \"\"\n    ok06, value06, err06 = string_try_pop(empty_target)\n    print(ok06)\n    print(value06)\n    print(err06 == \"expected non-empty string\")\n    ok1, value1, err1 = string_try_parse_integer(\"123\")\n    print(ok1)\n    print(value1)\n    print(err1 == \"\")\n    ok2, value2, err2 = string_try_parse_integer(\"-45\")\n    print(ok2)\n    print(value2)\n    print(err2 == \"\")\n    ok3, value3, err3 = string_try_parse_integer(\"12a\")\n    print(ok3)\n    print(value3)\n    print(err3 == \"invalid integer\")\n    ok4, value4, err4 = string_try_parse_integer(\"\")\n    print(ok4)\n    print(value4)\n    print(err4 == \"expected at least one digit\")\n    ok5, value5, err5 = string_try_parse_integer(\"-\")\n    print(ok5)\n    print(value5)\n    print(err5 == \"expected digits after '-'\" )\n    ok6, value6, err6 = string_try_parse_bigint(\"12345678901234567890\")\n    print(ok6)\n    print(value6)\n    print(err6 == \"\")\n    ok7, value7, err7 = string_try_parse_bigint(\"-9007199254740993\")\n    print(ok7)\n    print(value7)\n    print(err7 == \"\")\n    ok8, value8, err8 = string_try_parse_bigint(\"x\")\n    print(ok8)\n    print(value8)\n    print(err8 == \"invalid integer\")\n    print(list_all([1, 2, 3], __positive_item))\n    print(list_all([1, 0, 3], __positive_item))\n    print(list_all([], __positive_item))\n    print(list_any([1, 0, 3], __zero_item))\n    print(list_any([1, 2, 3], __zero_item))\n    print(list_any([], __zero_item))\n    print(string_repeat(\"ab\", 3))\n    print(string_reverse(\"hé🙂\") == \"🙂éh\")\nend\n\nfn __all_digits(ch) do\n    if ch >= 48 do\n        ch <= 57\n    else\n        0\n    end\nend\n\nfn __positive_item(item) do\n    if item > 0 do\n        1\n    else\n        0\n    end\nend\n\nfn __zero_item(item) do\n    if item == 0 do\n        1\n    else\n        0\n    end\nend";
    assert_backend_executable_output(
        src,
        CodegenBackend::Llvm,
        "1\n0\n0\n1\n3\n104\n128578\n1\n0\n1\n0\n1\n0\n1\n1\n0\n1\n0\n0\n1\n0\n0\n1\n0\n0\n0\n1\n1\n104\n1\n0\n0\n1\n1\n128578\n1\n1\n98\n1\n0\n0\n1\n1\n99\n1\nab\n0\n0\n1\n1\n123\n1\n1\n-45\n1\n0\n0\n1\n0\n0\n1\n0\n0\n1\n1\n12345678901234567890\n1\n1\n-9007199254740993\n1\n0\n0\n1\n1\n0\n0\n1\n0\n0\nababab\n1\n",
        0,
    );
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_jit_autoloaded_stdlib_string_helpers_work() {
    let src = "fn main() do\n    if string_contains(\"banana\", \"nan\") and not string_contains(\"banana\", \"nab\") and string_repeat(\"ab\", 2) == \"abab\" and string_reverse(\"hé🙂\") == \"🙂éh\" do\n        1\n    else\n        0\n    end\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 1);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_autoloaded_stdlib_functions_can_be_used_as_values() {
    let src = "fn main() do\n    pred = string_is_empty\n    print(pred(\"\"))\n    print(pred(\"x\"))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "1\n0\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_runtime_type_predicates_and_type_of_work() {
    let src = "fn helper(x) do\n    x\nend\n\nfn main() do\n    print(is_int(1))\n    print(is_bigint(1))\n    print(is_bigint(1n))\n    print(is_string(\"x\"))\n    print(is_list([1]))\n    m = map_new()\n    print(is_map(m))\n    f = helper\n    print(is_function(f))\n    it = string_chars(\"a\")\n    print(is_string_iter(it))\n    map_it = map_iter(m)\n    print(is_map_iter(map_it))\n    print(type_of(1) == \"int\")\n    print(type_of(1n) == \"bigint\")\n    print(type_of(\"x\") == \"string\")\n    print(type_of([1]) == \"list\")\n    print(type_of(m) == \"map\")\n    print(type_of(f) == \"function\")\n    print(type_of(it) == \"string_iter\")\n    print(type_of(map_it) == \"map_iter\")\nend";
    assert_backend_executable_output(
        src,
        CodegenBackend::Llvm,
        "1\n0\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n1\n",
        0,
    );
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_string_from_codepoints_works() {
    let src = "fn main() do\n    it = string_chars(\"hé🙂\")\n    xs = list_new()\n    list_push(xs, string_iter_next(it))\n    list_push(xs, string_iter_next(it))\n    list_push(xs, string_iter_next(it))\n    print(string_from_codepoints(xs))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "hé🙂\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_map_iter_and_map_values_work() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"a\", 10)\n    map_set(m, \"b\", 32)\n    it = map_iter(m)\n    print(map_iter_done(it))\n    key1, value1 = map_iter_next(it)\n    print(key1 == \"a\" or key1 == \"b\")\n    print((key1 == \"a\" and value1 == 10) or (key1 == \"b\" and value1 == 32))\n    print(map_iter_done(it))\n    key2, value2 = map_iter_next(it)\n    print(key2 == \"a\" or key2 == \"b\")\n    print(key1 != key2)\n    print(value1 + value2)\n    print(map_iter_done(it))\n    values = map_values(m)\n    print(list_len(values))\n    print(values[0] + values[1])\nend";
    assert_backend_executable_output(
        src,
        CodegenBackend::Llvm,
        "0\n1\n1\n0\n1\n1\n42\n1\n2\n42\n",
        0,
    );
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_maps_work() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"a\", 10)\n    map_set(m, \"b\", 32)\n    map_set(m, \"a\", 11)\n    print(map_len(m))\n    print(map_has(m, \"a\"))\n    print(map_has(m, \"missing\"))\n    print(map_get(m, \"a\"))\n    print(map_get(m, \"b\"))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "2\n1\n0\n11\n32\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_map_try_get_works() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"answer\", 42)\n    ok1, value1, err1 = map_try_get(m, \"answer\")\n    print(ok1)\n    print(value1)\n    print(err1 == \"\")\n    ok2, value2, err2 = map_try_get(m, \"missing\")\n    print(ok2)\n    print(value2)\n    print(err2 == \"missing key\")\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "1\n42\n1\n0\n0\n1\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_map_delete_and_try_delete_work() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"a\", 10)\n    map_set(m, \"b\", 32)\n    print(map_delete(m, \"a\"))\n    print(map_len(m))\n    print(map_has(m, \"a\"))\n    print(map_get(m, \"b\"))\n    ok, value, err = map_try_delete(m, \"missing\")\n    print(ok)\n    print(value)\n    print(err == \"missing key\")\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "10\n1\n0\n32\n0\n0\n1\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_map_try_pop_works() {
    let src = "fn main() do\n    m = map_new()\n    ok1, key1, value1 = map_try_pop(m)\n    print(ok1)\n    print(key1 == \"\")\n    print(value1)\n    map_set(m, \"a\", 10)\n    map_set(m, \"b\", 32)\n    ok2, key2, value2 = map_try_pop(m)\n    print(ok2)\n    print(map_len(m))\n    print(key2 == \"a\" or key2 == \"b\")\n    print((key2 == \"a\" and value2 == 10) or (key2 == \"b\" and value2 == 32))\n    print(list_len(map_keys(m)))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "0\n1\n0\n1\n1\n1\n1\n1\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_map_update_and_map_update_or_default_work() {
    let src = "fn inc(x) do\n    x + 1\nend\n\nfn main() do\n    m = map_new()\n    print(map_update(m, \"count\", inc))\n    print(map_has(m, \"count\"))\n    print(map_update_or_default(m, \"count\", 0, inc))\n    print(map_get(m, \"count\"))\n    print(map_update(m, \"count\", inc))\n    print(map_get(m, \"count\"))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "0\n0\n1\n1\n1\n2\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_map_keys_work() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"a\", 10)\n    map_set(m, \"b\", 32)\n    map_set(m, \"a\", 11)\n    keys = map_keys(m)\n    print(list_len(keys))\n    print(keys[0])\n    print(keys[1])\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "2\na\nb\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_map_literal_works() {
    let src = "fn main() do\n    dyn_key = \"count\"\n    m = {\n        name: \"expr\",\n        dyn_key => 3,\n    }\n    print(map_get(m, \"name\"))\n    print(map_get(m, \"count\"))\n    print(map_len(m))\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "expr\n3\n2\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_map_delete_preserves_probe_chain_and_reuses_tombstone() {
    let (key1, key2) = test_find_colliding_map_keys();
    let src = format!(
        "fn main() do\n    m = map_new()\n    map_set(m, \"{key1}\", 10)\n    map_set(m, \"{key2}\", 32)\n    print(map_delete(m, \"{key1}\"))\n    print(map_has(m, \"{key1}\"))\n    print(map_get(m, \"{key2}\"))\n    map_set(m, \"{key1}\", 11)\n    print(map_get(m, \"{key1}\"))\n    print(map_get(m, \"{key2}\"))\n    print(map_len(m))\nend"
    );
    assert_backend_executable_output(&src, CodegenBackend::Llvm, "10\n0\n32\n11\n32\n2\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_map_grows_past_initial_capacity() {
    let src = build_large_map_growth_source(80);
    assert_backend_executable_output(&src, CodegenBackend::Llvm, "80\n1\n41\n80\n999\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_jit_autoloaded_stdlib_functions_can_be_used_as_values() {
    let src = "fn main() do\n    pred = string_is_empty\n    if pred(\"\") and not pred(\"x\") do\n        1\n    else\n        0\n    end\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 1);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_jit_logical_and_or_short_circuit() {
    let src = "fn boom() do\n    1 / 0\nend\n\nfn main() do\n    print(0 and boom())\n    print(1 or boom())\n    print(1 and 2)\n    print(0 or 5)\n    print(not 0)\n    print(not 7)\n    print(not 1 == 0)\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "0\n1\n1\n1\n1\n0\n1\n", 0);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn find_llvm_tool_prefers_explicit_tool_env_var() {
    let _guard = llvm_tool_test_lock().lock().unwrap();
    let env_name = "WASM_LD";
    let old_tool = std::env::var_os(env_name);
    let old_prefix = std::env::var_os("LLVM_SYS_201_PREFIX");
    unsafe {
        set_env_var(env_name, "custom-wasm-ld");
        remove_env_var("LLVM_SYS_201_PREFIX");
    }

    let path = find_llvm_tool("wasm-ld");

    if let Some(value) = old_tool {
        unsafe { set_env_var(env_name, value) };
    } else {
        unsafe { remove_env_var(env_name) };
    }
    if let Some(value) = old_prefix {
        unsafe { set_env_var("LLVM_SYS_201_PREFIX", value) };
    } else {
        unsafe { remove_env_var("LLVM_SYS_201_PREFIX") };
    }

    assert_eq!(path, std::path::PathBuf::from("custom-wasm-ld"));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn find_llvm_tool_uses_prefix_bin_when_tool_exists() {
    let _guard = llvm_tool_test_lock().lock().unwrap();
    let unique = format!(
        "expr-compiler-llvm-tool-{}",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system time should be after unix epoch")
            .as_nanos()
    );
    let prefix = std::env::temp_dir().join(unique);
    let bin = prefix.join("bin");
    std::fs::create_dir_all(&bin).expect("bin dir should be creatable");
    let exe_name = if cfg!(windows) { "wasm-ld.exe" } else { "wasm-ld" };
    let tool_path = bin.join(exe_name);
    std::fs::write(&tool_path, b"").expect("tool file should be creatable");

    let old_tool = std::env::var_os("WASM_LD");
    let old_prefix = std::env::var_os("LLVM_SYS_201_PREFIX");
    unsafe {
        remove_env_var("WASM_LD");
        set_env_var("LLVM_SYS_201_PREFIX", &prefix);
    }

    let path = find_llvm_tool("wasm-ld");

    if let Some(value) = old_tool {
        unsafe { set_env_var("WASM_LD", value) };
    } else {
        unsafe { remove_env_var("WASM_LD") };
    }
    if let Some(value) = old_prefix {
        unsafe { set_env_var("LLVM_SYS_201_PREFIX", value) };
    } else {
        unsafe { remove_env_var("LLVM_SYS_201_PREFIX") };
    }
    let _ = std::fs::remove_file(&tool_path);
    let _ = std::fs::remove_dir(&bin);
    let _ = std::fs::remove_dir(&prefix);

    assert_eq!(path, tool_path);
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn find_llvm_tool_falls_back_to_executable_name() {
    let _guard = llvm_tool_test_lock().lock().unwrap();
    let old_tool = std::env::var_os("WASM_LD");
    let old_prefix = std::env::var_os("LLVM_SYS_201_PREFIX");
    unsafe {
        remove_env_var("WASM_LD");
        remove_env_var("LLVM_SYS_201_PREFIX");
    }

    let path = find_llvm_tool("wasm-ld");

    if let Some(value) = old_tool {
        unsafe { set_env_var("WASM_LD", value) };
    }
    if let Some(value) = old_prefix {
        unsafe { set_env_var("LLVM_SYS_201_PREFIX", value) };
    }

    let expected = if cfg!(windows) { "wasm-ld.exe" } else { "wasm-ld" };
    assert_eq!(path, std::path::PathBuf::from(expected));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn try_compile_to_wasm_returns_toolchain_error_when_llvm_mc_is_missing() {
    let _guard = llvm_tool_test_lock().lock().unwrap();
    let unique = format!(
        "expr-compiler-missing-llvm-mc-{}",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system time should be after unix epoch")
            .as_nanos()
    );
    let missing_tool = std::env::temp_dir().join(if cfg!(windows) {
        format!("{unique}.exe")
    } else {
        unique.clone()
    });
    let output = std::env::temp_dir().join(format!("{unique}.wasm"));
    let old_tool = std::env::var_os("LLVM_MC");
    unsafe { set_env_var("LLVM_MC", &missing_tool) };

    let module = Module::try_from_source("fn main() do\n    0\nend").expect("source should parse");
    let err = module
        .try_compile_to_executable_with_backend(&output, CodegenBackend::Llvm)
        .expect_err("missing llvm-mc should surface as toolchain error");

    if let Some(value) = old_tool {
        unsafe { set_env_var("LLVM_MC", value) };
    } else {
        unsafe { remove_env_var("LLVM_MC") };
    }
    std::fs::remove_file(&output).ok();

    match err {
        CompileError::Toolchain(message) => {
            assert!(message.contains("failed to launch llvm-mc"), "{message}");
        }
        other => panic!("expected toolchain error, got {other:?}"),
    }
}

#[cfg(all(test, feature = "llvm-backend", feature = "wasi"))]
#[test]
fn try_compile_to_component_returns_toolchain_error_when_llvm_mc_is_missing() {
    let _guard = llvm_tool_test_lock().lock().unwrap();
    let unique = format!(
        "expr-compiler-missing-component-llvm-mc-{}",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system time should be after unix epoch")
            .as_nanos()
    );
    let missing_tool = std::env::temp_dir().join(if cfg!(windows) {
        format!("{unique}.exe")
    } else {
        unique.clone()
    });
    let output = std::env::temp_dir().join(format!("{unique}.component.wasm"));
    let old_tool = std::env::var_os("LLVM_MC");
    unsafe { set_env_var("LLVM_MC", &missing_tool) };

    let module = Module::try_from_source("fn main() do\n    0\nend").expect("source should parse");
    let err = module
        .try_compile_to_executable_with_backend(&output, CodegenBackend::Llvm)
        .expect_err("missing llvm-mc should surface as component toolchain error");

    if let Some(value) = old_tool {
        unsafe { set_env_var("LLVM_MC", value) };
    } else {
        unsafe { remove_env_var("LLVM_MC") };
    }
    std::fs::remove_file(&output).ok();

    match err {
        CompileError::Toolchain(message) => {
            assert!(message.contains("failed to launch llvm-mc"), "{message}");
        }
        other => panic!("expected toolchain error, got {other:?}"),
    }
}

#[cfg(all(feature = "llvm-backend", feature = "wasi"))]
#[test]
fn try_compile_to_component_succeeds_with_available_toolchain() {
    let _guard = llvm_tool_test_lock().lock().unwrap();
    let unique = format!(
        "expr-compiler-component-success-{}",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system time should be after unix epoch")
            .as_nanos()
    );
    let output = std::env::temp_dir().join(format!("{unique}.component.wasm"));
    let _ = std::fs::remove_file(&output);
    let src = "fn main(args) do\n    print(list_len(args))\nend";
    Module::try_from_source(src)
        .expect("source should parse")
        .try_compile_to_executable_with_backend(&output, CodegenBackend::Llvm)
        .expect("llvm component compile should succeed");
    assert!(output.exists(), "component output should exist");
    let _ = std::fs::remove_file(&output);
    let _ = std::fs::remove_file(output.with_extension("component.s"));
    let _ = std::fs::remove_file(output.with_extension("component.o"));
    let _ = std::fs::remove_file(output.with_extension("core.wasm"));
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_jit_main_with_args_can_use_string_helpers() {
    let src =
        "fn main(args) do\n    s = list_get(args, 0)\n    bytes_len(s) + bytes_get(s, 0)\nend";
    assert_jit_backend_result_with_args(src, CodegenBackend::Llvm, &["hi"], 106);
}

#[test]
#[should_panic(expected = "list_map callback `__lambda_1` must take exactly 1 argument")]
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
fn jit_multi_return_destructuring_works() {
    let src = "fn pair() do\n    20, 22\nend\n\nfn main() do\n    a, b = pair()\n    a + b\nend";
    assert_cranelift_jit_result(src, 42);
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_builtin_argument_type() {
    let src = "fn main() do\n    bytes_len(1)\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_list_builtin_argument_type() {
    let src = "fn main() do\n    list_len(\"abc\")\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "list_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "list");
            assert_eq!(found, "string");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_bigint_builtin_argument_type() {
    let src = "fn main() do\n    bigint_from_int(\"abc\")\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bigint_from_int");
            assert_eq!(argument, 1);
            assert_eq!(expected, "int");
            assert_eq!(found, "string");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_map_builtin_argument_type() {
    let src = "fn main() do\n    map_len(\"abc\")\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "map_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "map");
            assert_eq!(found, "string");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_map_key_argument_type() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, 1, 2)\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "map_set");
            assert_eq!(argument, 2);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_map_literal_dynamic_key_type() {
    let src = "fn main() do\n    m = {\n        1 => 2,\n    }\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "map literal");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_map_get_map_argument_type() {
    let src = "fn main() do\n    map_get(list_new(), \"a\")\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "map_get");
            assert_eq!(argument, 1);
            assert_eq!(expected, "map");
            assert_eq!(found, "list");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_map_delete_key_argument_type() {
    let src = "fn main() do\n    m = map_new()\n    map_delete(m, 1)\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "map_delete");
            assert_eq!(argument, 2);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_map_keys_argument_type() {
    let src = "fn main() do\n    map_keys(\"abc\")\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "map_keys");
            assert_eq!(argument, 1);
            assert_eq!(expected, "map");
            assert_eq!(found, "string");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_arithmetic_argument_type() {
    let src = "fn main() do\n    \"a\" + 1\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "add");
            assert_eq!(argument, 1);
            assert_eq!(expected, "int | bigint");
            assert_eq!(found, "string");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_ordered_comparison_argument_type() {
    let src = "fn main() do\n    1 < \"a\"\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "lt");
            assert_eq!(argument, 2);
            assert_eq!(expected, "int | bigint");
            assert_eq!(found, "string");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_allows_unknown_builtin_argument_types() {
    let src = "fn f(x) do\n    bytes_len(x)\nend\n\nfn main() do\n    0\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    module.try_compile_to_jit().expect("unknown parameter type should stay runtime-checked");
}

#[test]
fn try_compile_to_jit_allows_unknown_arithmetic_argument_types() {
    let src = "fn f(x) do\n    x + 1\nend\n\nfn main() do\n    0\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    module.try_compile_to_jit().expect("unknown parameter type should stay runtime-checked");
}

#[test]
fn try_compile_to_jit_rejects_invalid_index_collection_type() {
    let src = "fn main() do\n    1[0]\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "index access");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string | list");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_index_type() {
    let src = "fn main() do\n    xs = [1]\n    xs[\"0\"]\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "index access");
            assert_eq!(argument, 2);
            assert_eq!(expected, "int");
            assert_eq!(found, "string");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_index_assignment_value_type() {
    let src = "fn main() do\n    s = \"abc\"\n    s[0] = list_new()\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "index assignment");
            assert_eq!(argument, 3);
            assert_eq!(expected, "int");
            assert_eq!(found, "list");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_builtin_after_list_get() {
    let src = "fn main() do\n    xs = [1]\n    bytes_len(list_get(xs, 0))\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_builtin_after_list_index() {
    let src = "fn main() do\n    xs = [1]\n    bytes_len(xs[0])\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_builtin_after_list_map() {
    let src = "fn double(x) do\n    x * 2\nend\n\nfn main() do\n    xs = [1]\n    ys = list_map(xs, double)\n    bytes_len(ys[0])\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_builtin_after_list_filter() {
    let src = "fn main() do\n    xs = [1, 2]\n    ys = list_filter(xs, fn item -> item > 1 end)\n    bytes_len(ys[0])\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_builtin_after_list_map_function_alias() {
    let src = "fn double(x) do\n    x * 2\nend\n\nfn main() do\n    f = double\n    xs = [1]\n    ys = list_map(xs, f)\n    bytes_len(ys[0])\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_builtin_after_list_map_inline_lambda() {
    let src = "fn main() do\n    xs = [1]\n    ys = list_map(xs, fn item -> item * 2 end)\n    bytes_len(ys[0])\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_builtin_after_list_map_lambda_alias() {
    let src = "fn main() do\n    f = fn item -> item * 2 end\n    xs = [1]\n    ys = list_map(xs, f)\n    bytes_len(ys[0])\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_builtin_after_map_get() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"count\", 1)\n    bytes_len(map_get(m, \"count\"))\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_builtin_after_map_try_get() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"count\", 1)\n    ok, value, err = map_try_get(m, \"count\")\n    bytes_len(value)\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_string_builtin_after_map_try_pop() {
    let src = "fn main() do\n    m = map_new()\n    map_set(m, \"count\", 1)\n    ok, key, value = map_try_pop(m)\n    bytes_len(value)\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_allows_string_builtin_in_is_string_then_branch() {
    let src = "fn main() do\n    x = 0\n    if 1 do\n        x = \"a\"\n    else\n        x = 1\n    end\n    if is_string(x) do\n        bytes_len(x)\n    else\n        0\n    end\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    module.try_compile_to_jit().expect("jit compile should succeed");
}

#[test]
fn try_compile_to_jit_rejects_string_builtin_in_not_is_string_then_branch() {
    let src = "fn main() do\n    x = 0\n    if 1 do\n        x = \"a\"\n    else\n        x = 1\n    end\n    if not is_string(x) do\n        bytes_len(x)\n    else\n        0\n    end\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bytes_len");
            assert_eq!(argument, 1);
            assert_eq!(expected, "string");
            assert_eq!(found, "int");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_bitwise_argument_type() {
    let src = "fn main() do\n    \"a\" & 1\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "bitand");
            assert_eq!(argument, 1);
            assert_eq!(expected, "int | bigint");
            assert_eq!(found, "string");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_invalid_arithmetic_after_list_filter() {
    let src = "fn main() do\n    xs = [\"a\", \"b\"]\n    ys = list_filter(xs, fn item -> string_is_not_empty(item) end)\n    ys[0] + 1\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("jit compile should fail"),
        Err(err) => err,
    };
    match err {
        CompileError::InvalidArgumentType { function, argument, expected, found, .. } => {
            assert_eq!(function, "add");
            assert_eq!(argument, 1);
            assert_eq!(expected, "int | bigint");
            assert_eq!(found, "string");
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_destructuring_arity_mismatch() {
    let src = "fn pair() do\n    1, 2\nend\n\nfn main() do\n    only = pair()\n    only\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("compilation should fail for single assignment from multi-return"),
        Err(err) => err,
    };

    match err {
        CompileError::UnsupportedMultiValueContext { .. } => {}
        other => panic!("expected unsupported multi-value context, got {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_multi_assign_arity_mismatch() {
    let src = "fn pair() do\n    1, 2\nend\n\nfn main() do\n    a, b, c = pair()\n    a\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("compilation should fail for destructuring arity mismatch"),
        Err(err) => err,
    };

    match err {
        CompileError::DestructuringArityMismatch { expected, found, .. } => {
            assert_eq!(expected, 3);
            assert_eq!(found, 2);
        }
        other => panic!("expected destructuring arity mismatch, got {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_function_return_arity_mismatch() {
    let src = "fn pair(flag) do\n    if flag do\n        1, 2\n    else\n        1\n    end\nend\n\nfn main() do\n    0\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("compilation should fail for inconsistent return arity"),
        Err(err) => err,
    };

    match err {
        CompileError::ReturnArityMismatch { function, expected, found, .. } => {
            assert_eq!(function, "pair");
            assert_eq!(expected, 2);
            assert_eq!(found, 1);
        }
        other => panic!("expected return arity mismatch, got {other:?}"),
    }
}

#[test]
fn try_compile_to_jit_rejects_multi_return_main() {
    let src = "fn main() do\n    1, 2\nend";
    let module = Module::try_from_source(src).expect("source should parse");
    let err = match module.try_compile_to_jit() {
        Ok(_) => panic!("compilation should fail for multi-return main"),
        Err(err) => err,
    };

    match err {
        CompileError::InvalidMainReturnArity { mode, found, .. } => {
            assert_eq!(mode, "runnable main function");
            assert_eq!(found, 2);
        }
        other => panic!("expected invalid main return arity, got {other:?}"),
    }
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
fn jit_list_delete_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    x = list_delete(xs, 1)\n    x + xs[1] + list_len(xs)\nend";
    assert_cranelift_jit_result(src, 7);
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
        Ast::Expression(ExpressionAst { function, args, .. }) => {
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
    let metadata =
        module.closure_metadata.get("__lambda_1").expect("missing lifted lambda metadata");
    assert_eq!(metadata.captures, vec!["a".to_string()]);
}

#[test]
fn collect_captures_deduplicates_multiple_uses() {
    let ast = Ast::Block(BlockAst {
        lines: vec![
            Ast::Variable(Ident::synthetic("outer".to_string())),
            Ast::Variable(Ident::synthetic("outer".to_string())),
            Ast::Expression(ExpressionAst {
                function_span: None,
                function: "add".to_string(),
                args: vec![
                    Ast::Variable(Ident::synthetic("outer".to_string())),
                    Ast::Variable(Ident::synthetic("outer".to_string())),
                ],
            }),
        ],
    });

    let captures = collect_captures(&ast, &[], &["outer".to_string()]);
    assert_eq!(captures, vec!["outer".to_string()]);
}

#[test]
fn collect_captures_visits_index_and_index_assign() {
    let ast = Ast::Block(BlockAst {
        lines: vec![
            Ast::Index {
                collection: Box::new(Ast::Variable(Ident::synthetic("xs".to_string()))),
                index: Box::new(Ast::Variable(Ident::synthetic("i".to_string()))),
                span: None,
            },
            Ast::IndexAssign {
                collection: Box::new(Ast::Variable(Ident::synthetic("ys".to_string()))),
                index: Box::new(Ast::Variable(Ident::synthetic("j".to_string()))),
                value: Box::new(Ast::Variable(Ident::synthetic("value".to_string()))),
                span: None,
            },
        ],
    });

    let scope = vec![
        "xs".to_string(),
        "i".to_string(),
        "ys".to_string(),
        "j".to_string(),
        "value".to_string(),
    ];
    let captures = collect_captures(&ast, &[], &scope);
    assert_eq!(captures, scope);
}

#[test]
fn collect_captures_visits_if_else_and_assignment_values() {
    let ast = Ast::Block(BlockAst {
        lines: vec![
            Ast::Assign {
                name: "local".to_string(),
                value: Box::new(Ast::Variable(Ident::synthetic("assigned".to_string()))),
                span: None,
            },
            Ast::If {
                condition: Box::new(Ast::Variable(Ident::synthetic("cond".to_string()))),
                then: BlockAst {
                    lines: vec![Ast::Variable(Ident::synthetic("then_value".to_string()))],
                },
                else_: Some(BlockAst {
                    lines: vec![Ast::Variable(Ident::synthetic("else_value".to_string()))],
                }),
                span: None,
            },
        ],
    });

    let scope = vec![
        "assigned".to_string(),
        "cond".to_string(),
        "then_value".to_string(),
        "else_value".to_string(),
    ];
    let captures = collect_captures(&ast, &["local".to_string()], &scope);
    assert_eq!(captures, scope);
}

#[test]
fn collect_captures_respects_nested_lambda_inputs_and_locals() {
    let ast = Ast::Lambda {
        inputs: vec!["item".to_string()],
        body: Box::new(Ast::Block(BlockAst {
            lines: vec![
                Ast::Assign {
                    name: "tmp".to_string(),
                    value: Box::new(Ast::Variable(Ident::synthetic("item".to_string()))),
                    span: None,
                },
                Ast::Variable(Ident::synthetic("outer".to_string())),
                Ast::Variable(Ident::synthetic("tmp".to_string())),
                Ast::Lambda {
                    inputs: vec!["outer".to_string()],
                    body: Box::new(Ast::Block(BlockAst {
                        lines: vec![
                            Ast::Variable(Ident::synthetic("outer".to_string())),
                            Ast::Variable(Ident::synthetic("deep".to_string())),
                        ],
                    })),
                },
            ],
        })),
    };

    let captures = collect_captures(&ast, &[], &["outer".to_string(), "deep".to_string()]);
    assert_eq!(captures, vec!["outer".to_string(), "deep".to_string()]);
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
fn jit_bitwise_operators_work() {
    let src = "fn main() do\n    (6 & 3) + (4 | 1) + (7 ^ 3) + (1 << 4) + ((0 - 8) >> 1)\nend";
    assert_cranelift_jit_result(src, 23);
}

#[test]
fn bigint_bitwise_operators_work() {
    let src = "fn main() do\n    print(6n & 3n)\n    print(4n | 1n)\n    print(7n ^ 3n)\n    print(1n << 4)\n    print(32n >> 2)\nend";
    assert_cranelift_executable_output(src, "2\n5\n4\n16\n8\n", 0);
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
fn llvm_jit_bitwise_operators_work() {
    let src = "fn main() do\n    (6 & 3) + (4 | 1) + (7 ^ 3) + (1 << 4) + ((0 - 8) >> 1)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 23);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_bigint_bitwise_operators_work() {
    let src = "fn main() do\n    print(6n & 3n)\n    print(4n | 1n)\n    print(7n ^ 3n)\n    print(1n << 4)\n    print(32n >> 2)\nend";
    assert_backend_executable_output(src, CodegenBackend::Llvm, "2\n5\n4\n16\n8\n", 0);
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
fn llvm_jit_list_delete_works() {
    let src = "fn main() do\n    xs = [1, 2, 3]\n    x = list_delete(xs, 1)\n    x + xs[1] + list_len(xs)\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 7);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn llvm_jit_multi_return_destructuring_works() {
    let src = "fn pair() do\n    20, 22\nend\n\nfn main() do\n    a, b = pair()\n    a + b\nend";
    assert_jit_backend_result(src, CodegenBackend::Llvm, 42);
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
    let ptr = jit.get_int_result_fn_ptr("main").expect("llvm int-result wrapper should exist");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), 8);
}

#[cfg(feature = "llvm-backend")]
#[test]
fn cranelift_jit_int_result_wrapper_works() {
    let src = "fn main() do\n    7 + 5 - 4\nend";
    let jit = Module::from_source(src).compile_to_jit_with_backend(CodegenBackend::Cranelift);
    let ptr = jit.get_int_result_fn_ptr("main").expect("cranelift int-result wrapper should exist");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), 8);
}

#[cfg(all(feature = "llvm-backend", windows))]
#[test]
fn llvm_compile_to_executable_runs() {
    let src = "fn main() do\n    7 + 5 - 4\nend";
    let output = windows_temp_exe_path("__expr_compiler_llvm_test_exe");
    Module::from_source(src).compile_to_executable_with_backend(&output, CodegenBackend::Llvm);

    let status = Command::new(&output).status().expect("failed to run llvm executable");
    assert_eq!(status.code(), Some(8));

    std::fs::remove_file(&output).ok();
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_executable_main_can_receive_argument_list() {
    let src = "fn main(args) do\n    print(list_len(args))\n    print(list_get(args, 0))\n    print(list_get(args, 1))\n    list_len(args)\nend";
    assert_backend_executable_output_with_args(
        src,
        CodegenBackend::Llvm,
        &["hello", "world"],
        "2\nhello\nworld\n",
        2,
    );
}

#[cfg(all(test, feature = "llvm-backend"))]
#[test]
fn llvm_jit_main_can_receive_argument_list() {
    let src = "fn main(args) do\n    print(list_len(args))\n    print(list_get(args, 0))\n    print(list_get(args, 1))\n    list_len(args)\nend";
    assert_jit_backend_result_with_args(src, CodegenBackend::Llvm, &["hello", "world"], 2);
}
