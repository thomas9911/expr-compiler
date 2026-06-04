use super::{
    ClosureMetadata, CompileError, Module, function_arities, function_ordinals,
    infer_ast_value_shape, is_builtin_name, local_var_names, shape_is_exact_kind,
};
use crate::analysis::{FunctionValueKindAnalysis, KindSet, ModuleValueKindAnalysis};
use crate::parser::{Ast, ExpressionAst, FunctionDefAst, LiteralAst};
use crate::value::{
    BIGINT_HEADER_SIZE, BIGINT_LIMB_SIZE, CLOSURE_SIZE, MULTI_HEADER_SIZE, STRING_HEADER_SIZE,
    STRING_ITER_HEADER_SIZE, TAG_BIGINT, TAG_FUNCTION, TAG_INT, TAG_LIST, TAG_MULTI, TAG_STRING,
    TAG_STRING_ITER, VALUE_PAYLOAD_OFFSET, VALUE_SIZE,
};
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Linkage;
use inkwell::module::Module as LlvmModule;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::IntType;
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, IntValue, PointerValue};
use std::collections::{HashMap, HashSet};
mod ast;
mod bigint;
mod lists;
mod strings;
mod values;
mod wasi;

pub struct LlvmJitModule {
    _context: &'static Context,
    _module: &'static LlvmModule<'static>,
    execution_engine: ExecutionEngine<'static>,
    function_names: HashSet<String>,
    int_result_function_names: HashSet<String>,
}

#[derive(Clone, Copy)]
pub(super) enum LlvmOutputMode {
    Jit,
    Executable,
    Wasm,
    #[cfg(feature = "wasi")]
    WasiPreview1Command,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum LlvmRuntimeMode {
    Native,
    Wasm,
    #[cfg(feature = "wasi")]
    WasiPreview1Command,
}

#[derive(Clone, Copy)]
enum LlvmTargetKind {
    Host,
    Wasm,
}

const WASM_ARENA_BYTES: u32 = 16 * 1024 * 1024;
const WASM_ARENA_BASE: u32 = 8 * 1024 * 1024;

impl LlvmJitModule {
    pub fn get_fn_ptr(&self, name: &str) -> *const u8 {
        let symbol = internal_symbol_name(name);
        let addr = self
            .execution_engine
            .get_function_address(&symbol)
            .unwrap_or_else(|e| panic!("unable to find JIT function '{symbol}': {e}"));
        addr as usize as *const u8
    }

    pub fn has_function(&self, name: &str) -> bool {
        self.function_names.contains(name)
    }

    pub fn user_function_names(&self) -> impl Iterator<Item = &str> {
        self.function_names.iter().filter(|name| !is_builtin_name(name)).map(|name| name.as_str())
    }

    pub fn get_int_result_fn_ptr(&self, name: &str) -> Option<*const u8> {
        if !self.int_result_function_names.contains(name) {
            return None;
        }
        let symbol = int_result_symbol_name(name, LlvmOutputMode::Jit);
        let addr = self.execution_engine.get_function_address(&symbol).unwrap_or_else(|e| {
            panic!("unable to find LLVM JIT int-result function '{symbol}': {e}")
        });
        Some(addr as usize as *const u8)
    }
}

pub(super) fn compile_to_jit(expr_module: Module) -> Result<LlvmJitModule, CompileError> {
    Target::initialize_native(&InitializationConfig::default()).map_err(|e| {
        CompileError::Toolchain(format!("failed to initialize LLVM native target: {e}"))
    })?;

    let (context, module, _machine) = create_codegen_context("expr", LlvmTargetKind::Host);

    let int_result_function_names = expr_module
        .functions
        .iter()
        .filter(|func| func.inputs.len() <= 1)
        .map(|func| func.name.clone())
        .collect::<HashSet<_>>();

    let value_kind_analysis = expr_module.analyze_value_kinds()?;
    let functions = {
        let mut compiler = LlvmCompiler::new(context, module, LlvmRuntimeMode::Native);
        compiler.bigint_enabled = expr_module.uses_bigint();
        compiler.list_enabled = expr_module.uses_lists();
        compiler.list_mutation_enabled = expr_module.uses_list_mutation();
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
        compiler.closure_metadata = expr_module.closure_metadata.clone();
        compiler.value_kind_analysis = value_kind_analysis.clone();
        compiler.declare_runtime_functions();
        compiler.declare_user_functions(&expr_module.functions, LlvmOutputMode::Jit);
        compiler.define_user_functions(&expr_module.functions);
        compiler.define_int_result_wrappers(&expr_module.functions, LlvmOutputMode::Jit);
        compiler.into_functions()
    };

    module.verify().map_err(|e| CompileError::Toolchain(format!("invalid LLVM module: {e}")))?;

    let execution_engine =
        module.create_jit_execution_engine(OptimizationLevel::None).map_err(|e| {
            CompileError::Toolchain(format!("failed to create LLVM execution engine: {e}"))
        })?;
    install_runtime_mappings(&functions, &execution_engine);

    Ok(LlvmJitModule {
        _context: context,
        _module: module,
        execution_engine,
        function_names: expr_module.functions.iter().map(|func| func.name.clone()).collect(),
        int_result_function_names,
    })
}

pub(super) fn compile_to_object(expr_module: Module, name: &str) -> Result<Vec<u8>, CompileError> {
    Target::initialize_native(&InitializationConfig::default()).map_err(|e| {
        CompileError::Toolchain(format!("failed to initialize LLVM native target: {e}"))
    })?;

    let value_kind_analysis = expr_module.analyze_value_kinds()?;
    let (context, module, machine) = create_codegen_context(name, LlvmTargetKind::Host);
    {
        let mut compiler = LlvmCompiler::new(context, module, LlvmRuntimeMode::Native);
        compiler.bigint_enabled = expr_module.uses_bigint();
        compiler.list_enabled = expr_module.uses_lists();
        compiler.list_mutation_enabled = expr_module.uses_list_mutation();
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
        compiler.closure_metadata = expr_module.closure_metadata.clone();
        compiler.value_kind_analysis = value_kind_analysis;
        compiler.declare_runtime_functions();
        compiler.declare_user_functions(&expr_module.functions, LlvmOutputMode::Executable);
        compiler.define_user_functions(&expr_module.functions);
        compiler.define_int_result_wrappers(&expr_module.functions, LlvmOutputMode::Executable);
    }

    module.verify().map_err(|e| CompileError::Toolchain(format!("invalid LLVM module: {e}")))?;

    let buffer = machine
        .write_to_memory_buffer(module, FileType::Object)
        .map_err(|e| CompileError::Toolchain(format!("failed to emit LLVM object: {e}")))?;
    Ok(buffer.as_slice().to_vec())
}

pub(super) fn compile_to_wasm_assembly(
    expr_module: Module,
    name: &str,
) -> Result<Vec<u8>, CompileError> {
    Target::initialize_webassembly(&InitializationConfig::default());
    let value_kind_analysis = expr_module.analyze_value_kinds()?;
    let needs_argv_list =
        expr_module.functions.iter().any(|func| func.name == "main" && func.inputs.len() == 1);

    let (context, module, machine) = create_codegen_context(name, LlvmTargetKind::Wasm);
    {
        let mut compiler = LlvmCompiler::new(context, module, LlvmRuntimeMode::Wasm);
        compiler.bigint_enabled = expr_module.uses_bigint();
        compiler.list_enabled = expr_module.uses_lists() || needs_argv_list;
        compiler.list_mutation_enabled = expr_module.uses_list_mutation();
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
        compiler.closure_metadata = expr_module.closure_metadata.clone();
        compiler.value_kind_analysis = value_kind_analysis;
        compiler.declare_runtime_functions();
        compiler.declare_user_functions(&expr_module.functions, LlvmOutputMode::Wasm);
        compiler.define_user_functions(&expr_module.functions);
        compiler.define_int_result_wrappers(&expr_module.functions, LlvmOutputMode::Wasm);
    }

    module.verify().map_err(|e| CompileError::Toolchain(format!("invalid LLVM module: {e}")))?;

    let buffer = machine
        .write_to_memory_buffer(module, FileType::Assembly)
        .map_err(|e| CompileError::Toolchain(format!("failed to emit LLVM wasm assembly: {e}")))?;
    Ok(buffer.as_slice().to_vec())
}

#[cfg(feature = "wasi")]
pub(super) fn compile_to_wasm_preview1_command_assembly(
    expr_module: Module,
    name: &str,
) -> Result<Vec<u8>, CompileError> {
    Target::initialize_webassembly(&InitializationConfig::default());
    let value_kind_analysis = expr_module.analyze_value_kinds()?;
    let needs_argv_list =
        expr_module.functions.iter().any(|func| func.name == "main" && func.inputs.len() == 1);

    let (context, module, machine) = create_codegen_context(name, LlvmTargetKind::Wasm);
    {
        let mut compiler = LlvmCompiler::new(context, module, LlvmRuntimeMode::WasiPreview1Command);
        compiler.bigint_enabled = expr_module.uses_bigint();
        compiler.list_enabled = expr_module.uses_lists() || needs_argv_list;
        compiler.list_mutation_enabled = expr_module.uses_list_mutation();
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
        compiler.closure_metadata = expr_module.closure_metadata.clone();
        compiler.value_kind_analysis = value_kind_analysis;
        compiler.declare_runtime_functions();
        compiler
            .declare_user_functions(&expr_module.functions, LlvmOutputMode::WasiPreview1Command);
        compiler.define_user_functions(&expr_module.functions);
        compiler.define_int_result_wrappers(
            &expr_module.functions,
            LlvmOutputMode::WasiPreview1Command,
        );
        #[cfg(feature = "wasi")]
        compiler.define_wasi_preview1_command_start_wrapper();
    }

    module.verify().map_err(|e| CompileError::Toolchain(format!("invalid LLVM module: {e}")))?;

    let buffer = machine.write_to_memory_buffer(module, FileType::Assembly).map_err(|e| {
        CompileError::Toolchain(format!("failed to emit LLVM preview1 command assembly: {e}"))
    })?;
    Ok(buffer.as_slice().to_vec())
}

fn create_codegen_context(
    module_name: &str,
    target_kind: LlvmTargetKind,
) -> (&'static Context, &'static LlvmModule<'static>, TargetMachine) {
    let context = Box::leak(Box::new(Context::create()));
    let module = Box::leak(Box::new(context.create_module(module_name)));

    let triple = match target_kind {
        LlvmTargetKind::Host => TargetMachine::get_default_triple(),
        LlvmTargetKind::Wasm => inkwell::targets::TargetTriple::create("wasm32-unknown-unknown"),
    };
    module.set_triple(&triple);
    let target = Target::from_triple(&triple).expect("host triple should be supported");
    let machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            OptimizationLevel::None,
            RelocMode::Default,
            CodeModel::Default,
        )
        .expect("host target machine should be supported");
    module.set_data_layout(&machine.get_target_data().get_data_layout());

    (context, module, machine)
}

struct LlvmCompiler<'ctx> {
    context: &'ctx Context,
    module: &'ctx LlvmModule<'ctx>,
    builder: Builder<'ctx>,
    i64_type: IntType<'ctx>,
    runtime_mode: LlvmRuntimeMode,
    bigint_enabled: bool,
    list_enabled: bool,
    list_mutation_enabled: bool,
    functions: HashMap<String, FunctionValue<'ctx>>,
    function_ordinals: HashMap<String, i64>,
    function_arities: HashMap<String, usize>,
    closure_metadata: HashMap<String, ClosureMetadata>,
    value_kind_analysis: ModuleValueKindAnalysis,
}

#[derive(Clone, Copy)]
struct CompiledValue<'ctx> {
    tag: IntValue<'ctx>,
    payload: IntValue<'ctx>,
}

impl<'ctx> LlvmCompiler<'ctx> {
    fn function_analysis(&self, current_function_name: &str) -> &FunctionValueKindAnalysis {
        self.value_kind_analysis
            .functions
            .get(current_function_name)
            .expect("missing value kind analysis for LLVM function")
    }

    fn new(
        context: &'ctx Context,
        module: &'ctx LlvmModule<'ctx>,
        runtime_mode: LlvmRuntimeMode,
    ) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            i64_type: context.i64_type(),
            runtime_mode,
            bigint_enabled: false,
            list_enabled: false,
            list_mutation_enabled: false,
            functions: HashMap::new(),
            function_ordinals: HashMap::new(),
            function_arities: HashMap::new(),
            closure_metadata: HashMap::new(),
            value_kind_analysis: ModuleValueKindAnalysis { functions: HashMap::new() },
        }
    }

    fn into_functions(self) -> HashMap<String, FunctionValue<'ctx>> {
        self.functions
    }

    fn declare_runtime_functions(&mut self) {
        let i64_type = self.i64_type;
        match self.runtime_mode {
            LlvmRuntimeMode::Native => {
                let runtime = [
                    ("print", "__expr_print_host", vec![i64_type.into()]),
                    ("list_print", "__expr_list_print_host", vec![i64_type.into()]),
                    (
                        "__box_value",
                        "__expr_box_value_host",
                        vec![i64_type.into(), i64_type.into()],
                    ),
                    ("__alloc", "__expr_alloc_host", vec![i64_type.into(), i64_type.into()]),
                ];

                for (name, symbol, params) in runtime {
                    let function = self.module.add_function(
                        symbol,
                        self.i64_type.fn_type(&params, false),
                        None,
                    );
                    self.functions.insert(name.to_string(), function);
                }
            }
            LlvmRuntimeMode::Wasm => {
                let runtime = [
                    ("print", "__expr_wasm_print_host"),
                    ("list_print", "__expr_wasm_list_print_host"),
                ];

                for (name, symbol) in runtime {
                    let function = self.module.add_function(
                        symbol,
                        self.context
                            .void_type()
                            .fn_type(&[i64_type.into(), i64_type.into()], false),
                        None,
                    );
                    self.functions.insert(name.to_string(), function);
                }

                let args_runtime = [
                    ("__expr_wasm_args_len_host", self.i64_type.fn_type(&[], false)),
                    ("__expr_wasm_arg_len_host", self.i64_type.fn_type(&[i64_type.into()], false)),
                    (
                        "__expr_wasm_arg_copy_host",
                        self.i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
                    ),
                ];
                for (name, ty) in args_runtime {
                    let function = self.module.add_function(name, ty, None);
                    self.functions.insert(name.to_string(), function);
                }

                self.define_wasm_allocator("__alloc", "llvm_wasm_alloc");
                self.define_wasm_multi3();
            }
            #[cfg(feature = "wasi")]
            LlvmRuntimeMode::WasiPreview1Command => {
                self.declare_wasi_preview1_import("__wasi_fd_write", "fd_write");
                self.declare_wasi_preview1_import("__wasi_args_sizes_get", "args_sizes_get");
                self.declare_wasi_preview1_import("__wasi_args_get", "args_get");
                self.declare_wasi_preview1_import("__wasi_proc_exit", "proc_exit");
                self.define_wasm_allocator("__alloc", "llvm_wasm_alloc");
                self.define_wasm_multi3();
            }
        }

        self.define_value_to_i64();
        self.define_value_is_truthy();
        if self.bigint_enabled {
            self.define_pair_bigint_from_int("__rt_bigint_from_int", "llvm_rt_bigint_from_int");
            self.define_pair_bigint_compare("__rt_bigint_compare", "llvm_rt_bigint_compare");
            self.define_pair_bigint_add("__rt_bigint_add", "llvm_rt_bigint_add");
            self.define_pair_bigint_subtract("__rt_bigint_subtract", "llvm_rt_bigint_subtract");
            self.define_pair_bigint_multiply("__rt_bigint_multiply", "llvm_rt_bigint_multiply");
            self.define_pair_bigint_divide("__rt_bigint_divide", "llvm_rt_bigint_divide");
            self.define_pair_bigint_modulo("__rt_bigint_modulo", "llvm_rt_bigint_modulo");
            self.define_pair_bigint_bitand("__rt_bigint_bitand", "llvm_rt_bigint_bitand");
            self.define_pair_bigint_bitor("__rt_bigint_bitor", "llvm_rt_bigint_bitor");
            self.define_pair_bigint_bitxor("__rt_bigint_bitxor", "llvm_rt_bigint_bitxor");
            self.define_pair_bigint_shl("__rt_bigint_shl", "llvm_rt_bigint_shl");
            self.define_pair_bigint_shr("__rt_bigint_shr", "llvm_rt_bigint_shr");
        }
        self.define_runtime_operation(
            "__op_add",
            "llvm_rt_add",
            BinaryArithOp::Add,
            self.bigint_enabled.then_some("__rt_bigint_add"),
        );
        self.define_runtime_operation(
            "__op_subtract",
            "llvm_rt_subtract",
            BinaryArithOp::Subtract,
            self.bigint_enabled.then_some("__rt_bigint_subtract"),
        );
        self.define_runtime_operation(
            "__op_multiply",
            "llvm_rt_multiply",
            BinaryArithOp::Multiply,
            self.bigint_enabled.then_some("__rt_bigint_multiply"),
        );
        self.define_runtime_operation(
            "__op_divide",
            "llvm_rt_divide",
            BinaryArithOp::Divide,
            self.bigint_enabled.then_some("__rt_bigint_divide"),
        );
        self.define_runtime_operation(
            "__op_modulo",
            "llvm_rt_modulo",
            BinaryArithOp::Modulo,
            self.bigint_enabled.then_some("__rt_bigint_modulo"),
        );
        self.define_runtime_operation(
            "__op_bitand",
            "llvm_rt_bitand",
            BinaryArithOp::BitAnd,
            self.bigint_enabled.then_some("__rt_bigint_bitand"),
        );
        self.define_runtime_operation(
            "__op_bitor",
            "llvm_rt_bitor",
            BinaryArithOp::BitOr,
            self.bigint_enabled.then_some("__rt_bigint_bitor"),
        );
        self.define_runtime_operation(
            "__op_bitxor",
            "llvm_rt_bitxor",
            BinaryArithOp::BitXor,
            self.bigint_enabled.then_some("__rt_bigint_bitxor"),
        );
        self.define_runtime_operation(
            "__op_shl",
            "llvm_rt_shl",
            BinaryArithOp::ShiftLeft,
            self.bigint_enabled.then_some("__rt_bigint_shl"),
        );
        self.define_runtime_operation(
            "__op_shr",
            "llvm_rt_shr",
            BinaryArithOp::ShiftRight,
            self.bigint_enabled.then_some("__rt_bigint_shr"),
        );
        self.define_runtime_compare(
            "__op_gt",
            "llvm_rt_gt",
            IntPredicate::SGT,
            self.bigint_enabled.then_some("__rt_bigint_compare"),
        );
        self.define_runtime_compare(
            "__op_lt",
            "llvm_rt_lt",
            IntPredicate::SLT,
            self.bigint_enabled.then_some("__rt_bigint_compare"),
        );
        self.define_runtime_compare(
            "__op_gte",
            "llvm_rt_gte",
            IntPredicate::SGE,
            self.bigint_enabled.then_some("__rt_bigint_compare"),
        );
        self.define_runtime_compare(
            "__op_lte",
            "llvm_rt_lte",
            IntPredicate::SLE,
            self.bigint_enabled.then_some("__rt_bigint_compare"),
        );
        self.define_runtime_compare(
            "__op_eq",
            "llvm_rt_eq",
            IntPredicate::EQ,
            self.bigint_enabled.then_some("__rt_bigint_compare"),
        );
        self.define_runtime_compare(
            "__op_ne",
            "llvm_rt_ne",
            IntPredicate::NE,
            self.bigint_enabled.then_some("__rt_bigint_compare"),
        );
        match self.runtime_mode {
            LlvmRuntimeMode::Native => {
                self.define_boxed_runtime_pair_wrapper("__rt_print", "llvm_rt_print", "print", 1);
                self.define_boxed_runtime_pair_wrapper(
                    "__rt_list_print",
                    "llvm_rt_list_print",
                    "list_print",
                    1,
                );
            }
            LlvmRuntimeMode::Wasm => {
                self.define_direct_pair_print_wrapper("__rt_print", "llvm_rt_print", "print");
                self.define_direct_pair_print_wrapper(
                    "__rt_list_print",
                    "llvm_rt_list_print",
                    "list_print",
                );
            }
            #[cfg(feature = "wasi")]
            LlvmRuntimeMode::WasiPreview1Command => {
                self.define_wasi_preview1_print_runtime();
            }
        }
        if self.list_enabled {
            self.define_pair_list_new("__rt_list_new", "llvm_rt_list_new");
            self.define_pair_list_push("__rt_list_push", "llvm_rt_list_push");
            self.define_pair_list_len("__rt_list_len", "llvm_rt_list_len");
            self.define_pair_list_get("__rt_list_get", "llvm_rt_list_get");
        }
        if self.list_mutation_enabled {
            self.define_pair_list_insert("__rt_list_insert", "llvm_rt_list_insert");
            self.define_pair_list_set("__rt_list_set", "llvm_rt_list_set");
            self.define_pair_list_swap("__rt_list_swap", "llvm_rt_list_swap");
            self.define_pair_list_pop("__rt_list_pop", "llvm_rt_list_pop");
            self.define_pair_list_delete("__rt_list_delete", "llvm_rt_list_delete");
            self.define_pair_list_copy("__rt_list_copy", "llvm_rt_list_copy");
        }
    }

    fn declare_user_functions(&mut self, functions: &[FunctionDefAst], mode: LlvmOutputMode) {
        let _ = mode;
        for func in functions {
            let mut internal_params: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
                vec![self.i64_type.into()];
            for _ in 0..(func.inputs.len() * 2) {
                internal_params.push(self.i64_type.into());
            }
            let internal_symbol = internal_symbol_name(&func.name);
            let internal = self.module.add_function(
                &internal_symbol,
                self.pair_type().fn_type(&internal_params, false),
                Some(Linkage::Private),
            );
            self.functions.insert(func.name.clone(), internal);
        }
    }

    fn define_user_functions(&self, functions: &[FunctionDefAst]) {
        for func in functions {
            self.define_user_function(func);
        }
    }

    fn define_int_result_wrappers(&self, functions: &[FunctionDefAst], mode: LlvmOutputMode) {
        for func in functions {
            if matches!(mode, LlvmOutputMode::Jit) && func.inputs.len() <= 1 {
                self.define_jit_int_result_wrapper(func);
                continue;
            }
            if func.name == "main"
                && matches!(mode, LlvmOutputMode::Executable)
                && func.inputs.len() <= 1
            {
                self.define_executable_main_int_result_wrapper(func);
                continue;
            }
            if func.name == "main" && matches!(mode, LlvmOutputMode::Wasm) && func.inputs.len() <= 1
            {
                self.define_wasm_main_int_result_wrapper(func);
                continue;
            }
            #[cfg(feature = "wasi")]
            if func.name == "main"
                && matches!(mode, LlvmOutputMode::WasiPreview1Command)
                && func.inputs.len() <= 1
            {
                self.define_wasi_preview1_main_int_result_wrapper(func);
                continue;
            }
            if func.inputs.is_empty() {
                self.define_int_result_wrapper(func, mode);
            }
        }
    }

    fn define_jit_int_result_wrapper(&self, func_def: &FunctionDefAst) {
        assert!(func_def.inputs.len() <= 1, "jit int-result wrapper supports at most one argument");

        let symbol = int_result_symbol_name(&func_def.name, LlvmOutputMode::Jit);
        let function_type = if func_def.inputs.len() == 1 {
            self.i64_type.fn_type(&[self.i64_type.into(), self.i64_type.into()], false)
        } else {
            self.i64_type.fn_type(&[], false)
        };
        let function = self.module.add_function(&symbol, function_type, Some(Linkage::External));
        let internal = self.require_func(&func_def.name);
        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let call_args = if func_def.inputs.len() == 1 {
            vec![CompiledValue {
                tag: function.get_nth_param(0).unwrap().into_int_value(),
                payload: function.get_nth_param(1).unwrap().into_int_value(),
            }]
        } else {
            vec![]
        };
        let value = self.build_user_call(
            internal,
            self.i64_type.const_zero(),
            &call_args,
            "jit_int_result_value",
        );
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "jit_int_result_is_int",
            )
            .expect("failed to compare jit int-result tag");
        self.builder
            .build_conditional_branch(is_int, ok_block, trap_block)
            .expect("failed to branch on jit int-result tag");

        self.builder.position_at_end(ok_block);
        self.builder
            .build_return(Some(&value.payload))
            .expect("failed to build jit int-result wrapper return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_int_result_wrapper(&self, func_def: &FunctionDefAst, mode: LlvmOutputMode) {
        let symbol = int_result_symbol_name(&func_def.name, mode);
        let linkage = Some(Linkage::External);
        let function =
            self.module.add_function(&symbol, self.i64_type.fn_type(&[], false), linkage);
        let internal = self.require_func(&func_def.name);
        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let value =
            self.build_user_call(internal, self.i64_type.const_zero(), &[], "int_result_value");
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "int_result_is_int",
            )
            .expect("failed to compare int-result tag");
        self.builder
            .build_conditional_branch(is_int, ok_block, trap_block)
            .expect("failed to branch on int-result tag");

        self.builder.position_at_end(ok_block);
        self.builder
            .build_return(Some(&value.payload))
            .expect("failed to build int-result wrapper return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_executable_main_int_result_wrapper(&self, func_def: &FunctionDefAst) {
        assert!(
            func_def.inputs.len() <= 1,
            "native executable main function supports at most one argument"
        );

        let symbol = int_result_symbol_name(&func_def.name, LlvmOutputMode::Executable);
        let linkage = Some(Linkage::External);
        let function = self.module.add_function(
            &symbol,
            self.i64_type.fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            linkage,
        );
        let internal = self.require_func(&func_def.name);
        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let arg_tag = function.get_nth_param(0).unwrap().into_int_value();
        let arg_payload = function.get_nth_param(1).unwrap().into_int_value();
        let args = if func_def.inputs.len() == 1 {
            vec![CompiledValue { tag: arg_tag, payload: arg_payload }]
        } else {
            vec![]
        };
        let value = self.build_user_call(
            internal,
            self.i64_type.const_zero(),
            &args,
            "executable_main_value",
        );
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "executable_main_is_int",
            )
            .expect("failed to compare executable main result tag");
        self.builder
            .build_conditional_branch(is_int, ok_block, trap_block)
            .expect("failed to branch on executable main result tag");

        self.builder.position_at_end(ok_block);
        self.builder
            .build_return(Some(&value.payload))
            .expect("failed to build executable main int-result return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_wasm_main_int_result_wrapper(&self, func_def: &FunctionDefAst) {
        assert!(func_def.inputs.len() <= 1, "wasm main function supports at most one argument");

        let symbol = int_result_symbol_name(&func_def.name, LlvmOutputMode::Wasm);
        let function = self.module.add_function(
            &symbol,
            self.i64_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        let internal = self.require_func(&func_def.name);
        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let args_value = if func_def.inputs.len() == 1 {
            Some(self.build_wasm_args_list(function))
        } else {
            None
        };
        let call_args = args_value.as_ref().map_or_else(Vec::new, |value| vec![*value]);
        let value = self.build_user_call(
            internal,
            self.i64_type.const_zero(),
            &call_args,
            "wasm_main_value",
        );
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "wasm_main_is_int",
            )
            .expect("failed to compare wasm main result tag");
        self.builder
            .build_conditional_branch(is_int, ok_block, trap_block)
            .expect("failed to branch on wasm main result tag");

        self.builder.position_at_end(ok_block);
        self.builder
            .build_return(Some(&value.payload))
            .expect("failed to build wasm main int-result return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn build_wasm_args_list(&self, function: FunctionValue<'ctx>) -> CompiledValue<'ctx> {
        let args_len = self.require_func("__expr_wasm_args_len_host");
        let arg_len = self.require_func("__expr_wasm_arg_len_host");
        let arg_copy = self.require_func("__expr_wasm_arg_copy_host");
        let list_new = self.require_func("__rt_list_new");
        let list_push = self.require_func("__rt_list_push");
        let alloc = self.require_func("__alloc");

        let argc = self
            .builder
            .build_call(args_len, &[], "wasm_args_len")
            .expect("failed to call wasm args len")
            .try_as_basic_value()
            .unwrap_basic()
            .into_int_value();
        let list = self.build_internal_call(list_new, &[], "wasm_args_list");

        let index_ptr = self
            .builder
            .build_alloca(self.i64_type, "wasm_args_index")
            .expect("failed to allocate wasm args index");
        self.builder
            .build_store(index_ptr, self.i64_type.const_zero())
            .expect("failed to initialize wasm args index");

        let loop_check = self.context.append_basic_block(function, "wasm_args_loop_check");
        let loop_body = self.context.append_basic_block(function, "wasm_args_loop_body");
        let loop_done = self.context.append_basic_block(function, "wasm_args_loop_done");
        self.builder
            .build_unconditional_branch(loop_check)
            .expect("failed to branch to wasm args loop");

        self.builder.position_at_end(loop_check);
        let index = self
            .builder
            .build_load(self.i64_type, index_ptr, "wasm_args_index_load")
            .expect("failed to load wasm args index")
            .into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, argc, "wasm_args_more")
            .expect("failed to compare wasm args index");
        self.builder
            .build_conditional_branch(more, loop_body, loop_done)
            .expect("failed to branch wasm args loop");

        self.builder.position_at_end(loop_body);
        let len = self
            .builder
            .build_call(arg_len, &[index.into()], "wasm_arg_len")
            .expect("failed to call wasm arg len")
            .try_as_basic_value()
            .unwrap_basic()
            .into_int_value();
        let data_raw = self.build_boxed_call(
            alloc,
            &[len, self.i64_type.const_int(1, false)],
            "wasm_arg_data_alloc",
        );
        let _ = self
            .builder
            .build_call(arg_copy, &[index.into(), data_raw.into()], "wasm_arg_copy")
            .expect("failed to call wasm arg copy");
        let data_ptr = self
            .builder
            .build_int_to_ptr(
                data_raw,
                self.context.ptr_type(Default::default()),
                "wasm_arg_data_ptr",
            )
            .expect("failed to convert wasm arg data ptr");
        let string = self.build_string_header_from_parts(data_ptr, len, "wasm_arg_string");
        let list_value = CompiledValue { tag: list.tag, payload: list.payload };
        let _ = self.build_internal_call(list_push, &[list_value, string], "wasm_args_push");

        let next_index = self
            .builder
            .build_int_add(index, self.i64_type.const_int(1, false), "wasm_args_index_next")
            .expect("failed to increment wasm args index");
        self.builder.build_store(index_ptr, next_index).expect("failed to store wasm args index");
        self.builder
            .build_unconditional_branch(loop_check)
            .expect("failed to branch back to wasm args loop");

        self.builder.position_at_end(loop_done);
        list
    }

    fn define_user_function(&self, func_def: &FunctionDefAst) {
        let function = self.require_func(&func_def.name);
        let entry = self.context.append_basic_block(function, "entry");
        let loop_block = self.context.append_basic_block(function, "loop");
        self.builder.position_at_end(entry);

        let mut vars = HashMap::new();
        let capture_slots: HashMap<String, usize> = self
            .closure_metadata
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
        let env_slot =
            self.builder.build_alloca(self.i64_type, "env").expect("failed to allocate env slot");
        let initial_env = function.get_first_param().unwrap().into_int_value();
        self.builder.build_store(env_slot, initial_env).expect("failed to store initial env");
        for (index, name) in func_def.inputs.iter().enumerate() {
            let ptr = self
                .builder
                .build_alloca(self.pair_type(), name)
                .expect("failed to allocate function param");
            let tag = function
                .get_nth_param((index * 2 + 1) as u32)
                .unwrap_or_else(|| {
                    panic!(
                        "internal compiler error: missing tag param {index} for {}",
                        func_def.name
                    )
                })
                .into_int_value();
            let payload = function
                .get_nth_param((index * 2 + 2) as u32)
                .unwrap_or_else(|| {
                    panic!(
                        "internal compiler error: missing payload param {index} for {}",
                        func_def.name
                    )
                })
                .into_int_value();
            self.builder
                .build_store(ptr, self.make_pair_value(tag, payload, name))
                .expect("failed to store function param");
            vars.insert(name.clone(), ptr);
        }

        for name in local_var_names(&func_def.block) {
            vars.entry(name.clone()).or_insert_with(|| {
                self.builder
                    .build_alloca(self.pair_type(), &name)
                    .expect("failed to allocate local")
            });
        }

        self.builder.build_unconditional_branch(loop_block).expect("failed to branch to user loop");

        self.builder.position_at_end(loop_block);
        self.compile_tail_block(
            &func_def.block,
            &vars,
            &capture_slots,
            env_slot,
            loop_block,
            function,
            &func_def.name,
            &func_def.inputs,
        );
    }

    fn apply_function_value(
        &self,
        callback: CompiledValue<'ctx>,
        args: &[CompiledValue<'ctx>],
        function: FunctionValue<'ctx>,
        label: &str,
    ) -> CompiledValue<'ctx> {
        let trap_block = self.context.append_basic_block(function, &format!("{label}_trap"));
        let merge_block = self.context.append_basic_block(function, &format!("{label}_merge"));

        let is_function = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                callback.tag,
                self.i64_type.const_int(TAG_FUNCTION as u64, false),
                &format!("{label}_is_function"),
            )
            .expect("failed to compare callback tag");

        let mut candidates: Vec<_> = self
            .function_ordinals
            .iter()
            .filter_map(|(name, &ordinal)| {
                (self.function_arities.get(name) == Some(&args.len()))
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

        let first_check = self.context.append_basic_block(function, &format!("{label}_check0"));
        self.builder
            .build_conditional_branch(is_function, first_check, trap_block)
            .expect("failed to branch on callback tag");

        self.builder.position_at_end(first_check);
        let closure_raw_ptr = self
            .builder
            .build_int_to_ptr(
                callback.payload,
                self.context.ptr_type(Default::default()),
                &format!("{label}_closure_ptr"),
            )
            .expect("failed to convert closure ptr");
        let ordinal_ptr = self
            .builder
            .build_struct_gep(
                self.closure_type(),
                closure_raw_ptr,
                0,
                &format!("{label}_ordinal_ptr"),
            )
            .expect("failed to build closure ordinal ptr");
        let env_ptr_ptr = self
            .builder
            .build_struct_gep(
                self.closure_type(),
                closure_raw_ptr,
                1,
                &format!("{label}_env_ptr_ptr"),
            )
            .expect("failed to build closure env ptr ptr");
        let closure_ordinal = self
            .builder
            .build_load(self.i64_type, ordinal_ptr, &format!("{label}_ordinal"))
            .expect("failed to load closure ordinal")
            .into_int_value();
        let closure_env = self
            .builder
            .build_load(self.i64_type, env_ptr_ptr, &format!("{label}_env"))
            .expect("failed to load closure env")
            .into_int_value();

        let mut current_check = first_check;
        let mut incomings = Vec::with_capacity(candidates.len());
        for (index, (ordinal, name)) in candidates.iter().enumerate() {
            if index != 0 {
                self.builder.position_at_end(current_check);
            }
            let matched = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    closure_ordinal,
                    self.i64_type.const_int(*ordinal as u64, true),
                    &format!("{label}_match_{index}"),
                )
                .expect("failed to compare callback ordinal");
            let call_block =
                self.context.append_basic_block(function, &format!("{label}_call_{index}"));
            let next_block = if index + 1 == candidates.len() {
                trap_block
            } else {
                self.context.append_basic_block(function, &format!("{label}_check_{}", index + 1))
            };
            self.builder
                .build_conditional_branch(matched, call_block, next_block)
                .expect("failed to branch on callback ordinal");

            self.builder.position_at_end(call_block);
            let result = self.build_user_call(
                self.require_func(name),
                closure_env,
                args,
                &format!("{label}_apply_{index}"),
            );
            self.builder
                .build_unconditional_branch(merge_block)
                .expect("failed to branch to callback merge");
            let result_block =
                self.builder.get_insert_block().expect("missing callback result block");
            incomings.push((result, result_block));

            current_check = next_block;
        }

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(merge_block);
        let tag_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_tag"))
            .expect("failed to build callback tag phi");
        let payload_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_payload"))
            .expect("failed to build callback payload phi");
        let tag_incoming: Vec<(&dyn inkwell::values::BasicValue<'ctx>, _)> = incomings
            .iter()
            .map(|(value, block)| (&value.tag as &dyn inkwell::values::BasicValue<'ctx>, *block))
            .collect();
        let payload_incoming: Vec<(&dyn inkwell::values::BasicValue<'ctx>, _)> = incomings
            .iter()
            .map(|(value, block)| {
                (&value.payload as &dyn inkwell::values::BasicValue<'ctx>, *block)
            })
            .collect();
        tag_phi.add_incoming(&tag_incoming);
        payload_phi.add_incoming(&payload_incoming);
        CompiledValue {
            tag: tag_phi.as_basic_value().into_int_value(),
            payload: payload_phi.as_basic_value().into_int_value(),
        }
    }

    fn compile_tail_block(
        &self,
        block: &crate::parser::BlockAst,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_slot: PointerValue<'ctx>,
        loop_block: inkwell::basic_block::BasicBlock<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
        current_function_inputs: &[String],
    ) {
        if block.lines.is_empty() {
            let zero = self.int_value(self.i64_type.const_zero());
            self.builder
                .build_return(Some(&self.make_pair_value(
                    zero.tag,
                    zero.payload,
                    "empty_tail_pair",
                )))
                .expect("failed to return empty tail value");
            return;
        }

        for line in &block.lines[..block.lines.len() - 1] {
            let current_env = self
                .builder
                .build_load(self.i64_type, env_slot, "tail_env")
                .expect("failed to load tail env")
                .into_int_value();
            let _ = self.compile_ast(
                line,
                vars,
                capture_slots,
                current_env,
                function,
                current_function_name,
            );
        }

        let current_env = self
            .builder
            .build_load(self.i64_type, env_slot, "tail_last_env")
            .expect("failed to load tail env")
            .into_int_value();
        self.compile_tail_ast(
            &block.lines[block.lines.len() - 1],
            vars,
            capture_slots,
            env_slot,
            current_env,
            loop_block,
            function,
            current_function_name,
            current_function_inputs,
        );
    }

    fn compile_tail_ast(
        &self,
        ast: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_slot: PointerValue<'ctx>,
        env_ptr: IntValue<'ctx>,
        loop_block: inkwell::basic_block::BasicBlock<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
        current_function_inputs: &[String],
    ) {
        match ast {
            Ast::Expression(ExpressionAst { function: name, args, .. })
                if name == current_function_name && !is_builtin_name(name) =>
            {
                let compiled = args
                    .iter()
                    .map(|arg| {
                        self.compile_ast(
                            arg,
                            vars,
                            capture_slots,
                            env_ptr,
                            function,
                            current_function_name,
                        )
                    })
                    .collect::<Vec<_>>();
                for (index, value) in compiled.iter().enumerate() {
                    let ptr = vars.get(&current_function_inputs[index]).unwrap_or_else(|| {
            panic!(
                "internal compiler error: missing param slot {index} for {current_function_name}"
            )
        });
                    self.builder
                        .build_store(
                            *ptr,
                            self.make_pair_value(
                                value.tag,
                                value.payload,
                                &format!("tail_arg_{index}"),
                            ),
                        )
                        .expect("failed to store tail arg");
                }
                self.builder
                    .build_unconditional_branch(loop_block)
                    .expect("failed to branch to llvm tail loop");
            }
            Ast::If { condition, then, else_, .. } => {
                let cond_value = self.compile_ast(
                    condition,
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let truth = self.build_internal_scalar_call(
                    self.require_func("__value_is_truthy"),
                    &[cond_value],
                    "tail_truthy",
                );
                let cond = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        truth,
                        self.i64_type.const_zero(),
                        "tail_if_cond",
                    )
                    .expect("failed to build tail if condition");
                let then_block = self.context.append_basic_block(function, "tail_then");
                let else_block = self.context.append_basic_block(function, "tail_else");
                self.builder
                    .build_conditional_branch(cond, then_block, else_block)
                    .expect("failed to branch in tail if");

                self.builder.position_at_end(then_block);
                self.compile_tail_block(
                    then,
                    vars,
                    capture_slots,
                    env_slot,
                    loop_block,
                    function,
                    current_function_name,
                    current_function_inputs,
                );

                self.builder.position_at_end(else_block);
                if let Some(else_block_ast) = else_ {
                    self.compile_tail_block(
                        else_block_ast,
                        vars,
                        capture_slots,
                        env_slot,
                        loop_block,
                        function,
                        current_function_name,
                        current_function_inputs,
                    );
                } else {
                    let zero = self.int_value(self.i64_type.const_zero());
                    self.builder
                        .build_return(Some(&self.make_pair_value(
                            zero.tag,
                            zero.payload,
                            "tail_else_zero",
                        )))
                        .expect("failed to return tail else zero");
                }
            }
            Ast::Block(block) => self.compile_tail_block(
                block,
                vars,
                capture_slots,
                env_slot,
                loop_block,
                function,
                current_function_name,
                current_function_inputs,
            ),
            _ => {
                let value = self.compile_ast(
                    ast,
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                self.builder
                    .build_return(Some(&self.make_pair_value(
                        value.tag,
                        value.payload,
                        "tail_return_pair",
                    )))
                    .expect("failed to return tail value");
            }
        }
    }

    fn build_boxed_call(
        &self,
        function: FunctionValue<'ctx>,
        args: &[IntValue<'ctx>],
        label: &str,
    ) -> IntValue<'ctx> {
        let args = args.iter().copied().map(BasicMetadataValueEnum::from).collect::<Vec<_>>();
        self.builder
            .build_call(function, &args, label)
            .expect("failed to build call")
            .try_as_basic_value()
            .unwrap_basic()
            .into_int_value()
    }

    fn build_internal_call(
        &self,
        function: FunctionValue<'ctx>,
        args: &[CompiledValue<'ctx>],
        label: &str,
    ) -> CompiledValue<'ctx> {
        let args = args
            .iter()
            .flat_map(|value| [value.tag, value.payload])
            .map(BasicMetadataValueEnum::from)
            .collect::<Vec<_>>();
        let pair = self
            .builder
            .build_call(function, &args, label)
            .expect("failed to build internal call")
            .try_as_basic_value()
            .unwrap_basic()
            .into_struct_value();
        let tag = self
            .builder
            .build_extract_value(pair, 0, &format!("{label}_tag"))
            .expect("failed to extract tag")
            .into_int_value();
        let payload = self
            .builder
            .build_extract_value(pair, 1, &format!("{label}_payload"))
            .expect("failed to extract payload")
            .into_int_value();
        CompiledValue { tag, payload }
    }

    fn build_user_call(
        &self,
        function: FunctionValue<'ctx>,
        env_ptr: IntValue<'ctx>,
        args: &[CompiledValue<'ctx>],
        label: &str,
    ) -> CompiledValue<'ctx> {
        let mut call_args = Vec::with_capacity(1 + args.len() * 2);
        call_args.push(BasicMetadataValueEnum::from(env_ptr));
        for value in args {
            call_args.push(BasicMetadataValueEnum::from(value.tag));
            call_args.push(BasicMetadataValueEnum::from(value.payload));
        }
        let pair = self
            .builder
            .build_call(function, &call_args, label)
            .expect("failed to build user call")
            .try_as_basic_value()
            .unwrap_basic()
            .into_struct_value();
        let tag = self
            .builder
            .build_extract_value(pair, 0, &format!("{label}_tag"))
            .expect("failed to extract user-call tag")
            .into_int_value();
        let payload = self
            .builder
            .build_extract_value(pair, 1, &format!("{label}_payload"))
            .expect("failed to extract user-call payload")
            .into_int_value();
        CompiledValue { tag, payload }
    }

    fn build_internal_scalar_call(
        &self,
        function: FunctionValue<'ctx>,
        args: &[CompiledValue<'ctx>],
        label: &str,
    ) -> IntValue<'ctx> {
        let args = args
            .iter()
            .flat_map(|value| [value.tag, value.payload])
            .map(BasicMetadataValueEnum::from)
            .collect::<Vec<_>>();
        self.builder
            .build_call(function, &args, label)
            .expect("failed to build internal scalar call")
            .try_as_basic_value()
            .unwrap_basic()
            .into_int_value()
    }

    fn compile_logical_op(
        &self,
        name: &str,
        lhs_ast: &Ast,
        rhs_ast: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        let lhs = self.compile_ast(
            lhs_ast,
            vars,
            capture_slots,
            env_ptr,
            function,
            current_function_name,
        );
        let lhs_truth = self.build_internal_scalar_call(
            self.require_func("__value_is_truthy"),
            &[lhs],
            "logic_lhs",
        );
        let lhs_non_zero = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                lhs_truth,
                self.i64_type.const_zero(),
                "logic_lhs_non_zero",
            )
            .expect("failed to compare logical lhs truth");

        let rhs_block = self.context.append_basic_block(function, "logic_rhs");
        let short_block = self.context.append_basic_block(function, "logic_short");
        let merge_block = self.context.append_basic_block(function, "logic_merge");

        if name == "and" {
            self.builder
                .build_conditional_branch(lhs_non_zero, rhs_block, short_block)
                .expect("failed to branch logical and");
        } else {
            self.builder
                .build_conditional_branch(lhs_non_zero, short_block, rhs_block)
                .expect("failed to branch logical or");
        }

        self.builder.position_at_end(short_block);
        let short_payload = self.i64_type.const_int(if name == "and" { 0 } else { 1 }, false);
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed to branch logical short merge");
        let short_from = self.builder.get_insert_block().expect("missing logical short block");

        self.builder.position_at_end(rhs_block);
        let rhs = self.compile_ast(
            rhs_ast,
            vars,
            capture_slots,
            env_ptr,
            function,
            current_function_name,
        );
        let rhs_truth = self.build_internal_scalar_call(
            self.require_func("__value_is_truthy"),
            &[rhs],
            "logic_rhs",
        );
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed to branch logical rhs merge");
        let rhs_from = self.builder.get_insert_block().expect("missing logical rhs block");

        self.builder.position_at_end(merge_block);
        let payload_phi = self
            .builder
            .build_phi(self.i64_type, "logic_payload")
            .expect("failed to build logical payload phi");
        payload_phi.add_incoming(&[
            (&short_payload as &dyn inkwell::values::BasicValue<'ctx>, short_from),
            (&rhs_truth as &dyn inkwell::values::BasicValue<'ctx>, rhs_from),
        ]);
        CompiledValue {
            tag: self.i64_type.const_int(TAG_INT as u64, false),
            payload: payload_phi.as_basic_value().into_int_value(),
        }
    }

    fn compile_logical_not(
        &self,
        arg_ast: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        let arg = self.compile_ast(
            arg_ast,
            vars,
            capture_slots,
            env_ptr,
            function,
            current_function_name,
        );
        let truth = self.build_internal_scalar_call(
            self.require_func("__value_is_truthy"),
            &[arg],
            "not_truth",
        );
        let is_zero = self
            .builder
            .build_int_compare(IntPredicate::EQ, truth, self.i64_type.const_zero(), "not_is_zero")
            .expect("failed to compare logical not truth");
        let payload = self
            .builder
            .build_int_z_extend(is_zero, self.i64_type, "not_payload")
            .expect("failed to extend logical not");
        CompiledValue { tag: self.i64_type.const_int(TAG_INT as u64, false), payload }
    }

    fn compile_is_tag_predicate(
        &self,
        value: CompiledValue<'ctx>,
        expected_tag: i64,
        label: &str,
    ) -> CompiledValue<'ctx> {
        let matches = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(expected_tag as u64, false),
                &format!("{label}_matches"),
            )
            .expect("failed to compare runtime tag");
        let payload = self
            .builder
            .build_int_z_extend(matches, self.i64_type, &format!("{label}_payload"))
            .expect("failed to extend runtime tag predicate");
        CompiledValue { tag: self.i64_type.const_int(TAG_INT as u64, false), payload }
    }

    fn build_trap_if(&self, condition: IntValue<'ctx>) {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let trap_block = self.context.append_basic_block(function, "trap_if_trap");
        let ok_block = self.context.append_basic_block(function, "trap_if_ok");
        self.builder
            .build_conditional_branch(condition, trap_block, ok_block)
            .expect("failed trap_if branch");
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(ok_block);
    }

    fn compile_exact_int_operator(
        &self,
        name: &str,
        lhs: CompiledValue<'ctx>,
        rhs: CompiledValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let raw = match name {
            "add" => {
                let (raw, overflow) = self.build_overflow_intrinsic_call(
                    "llvm.sadd.with.overflow.i64",
                    lhs.payload,
                    rhs.payload,
                    "int_add",
                );
                self.build_trap_if(overflow);
                raw
            }
            "subtract" => {
                let (raw, overflow) = self.build_overflow_intrinsic_call(
                    "llvm.ssub.with.overflow.i64",
                    lhs.payload,
                    rhs.payload,
                    "int_sub",
                );
                self.build_trap_if(overflow);
                raw
            }
            "multiply" => {
                let (raw, overflow) = self.build_overflow_intrinsic_call(
                    "llvm.smul.with.overflow.i64",
                    lhs.payload,
                    rhs.payload,
                    "int_mul",
                );
                self.build_trap_if(overflow);
                raw
            }
            "divide" | "modulo" => {
                let safe = self.build_division_safe_check(lhs.payload, rhs.payload, "int_div");
                let unsafe_block = self.context.append_basic_block(
                    self.builder.get_insert_block().unwrap().get_parent().unwrap(),
                    "int_div_trap",
                );
                let ok_block = self.context.append_basic_block(
                    self.builder.get_insert_block().unwrap().get_parent().unwrap(),
                    "int_div_ok",
                );
                self.builder
                    .build_conditional_branch(safe, ok_block, unsafe_block)
                    .expect("failed int div safe branch");
                self.builder.position_at_end(unsafe_block);
                self.build_trap_and_unreachable();
                self.builder.position_at_end(ok_block);
                if name == "divide" {
                    self.builder
                        .build_int_signed_div(lhs.payload, rhs.payload, "int_div")
                        .expect("failed int div")
                } else {
                    self.builder
                        .build_int_signed_rem(lhs.payload, rhs.payload, "int_rem")
                        .expect("failed int rem")
                }
            }
            "bitand" => self
                .builder
                .build_and(lhs.payload, rhs.payload, "int_bitand")
                .expect("failed int bitand"),
            "bitor" => self
                .builder
                .build_or(lhs.payload, rhs.payload, "int_bitor")
                .expect("failed int bitor"),
            "bitxor" => self
                .builder
                .build_xor(lhs.payload, rhs.payload, "int_bitxor")
                .expect("failed int bitxor"),
            "shl" | "shr" => {
                let non_neg = self
                    .builder
                    .build_int_compare(
                        IntPredicate::SGE,
                        rhs.payload,
                        self.i64_type.const_zero(),
                        "shift_non_neg",
                    )
                    .expect("failed shift non-neg");
                let lt_width = self
                    .builder
                    .build_int_compare(
                        IntPredicate::SLT,
                        rhs.payload,
                        self.i64_type.const_int(64, false),
                        "shift_lt_width",
                    )
                    .expect("failed shift lt width");
                let in_range = self
                    .builder
                    .build_and(non_neg, lt_width, "shift_in_range")
                    .expect("failed shift in range");
                let invalid = self
                    .builder
                    .build_not(in_range, "shift_invalid")
                    .expect("failed shift invalid");
                self.build_trap_if(invalid);
                if name == "shl" {
                    self.builder
                        .build_left_shift(lhs.payload, rhs.payload, "int_shl")
                        .expect("failed int shl")
                } else {
                    self.builder
                        .build_right_shift(lhs.payload, rhs.payload, true, "int_shr")
                        .expect("failed int shr")
                }
            }
            "gt" | "lt" | "gte" | "lte" | "eq" | "ne" => {
                let predicate = match name {
                    "gt" => IntPredicate::SGT,
                    "lt" => IntPredicate::SLT,
                    "gte" => IntPredicate::SGE,
                    "lte" => IntPredicate::SLE,
                    "eq" => IntPredicate::EQ,
                    "ne" => IntPredicate::NE,
                    _ => unreachable!(),
                };
                let cmp = self
                    .builder
                    .build_int_compare(predicate, lhs.payload, rhs.payload, "int_cmp")
                    .expect("failed int cmp");
                let one = self.i64_type.const_int(1, false);
                let zero = self.i64_type.const_zero();
                self.builder
                    .build_select(cmp, one, zero, "int_cmp_raw")
                    .expect("failed int cmp select")
                    .into_int_value()
            }
            _ => unreachable!("not an exact int operator: {name}"),
        };
        CompiledValue { tag: self.i64_type.const_int(TAG_INT as u64, false), payload: raw }
    }

    fn compile_exact_bigint_operator(
        &self,
        name: &str,
        lhs: CompiledValue<'ctx>,
        rhs: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        match name {
            "add" => {
                self.build_internal_call(self.require_func("bigint_add"), &[lhs, rhs], "bigint_add")
            }
            "subtract" => self.build_internal_call(
                self.require_func("bigint_subtract"),
                &[lhs, rhs],
                "bigint_subtract",
            ),
            "multiply" => self.build_internal_call(
                self.require_func("bigint_multiply"),
                &[lhs, rhs],
                "bigint_multiply",
            ),
            "divide" => self.build_internal_call(
                self.require_func("bigint_divide"),
                &[lhs, rhs],
                "bigint_divide",
            ),
            "modulo" => self.build_internal_call(
                self.require_func("bigint_modulo"),
                &[lhs, rhs],
                "bigint_modulo",
            ),
            "bitand" => self.build_internal_call(
                self.require_func("bigint_bitand"),
                &[lhs, rhs],
                "bigint_bitand",
            ),
            "bitor" => self.build_internal_call(
                self.require_func("bigint_bitor"),
                &[lhs, rhs],
                "bigint_bitor",
            ),
            "bitxor" => self.build_internal_call(
                self.require_func("bigint_bitxor"),
                &[lhs, rhs],
                "bigint_bitxor",
            ),
            "shl" => self.compile_bigint_shift_builtin("bigint_shl", lhs, rhs, function),
            "shr" => self.compile_bigint_shift_builtin("bigint_shr", lhs, rhs, function),
            "gt" | "lt" | "gte" | "lte" | "eq" | "ne" => {
                let compare = self.build_internal_call(
                    self.require_func("bigint_compare"),
                    &[lhs, rhs],
                    "bigint_compare",
                );
                let predicate = match name {
                    "gt" => IntPredicate::SGT,
                    "lt" => IntPredicate::SLT,
                    "gte" => IntPredicate::SGE,
                    "lte" => IntPredicate::SLE,
                    "eq" => IntPredicate::EQ,
                    "ne" => IntPredicate::NE,
                    _ => unreachable!(),
                };
                let cmp = self
                    .builder
                    .build_int_compare(
                        predicate,
                        compare.payload,
                        self.i64_type.const_zero(),
                        "bigint_cmp",
                    )
                    .expect("failed bigint cmp");
                let raw = self
                    .builder
                    .build_select(
                        cmp,
                        self.i64_type.const_int(1, false),
                        self.i64_type.const_zero(),
                        "bigint_cmp_raw",
                    )
                    .expect("failed bigint cmp select")
                    .into_int_value();
                CompiledValue { tag: self.i64_type.const_int(TAG_INT as u64, false), payload: raw }
            }
            _ => unreachable!("not an exact bigint operator: {name}"),
        }
    }

    fn require_func(&self, name: &str) -> FunctionValue<'ctx> {
        *self.functions.get(name).unwrap_or_else(|| {
            panic!("internal compiler error: missing function declaration: {name}")
        })
    }

    fn define_value_to_i64(&mut self) {
        let function = self.module.add_function(
            "llvm_rt_value_to_i64",
            self.i64_type.fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert("__value_to_i64".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let tag = function.get_first_param().unwrap().into_int_value();
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "value_to_i64_is_int",
            )
            .expect("failed to compare value tag");
        self.builder
            .build_conditional_branch(is_int, ok_block, trap_block)
            .expect("failed to branch on int tag");

        self.builder.position_at_end(ok_block);
        let raw = function.get_nth_param(1).unwrap().into_int_value();

        self.builder.build_return(Some(&raw)).expect("failed to return raw int");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_value_is_truthy(&mut self) {
        let function = self.module.add_function(
            "llvm_rt_value_is_truthy",
            self.i64_type.fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert("__value_is_truthy".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let int_block = self.context.append_basic_block(function, "int");
        let list_check_block = self.context.append_basic_block(function, "list_check");
        let list_block = self.context.append_basic_block(function, "list");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let tag = function.get_first_param().unwrap().into_int_value();
        let payload = function.get_nth_param(1).unwrap().into_int_value();
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "truthy_is_int",
            )
            .expect("failed to compare int tag");
        let is_list = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                self.i64_type.const_int(TAG_LIST as u64, false),
                "truthy_is_list",
            )
            .expect("failed to compare list tag");
        self.builder
            .build_conditional_branch(is_int, int_block, list_check_block)
            .expect("failed to branch on int truthiness");

        self.builder.position_at_end(int_block);
        let int_truthy = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                payload,
                self.i64_type.const_zero(),
                "truthy_int_flag",
            )
            .expect("failed to compare int truthiness");
        let int_raw = self
            .builder
            .build_int_z_extend(int_truthy, self.i64_type, "truthy_int_i64")
            .expect("failed to extend int truthiness");
        self.builder.build_return(Some(&int_raw)).expect("failed to return int truthiness");

        self.builder.position_at_end(list_check_block);
        self.builder
            .build_conditional_branch(is_list, list_block, trap_block)
            .expect("failed to validate list truthiness");

        self.builder.position_at_end(list_block);
        let list_ptr = self
            .builder
            .build_int_to_ptr(payload, self.context.ptr_type(Default::default()), "truthy_list_ptr")
            .expect("failed to convert list payload to pointer");
        let len_ptr = self
            .builder
            .build_struct_gep(self.list_header_type(), list_ptr, 1, "truthy_list_len_ptr")
            .expect("failed to build list len gep");
        let len = self
            .builder
            .build_load(self.i64_type, len_ptr, "truthy_list_len")
            .expect("failed to load list len")
            .into_int_value();
        let list_truthy = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                len,
                self.i64_type.const_zero(),
                "truthy_list_flag",
            )
            .expect("failed to compare list truthiness");
        let list_raw = self
            .builder
            .build_int_z_extend(list_truthy, self.i64_type, "truthy_list_i64")
            .expect("failed to extend list truthiness");

        self.builder.build_return(Some(&list_raw)).expect("failed to return truthiness");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_runtime_operation(
        &mut self,
        name: &str,
        symbol: &str,
        op: BinaryArithOp,
        bigint_name: Option<&str>,
    ) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(
                &[
                    self.i64_type.into(),
                    self.i64_type.into(),
                    self.i64_type.into(),
                    self.i64_type.into(),
                ],
                false,
            ),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let int_block = self.context.append_basic_block(function, "int");
        let trap_block = self.context.append_basic_block(function, "trap");
        let non_int_block = self.context.append_basic_block(function, "non_int");
        self.builder.position_at_end(entry);

        let lhs_tag = function.get_first_param().unwrap().into_int_value();
        let lhs_payload = function.get_nth_param(1).unwrap().into_int_value();
        let rhs_tag = function.get_nth_param(2).unwrap().into_int_value();
        let rhs_payload = function.get_nth_param(3).unwrap().into_int_value();
        let lhs_is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "lhs_is_int",
            )
            .expect("failed lhs_is_int");
        let rhs_is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rhs_tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "rhs_is_int",
            )
            .expect("failed rhs_is_int");
        let both_int =
            self.builder.build_and(lhs_is_int, rhs_is_int, "both_int").expect("failed both_int");
        self.builder
            .build_conditional_branch(both_int, int_block, non_int_block)
            .expect("failed to branch on int operands");

        self.builder.position_at_end(int_block);
        let lhs_raw = lhs_payload;
        let rhs_raw = rhs_payload;
        let raw = match op {
            BinaryArithOp::Add => {
                let int_ok_block = self.context.append_basic_block(function, "int_ok");
                let (value, overflow) = self.build_overflow_intrinsic_call(
                    "llvm.sadd.with.overflow.i64",
                    lhs_raw,
                    rhs_raw,
                    "add",
                );
                self.builder
                    .build_conditional_branch(overflow, trap_block, int_ok_block)
                    .expect("failed to branch on add overflow");
                self.builder.position_at_end(int_ok_block);
                value
            }
            BinaryArithOp::Subtract => {
                let int_ok_block = self.context.append_basic_block(function, "int_ok");
                let (value, overflow) = self.build_overflow_intrinsic_call(
                    "llvm.ssub.with.overflow.i64",
                    lhs_raw,
                    rhs_raw,
                    "sub",
                );
                self.builder
                    .build_conditional_branch(overflow, trap_block, int_ok_block)
                    .expect("failed to branch on subtract overflow");
                self.builder.position_at_end(int_ok_block);
                value
            }
            BinaryArithOp::Multiply => {
                let int_ok_block = self.context.append_basic_block(function, "int_ok");
                let (value, overflow) = self.build_overflow_intrinsic_call(
                    "llvm.smul.with.overflow.i64",
                    lhs_raw,
                    rhs_raw,
                    "mul",
                );
                self.builder
                    .build_conditional_branch(overflow, trap_block, int_ok_block)
                    .expect("failed to branch on multiply overflow");
                self.builder.position_at_end(int_ok_block);
                value
            }
            BinaryArithOp::Divide => {
                let int_ok_block = self.context.append_basic_block(function, "int_ok");
                let div_ok = self.build_division_safe_check(lhs_raw, rhs_raw, "div");
                self.builder
                    .build_conditional_branch(div_ok, int_ok_block, trap_block)
                    .expect("failed to build div branch");
                self.builder.position_at_end(int_ok_block);
                self.builder
                    .build_int_signed_div(lhs_raw, rhs_raw, "quot")
                    .expect("failed to divide")
            }
            BinaryArithOp::Modulo => {
                let int_ok_block = self.context.append_basic_block(function, "int_ok");
                let rem_ok = self.build_division_safe_check(lhs_raw, rhs_raw, "rem");
                self.builder
                    .build_conditional_branch(rem_ok, int_ok_block, trap_block)
                    .expect("failed to build rem branch");
                self.builder.position_at_end(int_ok_block);
                self.builder
                    .build_int_signed_rem(lhs_raw, rhs_raw, "rem")
                    .expect("failed to modulo")
            }
            BinaryArithOp::BitAnd => {
                self.builder.build_and(lhs_raw, rhs_raw, "bitand").expect("failed bitand")
            }
            BinaryArithOp::BitOr => {
                self.builder.build_or(lhs_raw, rhs_raw, "bitor").expect("failed bitor")
            }
            BinaryArithOp::BitXor => {
                self.builder.build_xor(lhs_raw, rhs_raw, "bitxor").expect("failed bitxor")
            }
            BinaryArithOp::ShiftLeft | BinaryArithOp::ShiftRight => {
                let int_ok_block = self.context.append_basic_block(function, "int_ok");
                let rhs_non_neg = self
                    .builder
                    .build_int_compare(
                        IntPredicate::SGE,
                        rhs_raw,
                        self.i64_type.const_zero(),
                        "shift_non_neg",
                    )
                    .expect("failed shift non-neg");
                let rhs_lt_width = self
                    .builder
                    .build_int_compare(
                        IntPredicate::SLT,
                        rhs_raw,
                        self.i64_type.const_int(64, false),
                        "shift_lt_width",
                    )
                    .expect("failed shift lt width");
                let rhs_in_range = self
                    .builder
                    .build_and(rhs_non_neg, rhs_lt_width, "shift_in_range")
                    .expect("failed shift in range");
                self.builder
                    .build_conditional_branch(rhs_in_range, int_ok_block, trap_block)
                    .expect("failed to build shift branch");
                self.builder.position_at_end(int_ok_block);
                if matches!(op, BinaryArithOp::ShiftLeft) {
                    self.builder.build_left_shift(lhs_raw, rhs_raw, "shl").expect("failed shl")
                } else {
                    self.builder
                        .build_right_shift(lhs_raw, rhs_raw, true, "shr")
                        .expect("failed shr")
                }
            }
        };

        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                raw,
                "op_result",
            )))
            .expect("failed to build runtime return");

        self.builder.position_at_end(non_int_block);
        if let Some(bigint_name) = bigint_name {
            let lhs_is_bigint = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    lhs_tag,
                    self.i64_type.const_int(TAG_BIGINT as u64, false),
                    "lhs_is_bigint",
                )
                .expect("failed lhs_is_bigint");
            let rhs_is_bigint = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    rhs_tag,
                    self.i64_type.const_int(TAG_BIGINT as u64, false),
                    "rhs_is_bigint",
                )
                .expect("failed rhs_is_bigint");
            if matches!(op, BinaryArithOp::ShiftLeft | BinaryArithOp::ShiftRight) {
                let lhs_bigint_rhs_int = self
                    .builder
                    .build_and(lhs_is_bigint, rhs_is_int, "lhs_bigint_rhs_int")
                    .expect("failed lhs_bigint_rhs_int");
                let bigint_block = self.context.append_basic_block(function, "bigint_shift");
                self.builder
                    .build_conditional_branch(lhs_bigint_rhs_int, bigint_block, trap_block)
                    .expect("failed bigint shift branch");

                self.builder.position_at_end(bigint_block);
                let result = self.build_internal_call(
                    self.require_func(bigint_name),
                    &[
                        CompiledValue { tag: lhs_tag, payload: lhs_payload },
                        CompiledValue { tag: rhs_tag, payload: rhs_payload },
                    ],
                    "bigint_shift",
                );
                self.builder
                    .build_return(Some(&self.make_pair_value(
                        result.tag,
                        result.payload,
                        "bigint_shift_result",
                    )))
                    .expect("failed to return bigint shift result");
            } else {
                let both_bigint = self
                    .builder
                    .build_and(lhs_is_bigint, rhs_is_bigint, "both_bigint")
                    .expect("failed both_bigint");
                let bigint_block = self.context.append_basic_block(function, "bigint");
                let lhs_promote_check_block =
                    self.context.append_basic_block(function, "lhs_promote_check");
                let lhs_promote_block = self.context.append_basic_block(function, "lhs_promote");
                let rhs_maybe_promote_block =
                    self.context.append_basic_block(function, "rhs_maybe_promote");
                let rhs_promote_block = self.context.append_basic_block(function, "rhs_promote");
                self.builder
                    .build_conditional_branch(both_bigint, bigint_block, lhs_promote_check_block)
                    .expect("failed bigint branch");

                self.builder.position_at_end(bigint_block);
                let result = self.build_internal_call(
                    self.require_func(bigint_name),
                    &[
                        CompiledValue { tag: lhs_tag, payload: lhs_payload },
                        CompiledValue { tag: rhs_tag, payload: rhs_payload },
                    ],
                    "bigint_op",
                );
                self.builder
                    .build_return(Some(&self.make_pair_value(
                        result.tag,
                        result.payload,
                        "bigint_op_result",
                    )))
                    .expect("failed to return bigint op result");

                self.builder.position_at_end(lhs_promote_check_block);
                let lhs_int_rhs_bigint = self
                    .builder
                    .build_and(lhs_is_int, rhs_is_bigint, "lhs_int_rhs_bigint")
                    .expect("failed lhs_int_rhs_bigint");
                self.builder
                    .build_conditional_branch(
                        lhs_int_rhs_bigint,
                        lhs_promote_block,
                        rhs_maybe_promote_block,
                    )
                    .expect("failed lhs promote branch");

                self.builder.position_at_end(lhs_promote_block);
                let lhs_big = self.build_internal_call(
                    self.require_func("__rt_bigint_from_int"),
                    &[CompiledValue { tag: lhs_tag, payload: lhs_payload }],
                    "lhs_promoted_bigint",
                );
                let result = self.build_internal_call(
                    self.require_func(bigint_name),
                    &[lhs_big, CompiledValue { tag: rhs_tag, payload: rhs_payload }],
                    "mixed_bigint_op_lhs",
                );
                self.builder
                    .build_return(Some(&self.make_pair_value(
                        result.tag,
                        result.payload,
                        "mixed_bigint_op_lhs_result",
                    )))
                    .expect("failed to return mixed bigint lhs op result");

                self.builder.position_at_end(rhs_maybe_promote_block);
                let rhs_int_lhs_bigint = self
                    .builder
                    .build_and(lhs_is_bigint, rhs_is_int, "rhs_int_lhs_bigint")
                    .expect("failed rhs_int_lhs_bigint");
                self.builder
                    .build_conditional_branch(rhs_int_lhs_bigint, rhs_promote_block, trap_block)
                    .expect("failed rhs maybe promote branch");

                self.builder.position_at_end(rhs_promote_block);
                let rhs_big = self.build_internal_call(
                    self.require_func("__rt_bigint_from_int"),
                    &[CompiledValue { tag: rhs_tag, payload: rhs_payload }],
                    "rhs_promoted_bigint",
                );
                let result = self.build_internal_call(
                    self.require_func(bigint_name),
                    &[CompiledValue { tag: lhs_tag, payload: lhs_payload }, rhs_big],
                    "mixed_bigint_op_rhs",
                );
                self.builder
                    .build_return(Some(&self.make_pair_value(
                        result.tag,
                        result.payload,
                        "mixed_bigint_op_rhs_result",
                    )))
                    .expect("failed to return mixed bigint rhs op result");
            }
        } else {
            self.build_trap_and_unreachable();
        }

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_runtime_compare(
        &mut self,
        name: &str,
        symbol: &str,
        pred: IntPredicate,
        bigint_name: Option<&str>,
    ) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(
                &[
                    self.i64_type.into(),
                    self.i64_type.into(),
                    self.i64_type.into(),
                    self.i64_type.into(),
                ],
                false,
            ),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let int_block = self.context.append_basic_block(function, "int");
        let non_int_block = self.context.append_basic_block(function, "non_int");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let lhs = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let rhs = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let lhs_is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs.tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "lhs_is_int",
            )
            .expect("failed compare lhs_is_int");
        let rhs_is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rhs.tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "rhs_is_int",
            )
            .expect("failed compare rhs_is_int");
        let both_int = self
            .builder
            .build_and(lhs_is_int, rhs_is_int, "both_int")
            .expect("failed compare both_int");
        self.builder
            .build_conditional_branch(both_int, int_block, non_int_block)
            .expect("failed compare int branch");

        self.builder.position_at_end(int_block);
        let int_cmp = self
            .builder
            .build_int_compare(pred, lhs.payload, rhs.payload, "int_cmp")
            .expect("failed to build int compare");
        let int_raw = self
            .builder
            .build_int_z_extend(int_cmp, self.i64_type, "int_cmp_i64")
            .expect("failed to extend int compare");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                int_raw,
                "cmp_result",
            )))
            .expect("failed to return compare");

        self.builder.position_at_end(non_int_block);
        if matches!(pred, IntPredicate::EQ | IntPredicate::NE) {
            let lhs_is_string = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    lhs.tag,
                    self.i64_type.const_int(TAG_STRING as u64, false),
                    "lhs_is_string",
                )
                .expect("failed compare lhs_is_string");
            let rhs_is_string = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    rhs.tag,
                    self.i64_type.const_int(TAG_STRING as u64, false),
                    "rhs_is_string",
                )
                .expect("failed compare rhs_is_string");
            let both_string = self
                .builder
                .build_and(lhs_is_string, rhs_is_string, "both_string")
                .expect("failed compare both_string");
            let any_string = self
                .builder
                .build_or(lhs_is_string, rhs_is_string, "any_string")
                .expect("failed compare any_string");
            let string_block = self.context.append_basic_block(function, "string");
            let string_mixed_block = self.context.append_basic_block(function, "string_mixed");
            let after_string_block = self.context.append_basic_block(function, "after_string");
            self.builder
                .build_conditional_branch(both_string, string_block, string_mixed_block)
                .expect("failed compare string branch");

            self.builder.position_at_end(string_block);
            let string_eq = self.build_string_eq_bytes(lhs.payload, rhs.payload, "string_eq");
            let string_raw = if matches!(pred, IntPredicate::NE) {
                self.builder
                    .build_xor(string_eq, self.i64_type.const_int(1, false), "string_ne")
                    .expect("failed string ne xor")
            } else {
                string_eq
            };
            self.builder
                .build_return(Some(&self.make_pair_value(
                    self.i64_type.const_int(TAG_INT as u64, false),
                    string_raw,
                    "string_cmp_result",
                )))
                .expect("failed to return string compare");

            self.builder.position_at_end(string_mixed_block);
            let mixed_raw = if matches!(pred, IntPredicate::NE) {
                self.i64_type.const_int(1, false)
            } else {
                self.i64_type.const_zero()
            };
            let mixed_done = self.context.append_basic_block(function, "string_mixed_done");
            self.builder
                .build_conditional_branch(any_string, mixed_done, after_string_block)
                .expect("failed compare string mixed short-circuit");
            self.builder.position_at_end(mixed_done);
            self.builder
                .build_return(Some(&self.make_pair_value(
                    self.i64_type.const_int(TAG_INT as u64, false),
                    mixed_raw,
                    "string_mixed_cmp_result",
                )))
                .expect("failed to return string mixed compare");
            self.builder.position_at_end(after_string_block);
        }
        if let Some(bigint_name) = bigint_name {
            let lhs_is_bigint = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    lhs.tag,
                    self.i64_type.const_int(TAG_BIGINT as u64, false),
                    "lhs_is_bigint",
                )
                .expect("failed compare lhs_is_bigint");
            let rhs_is_bigint = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    rhs.tag,
                    self.i64_type.const_int(TAG_BIGINT as u64, false),
                    "rhs_is_bigint",
                )
                .expect("failed compare rhs_is_bigint");
            let both_bigint = self
                .builder
                .build_and(lhs_is_bigint, rhs_is_bigint, "both_bigint")
                .expect("failed compare both_bigint");
            let bigint_block = self.context.append_basic_block(function, "bigint");
            let lhs_promote_check_block =
                self.context.append_basic_block(function, "lhs_promote_check");
            let lhs_promote_block = self.context.append_basic_block(function, "lhs_promote");
            let rhs_maybe_promote_block =
                self.context.append_basic_block(function, "rhs_maybe_promote");
            let rhs_promote_block = self.context.append_basic_block(function, "rhs_promote");
            self.builder
                .build_conditional_branch(both_bigint, bigint_block, lhs_promote_check_block)
                .expect("failed compare bigint branch");

            self.builder.position_at_end(bigint_block);
            let raw_value = self.build_internal_call(
                self.require_func(bigint_name),
                &[lhs, rhs],
                "bigint_cmp_value",
            );
            let bigint_cmp = self
                .builder
                .build_int_compare(
                    pred,
                    raw_value.payload,
                    self.i64_type.const_zero(),
                    "bigint_cmp",
                )
                .expect("failed to build bigint compare");
            let bigint_raw = self
                .builder
                .build_int_z_extend(bigint_cmp, self.i64_type, "bigint_cmp_i64")
                .expect("failed to extend bigint compare");
            self.builder
                .build_return(Some(&self.make_pair_value(
                    self.i64_type.const_int(TAG_INT as u64, false),
                    bigint_raw,
                    "bigint_cmp_result",
                )))
                .expect("failed to return bigint compare");

            self.builder.position_at_end(lhs_promote_check_block);
            let lhs_int_rhs_bigint = self
                .builder
                .build_and(lhs_is_int, rhs_is_bigint, "lhs_int_rhs_bigint")
                .expect("failed compare lhs_int_rhs_bigint");
            self.builder
                .build_conditional_branch(
                    lhs_int_rhs_bigint,
                    lhs_promote_block,
                    rhs_maybe_promote_block,
                )
                .expect("failed compare lhs promote branch");

            self.builder.position_at_end(lhs_promote_block);
            let lhs_big = self.build_internal_call(
                self.require_func("__rt_bigint_from_int"),
                &[lhs],
                "lhs_promoted_bigint",
            );
            let raw_value = self.build_internal_call(
                self.require_func(bigint_name),
                &[lhs_big, rhs],
                "mixed_bigint_cmp_lhs",
            );
            let bigint_cmp = self
                .builder
                .build_int_compare(
                    pred,
                    raw_value.payload,
                    self.i64_type.const_zero(),
                    "mixed_bigint_cmp_lhs_raw",
                )
                .expect("failed to build mixed bigint lhs compare");
            let bigint_raw = self
                .builder
                .build_int_z_extend(bigint_cmp, self.i64_type, "mixed_bigint_cmp_lhs_i64")
                .expect("failed to extend mixed bigint lhs compare");
            self.builder
                .build_return(Some(&self.make_pair_value(
                    self.i64_type.const_int(TAG_INT as u64, false),
                    bigint_raw,
                    "mixed_bigint_cmp_lhs_result",
                )))
                .expect("failed to return mixed bigint lhs compare");

            self.builder.position_at_end(rhs_maybe_promote_block);
            let rhs_int_lhs_bigint = self
                .builder
                .build_and(lhs_is_bigint, rhs_is_int, "rhs_int_lhs_bigint")
                .expect("failed compare rhs_int_lhs_bigint");
            self.builder
                .build_conditional_branch(rhs_int_lhs_bigint, rhs_promote_block, trap_block)
                .expect("failed compare rhs maybe promote branch");

            self.builder.position_at_end(rhs_promote_block);
            let rhs_big = self.build_internal_call(
                self.require_func("__rt_bigint_from_int"),
                &[rhs],
                "rhs_promoted_bigint",
            );
            let raw_value = self.build_internal_call(
                self.require_func(bigint_name),
                &[lhs, rhs_big],
                "mixed_bigint_cmp_rhs",
            );
            let bigint_cmp = self
                .builder
                .build_int_compare(
                    pred,
                    raw_value.payload,
                    self.i64_type.const_zero(),
                    "mixed_bigint_cmp_rhs_raw",
                )
                .expect("failed to build mixed bigint rhs compare");
            let bigint_raw = self
                .builder
                .build_int_z_extend(bigint_cmp, self.i64_type, "mixed_bigint_cmp_rhs_i64")
                .expect("failed to extend mixed bigint rhs compare");
            self.builder
                .build_return(Some(&self.make_pair_value(
                    self.i64_type.const_int(TAG_INT as u64, false),
                    bigint_raw,
                    "mixed_bigint_cmp_rhs_result",
                )))
                .expect("failed to return mixed bigint rhs compare");
        } else {
            self.builder
                .build_unconditional_branch(trap_block)
                .expect("failed compare trap branch");
        }

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_boxed_runtime_pair_wrapper(
        &mut self,
        name: &str,
        symbol: &str,
        host_name: &str,
        arg_count: usize,
    ) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(&vec![self.i64_type.into(); arg_count * 2], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let mut args = Vec::with_capacity(arg_count);
        for index in 0..arg_count {
            args.push(CompiledValue {
                tag: function.get_nth_param((index * 2) as u32).unwrap().into_int_value(),
                payload: function.get_nth_param((index * 2 + 1) as u32).unwrap().into_int_value(),
            });
        }

        let boxed_args = args
            .iter()
            .map(|value| self.box_compiled_value(*value, &format!("{symbol}_arg")))
            .collect::<Vec<_>>();
        let host = self.require_func(host_name);
        let handle = self.build_boxed_call(host, &boxed_args, symbol);
        let result = self.unbox_handle(handle, symbol);
        self.builder
            .build_return(Some(&self.make_pair_value(
                result.tag,
                result.payload,
                &format!("{symbol}_result"),
            )))
            .expect("failed to return boxed runtime pair wrapper");
    }

    fn define_direct_pair_print_wrapper(&mut self, name: &str, symbol: &str, import_name: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let tag = function.get_first_param().unwrap().into_int_value();
        let payload = function.get_nth_param(1).unwrap().into_int_value();
        let import_fn = self.require_func(import_name);
        self.builder
            .build_call(import_fn, &[tag.into(), payload.into()], "wasm_print")
            .expect("failed to call wasm print import");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                self.i64_type.const_zero(),
                &format!("{symbol}_result"),
            )))
            .expect("failed to return direct print wrapper");
    }

    fn define_wasm_multi3(&mut self) {
        let ptr_type = self.context.ptr_type(Default::default());
        let void_type = self.context.void_type();
        let function = self.module.add_function(
            "__multi3",
            void_type.fn_type(
                &[
                    ptr_type.into(),
                    self.i64_type.into(),
                    self.i64_type.into(),
                    self.i64_type.into(),
                    self.i64_type.into(),
                ],
                false,
            ),
            Some(Linkage::Internal),
        );
        self.functions.insert("__multi3".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let dst = function.get_first_param().unwrap().into_pointer_value();
        let a_lo = function.get_nth_param(1).unwrap().into_int_value();
        let a_hi = function.get_nth_param(2).unwrap().into_int_value();
        let b_lo = function.get_nth_param(3).unwrap().into_int_value();
        let b_hi = function.get_nth_param(4).unwrap().into_int_value();

        let (lo, carry) = self.build_u64_mul_wide(a_lo, b_lo, "multi3_ll");
        let mid1 = self
            .builder
            .build_int_mul(a_lo, b_hi, "multi3_mid1")
            .expect("failed to multiply multi3 mid1");
        let mid2 = self
            .builder
            .build_int_mul(a_hi, b_lo, "multi3_mid2")
            .expect("failed to multiply multi3 mid2");
        let high = self
            .builder
            .build_int_add(carry, mid1, "multi3_high_partial")
            .expect("failed to add multi3 carry/mid1");
        let high = self
            .builder
            .build_int_add(high, mid2, "multi3_high")
            .expect("failed to add multi3 mid2");

        let i64_ptr = self
            .builder
            .build_pointer_cast(dst, self.context.ptr_type(Default::default()), "multi3_i64_ptr")
            .expect("failed to cast multi3 dst ptr");
        let zero32 = self.context.i32_type().const_zero();
        let one32 = self.context.i32_type().const_int(1, false);
        let lo_ptr = unsafe {
            self.builder
                .build_gep(self.i64_type, i64_ptr, &[zero32], "multi3_lo_ptr")
                .expect("failed to gep multi3 lo ptr")
        };
        let hi_ptr = unsafe {
            self.builder
                .build_gep(self.i64_type, i64_ptr, &[one32], "multi3_hi_ptr")
                .expect("failed to gep multi3 hi ptr")
        };
        self.builder.build_store(lo_ptr, lo).expect("failed to store multi3 lo");
        self.builder.build_store(hi_ptr, high).expect("failed to store multi3 hi");
        self.builder.build_return(None).expect("failed to return from multi3");
    }

    fn build_u64_mul_wide(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        label: &str,
    ) -> (IntValue<'ctx>, IntValue<'ctx>) {
        let mask32 = self.i64_type.const_int(0xffff_ffff, false);
        let a0 = self
            .builder
            .build_and(lhs, mask32, &format!("{label}_a0"))
            .expect("failed to mask lhs lo32");
        let a1 = self
            .builder
            .build_right_shift(
                lhs,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_a1"),
            )
            .expect("failed to shift lhs hi32");
        let b0 = self
            .builder
            .build_and(rhs, mask32, &format!("{label}_b0"))
            .expect("failed to mask rhs lo32");
        let b1 = self
            .builder
            .build_right_shift(
                rhs,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_b1"),
            )
            .expect("failed to shift rhs hi32");

        let p0 =
            self.builder.build_int_mul(a0, b0, &format!("{label}_p0")).expect("failed to mul p0");
        let p1 =
            self.builder.build_int_mul(a0, b1, &format!("{label}_p1")).expect("failed to mul p1");
        let p2 =
            self.builder.build_int_mul(a1, b0, &format!("{label}_p2")).expect("failed to mul p2");
        let p3 =
            self.builder.build_int_mul(a1, b1, &format!("{label}_p3")).expect("failed to mul p3");

        let p0_hi = self
            .builder
            .build_right_shift(
                p0,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_p0_hi"),
            )
            .expect("failed to shift p0 hi");
        let p1_lo = self
            .builder
            .build_and(p1, mask32, &format!("{label}_p1_lo"))
            .expect("failed to mask p1 lo");
        let p2_lo = self
            .builder
            .build_and(p2, mask32, &format!("{label}_p2_lo"))
            .expect("failed to mask p2 lo");
        let middle = self
            .builder
            .build_int_add(p0_hi, p1_lo, &format!("{label}_middle_1"))
            .expect("failed to add middle 1");
        let middle = self
            .builder
            .build_int_add(middle, p2_lo, &format!("{label}_middle"))
            .expect("failed to add middle 2");

        let low_lo = self
            .builder
            .build_and(p0, mask32, &format!("{label}_low_lo"))
            .expect("failed to mask low lo");
        let middle_lo = self
            .builder
            .build_and(middle, mask32, &format!("{label}_middle_lo"))
            .expect("failed to mask middle lo");
        let middle_lo_shifted = self
            .builder
            .build_left_shift(
                middle_lo,
                self.i64_type.const_int(32, false),
                &format!("{label}_middle_lo_shifted"),
            )
            .expect("failed to shift middle lo");
        let low = self
            .builder
            .build_or(low_lo, middle_lo_shifted, &format!("{label}_low"))
            .expect("failed to build low");

        let p1_hi = self
            .builder
            .build_right_shift(
                p1,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_p1_hi"),
            )
            .expect("failed to shift p1 hi");
        let p2_hi = self
            .builder
            .build_right_shift(
                p2,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_p2_hi"),
            )
            .expect("failed to shift p2 hi");
        let middle_hi = self
            .builder
            .build_right_shift(
                middle,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_middle_hi"),
            )
            .expect("failed to shift middle hi");
        let high = self
            .builder
            .build_int_add(p3, p1_hi, &format!("{label}_high_1"))
            .expect("failed to add high 1");
        let high = self
            .builder
            .build_int_add(high, p2_hi, &format!("{label}_high_2"))
            .expect("failed to add high 2");
        let high = self
            .builder
            .build_int_add(high, middle_hi, &format!("{label}_high"))
            .expect("failed to add high 3");

        (low, high)
    }

    fn define_wasm_allocator(&mut self, name: &str, symbol: &str) {
        let offset = self.module.add_global(self.i64_type, None, "__llvm_wasm_arena_offset");
        offset.set_linkage(Linkage::Internal);
        offset.set_initializer(&self.i64_type.const_int(WASM_ARENA_BASE as u64, false));
        let _ = offset.set_alignment(8);

        let function = self.module.add_function(
            symbol,
            self.i64_type.fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let size = function.get_first_param().unwrap().into_int_value();
        let align = function.get_nth_param(1).unwrap().into_int_value();
        let zero = self.i64_type.const_zero();
        let align_non_zero = self
            .builder
            .build_int_compare(IntPredicate::NE, align, zero, "alloc_align_non_zero")
            .expect("failed to compare wasm alloc align");
        let old_offset = self
            .builder
            .build_load(self.i64_type, offset.as_pointer_value(), "alloc_old_offset")
            .expect("failed to load wasm alloc offset")
            .into_int_value();
        let align_minus_one = self
            .builder
            .build_int_sub(align, self.i64_type.const_int(1, false), "alloc_align_minus_one")
            .expect("failed to compute wasm alloc align");
        let start_plus = self
            .builder
            .build_int_add(old_offset, align_minus_one, "alloc_start_plus")
            .expect("failed to compute wasm alloc start");
        let mask = self
            .builder
            .build_not(align_minus_one, "alloc_mask")
            .expect("failed to compute wasm alloc mask");
        let aligned = self
            .builder
            .build_and(start_plus, mask, "alloc_aligned")
            .expect("failed to align wasm alloc");
        let end = self
            .builder
            .build_int_add(aligned, size, "alloc_end")
            .expect("failed to compute wasm alloc end");
        let fits = self
            .builder
            .build_int_compare(
                IntPredicate::ULE,
                end,
                self.i64_type.const_int(WASM_ARENA_BYTES as u64, false),
                "alloc_fits",
            )
            .expect("failed to compare wasm alloc bounds");
        let ok = self
            .builder
            .build_and(align_non_zero, fits, "alloc_ok")
            .expect("failed to build wasm alloc guard");
        self.builder
            .build_conditional_branch(ok, ok_block, trap_block)
            .expect("failed to branch in wasm allocator");

        self.builder.position_at_end(ok_block);
        self.builder
            .build_store(offset.as_pointer_value(), end)
            .expect("failed to store wasm alloc offset");
        self.builder.build_return(Some(&aligned)).expect("failed to return wasm alloc ptr");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn build_trap_and_unreachable(&self) {
        let void_type = self.context.void_type();
        let trap_fn = self.module.get_function("llvm.trap").unwrap_or_else(|| {
            self.module.add_function("llvm.trap", void_type.fn_type(&[], false), None)
        });
        self.builder.build_call(trap_fn, &[], "trap").expect("failed to build trap call");
        self.builder.build_unreachable().expect("failed to build unreachable");
    }

    fn invert_i1(&self, value: IntValue<'ctx>, name: &str) -> IntValue<'ctx> {
        let one = self.context.bool_type().const_all_ones();
        self.builder.build_xor(value, one, name).expect("failed to invert i1")
    }

    fn build_division_safe_check(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        prefix: &str,
    ) -> IntValue<'ctx> {
        let zero = self.i64_type.const_zero();
        let rhs_non_zero = self
            .builder
            .build_int_compare(IntPredicate::NE, rhs, zero, &format!("{prefix}_rhs_non_zero"))
            .expect("failed to compare rhs");
        let min_i64 = self.i64_type.const_int(i64::MIN as u64, true);
        let neg_one = self.i64_type.const_all_ones();
        let lhs_is_min = self
            .builder
            .build_int_compare(IntPredicate::EQ, lhs, min_i64, &format!("{prefix}_lhs_is_min"))
            .expect("failed to compare lhs min");
        let rhs_is_neg_one = self
            .builder
            .build_int_compare(IntPredicate::EQ, rhs, neg_one, &format!("{prefix}_rhs_is_neg_one"))
            .expect("failed to compare rhs neg one");
        let overflow = self
            .builder
            .build_and(lhs_is_min, rhs_is_neg_one, &format!("{prefix}_overflow"))
            .expect("failed to build div overflow");
        self.builder
            .build_and(
                rhs_non_zero,
                self.invert_i1(overflow, &format!("{prefix}_ok")),
                &format!("{prefix}_safe"),
            )
            .expect("failed to build div ok")
    }

    fn build_overflow_intrinsic_call(
        &self,
        intrinsic_name: &str,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        label: &str,
    ) -> (IntValue<'ctx>, IntValue<'ctx>) {
        let result_type = self
            .context
            .struct_type(&[self.i64_type.into(), self.context.bool_type().into()], false);
        let function = self.module.get_function(intrinsic_name).unwrap_or_else(|| {
            self.module.add_function(
                intrinsic_name,
                result_type.fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
                None,
            )
        });
        let call = self
            .builder
            .build_call(function, &[lhs.into(), rhs.into()], &format!("{label}_overflow"))
            .expect("failed to build intrinsic call")
            .try_as_basic_value()
            .unwrap_basic()
            .into_struct_value();
        let value = self
            .builder
            .build_extract_value(call, 0, &format!("{label}_value"))
            .expect("failed to extract value")
            .into_int_value();
        let overflow = self
            .builder
            .build_extract_value(call, 1, &format!("{label}_overflow_flag"))
            .expect("failed to extract overflow")
            .into_int_value();
        (value, overflow)
    }
}

#[derive(Clone, Copy)]
enum BinaryArithOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
}

fn internal_symbol_name(name: &str) -> String {
    format!("__expr_internal_{name}")
}

fn int_result_symbol_name(name: &str, mode: LlvmOutputMode) -> String {
    if name == "main" && matches!(mode, LlvmOutputMode::Executable) {
        #[cfg(windows)]
        {
            return "expr_main_entry_int".to_string();
        }
        #[cfg(not(windows))]
        {
            return "__expr_main_i64".to_string();
        }
    }

    if name == "main" && matches!(mode, LlvmOutputMode::Wasm) {
        return "__expr_main_i64".to_string();
    }

    #[cfg(feature = "wasi")]
    if name == "main" && matches!(mode, LlvmOutputMode::WasiPreview1Command) {
        return "__expr_main_i64".to_string();
    }

    format!("__expr_i64_{name}")
}

fn install_runtime_mappings<'ctx>(
    functions: &HashMap<String, FunctionValue<'ctx>>,
    execution_engine: &ExecutionEngine<'ctx>,
) {
    let mappings = [
        ("print", crate::runtime::__expr_print_host as usize),
        ("list_print", crate::runtime::__expr_list_print_host as usize),
        ("__box_value", crate::runtime::__expr_box_value_host as usize),
        ("__alloc", crate::runtime::__expr_alloc_host as usize),
    ];

    for (name, addr) in mappings {
        let function =
            functions.get(name).unwrap_or_else(|| panic!("missing function declaration: {name}"));
        execution_engine.add_global_mapping(function, addr);
    }
}
