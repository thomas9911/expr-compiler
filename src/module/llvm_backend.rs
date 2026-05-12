use super::{
    ClosureMetadata, Module, function_arities, function_ordinals, is_builtin_name, local_var_names,
};
use crate::parser::{Ast, ExpressionAst, FunctionDefAst, LiteralAst};
use crate::value::{
    BIGINT_HEADER_SIZE, BIGINT_LIMB_SIZE, CLOSURE_SIZE, TAG_BIGINT, TAG_FUNCTION, TAG_INT,
    TAG_LIST, VALUE_SIZE,
};
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
#[cfg(feature = "wasi")]
use inkwell::attributes::AttributeLoc;
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
        self.function_names
            .iter()
            .filter(|name| !is_builtin_name(name))
            .map(|name| name.as_str())
    }

    pub fn get_int_result_fn_ptr(&self, name: &str) -> Option<*const u8> {
        if !self.int_result_function_names.contains(name) {
            return None;
        }
        let symbol = int_result_symbol_name(name, LlvmOutputMode::Jit);
        let addr = self
            .execution_engine
            .get_function_address(&symbol)
            .unwrap_or_else(|e| {
                panic!("unable to find LLVM JIT int-result function '{symbol}': {e}")
            });
        Some(addr as usize as *const u8)
    }
}

pub(super) fn compile_to_jit(expr_module: Module) -> LlvmJitModule {
    Target::initialize_native(&InitializationConfig::default())
        .unwrap_or_else(|e| panic!("failed to initialize LLVM native target: {e}"));

    let (context, module, _machine) = create_codegen_context("expr", LlvmTargetKind::Host);

    let int_result_function_names = expr_module
        .functions
        .iter()
        .filter(|func| func.inputs.is_empty())
        .map(|func| func.name.clone())
        .collect::<HashSet<_>>();

    let functions = {
        let mut compiler = LlvmCompiler::new(context, module, LlvmRuntimeMode::Native);
        compiler.bigint_enabled = expr_module.uses_bigint();
        compiler.list_enabled = expr_module.uses_lists();
        compiler.list_mutation_enabled = expr_module.uses_list_mutation();
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
        compiler.closure_metadata = expr_module.closure_metadata.clone();
        compiler.declare_runtime_functions();
        compiler.declare_user_functions(&expr_module.functions, LlvmOutputMode::Jit);
        compiler.define_user_functions(&expr_module.functions);
        compiler.define_int_result_wrappers(&expr_module.functions, LlvmOutputMode::Jit);
        compiler.into_functions()
    };

    module
        .verify()
        .unwrap_or_else(|e| panic!("invalid LLVM module: {e}"));

    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap_or_else(|e| panic!("failed to create LLVM execution engine: {e}"));
    install_runtime_mappings(&functions, &execution_engine);

    LlvmJitModule {
        _context: context,
        _module: module,
        execution_engine,
        function_names: expr_module
            .functions
            .iter()
            .map(|func| func.name.clone())
            .collect(),
        int_result_function_names,
    }
}

pub(super) fn compile_to_object(expr_module: Module, name: &str) -> Vec<u8> {
    Target::initialize_native(&InitializationConfig::default())
        .unwrap_or_else(|e| panic!("failed to initialize LLVM native target: {e}"));

    let (context, module, machine) = create_codegen_context(name, LlvmTargetKind::Host);
    {
        let mut compiler = LlvmCompiler::new(context, module, LlvmRuntimeMode::Native);
        compiler.bigint_enabled = expr_module.uses_bigint();
        compiler.list_enabled = expr_module.uses_lists();
        compiler.list_mutation_enabled = expr_module.uses_list_mutation();
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
        compiler.closure_metadata = expr_module.closure_metadata.clone();
        compiler.declare_runtime_functions();
        compiler.declare_user_functions(&expr_module.functions, LlvmOutputMode::Executable);
        compiler.define_user_functions(&expr_module.functions);
        compiler.define_int_result_wrappers(&expr_module.functions, LlvmOutputMode::Executable);
    }

    module
        .verify()
        .unwrap_or_else(|e| panic!("invalid LLVM module: {e}"));

    let buffer = machine
        .write_to_memory_buffer(module, FileType::Object)
        .unwrap_or_else(|e| panic!("failed to emit LLVM object: {e}"));
    buffer.as_slice().to_vec()
}

pub(super) fn compile_to_wasm_assembly(expr_module: Module, name: &str) -> Vec<u8> {
    Target::initialize_webassembly(&InitializationConfig::default());

    let (context, module, machine) = create_codegen_context(name, LlvmTargetKind::Wasm);
    {
        let mut compiler = LlvmCompiler::new(context, module, LlvmRuntimeMode::Wasm);
        compiler.bigint_enabled = expr_module.uses_bigint();
        compiler.list_enabled = expr_module.uses_lists();
        compiler.list_mutation_enabled = expr_module.uses_list_mutation();
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
        compiler.closure_metadata = expr_module.closure_metadata.clone();
        compiler.declare_runtime_functions();
        compiler.declare_user_functions(&expr_module.functions, LlvmOutputMode::Wasm);
        compiler.define_user_functions(&expr_module.functions);
        compiler.define_int_result_wrappers(&expr_module.functions, LlvmOutputMode::Wasm);
    }

    module
        .verify()
        .unwrap_or_else(|e| panic!("invalid LLVM module: {e}"));

    let buffer = machine
        .write_to_memory_buffer(module, FileType::Assembly)
        .unwrap_or_else(|e| panic!("failed to emit LLVM wasm assembly: {e}"));
    buffer.as_slice().to_vec()
}

#[cfg(feature = "wasi")]
pub(super) fn compile_to_wasm_preview1_command_assembly(
    expr_module: Module,
    name: &str,
) -> Vec<u8> {
    Target::initialize_webassembly(&InitializationConfig::default());

    let (context, module, machine) = create_codegen_context(name, LlvmTargetKind::Wasm);
    {
        let mut compiler = LlvmCompiler::new(context, module, LlvmRuntimeMode::WasiPreview1Command);
        compiler.bigint_enabled = expr_module.uses_bigint();
        compiler.list_enabled = expr_module.uses_lists();
        compiler.list_mutation_enabled = expr_module.uses_list_mutation();
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
        compiler.closure_metadata = expr_module.closure_metadata.clone();
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

    module
        .verify()
        .unwrap_or_else(|e| panic!("invalid LLVM module: {e}"));

    let buffer = machine
        .write_to_memory_buffer(module, FileType::Assembly)
        .unwrap_or_else(|e| panic!("failed to emit LLVM preview1 command assembly: {e}"));
    buffer.as_slice().to_vec()
}

fn create_codegen_context(
    module_name: &str,
    target_kind: LlvmTargetKind,
) -> (
    &'static Context,
    &'static LlvmModule<'static>,
    TargetMachine,
) {
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
}

#[derive(Clone, Copy)]
struct CompiledValue<'ctx> {
    tag: IntValue<'ctx>,
    payload: IntValue<'ctx>,
}

impl<'ctx> LlvmCompiler<'ctx> {
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
        }
    }

    fn into_functions(self) -> HashMap<String, FunctionValue<'ctx>> {
        self.functions
    }

    #[cfg(feature = "wasi")]
    fn declare_wasi_preview1_import(&mut self, name: &str, import_name: &str) {
        let i32_type = self.context.i32_type();
        let function = match import_name {
            "fd_write" => self.module.add_function(
                name,
                i32_type.fn_type(
                    &[
                        i32_type.into(),
                        i32_type.into(),
                        i32_type.into(),
                        i32_type.into(),
                    ],
                    false,
                ),
                None,
            ),
            "proc_exit" => self.module.add_function(
                name,
                self.context.void_type().fn_type(&[i32_type.into()], false),
                None,
            ),
            other => panic!("unsupported WASI Preview 1 import: {other}"),
        };

        let import_module = self
            .context
            .create_string_attribute("wasm-import-module", "wasi_snapshot_preview1");
        let import_name_attr = self
            .context
            .create_string_attribute("wasm-import-name", import_name);
        function.add_attribute(AttributeLoc::Function, import_module);
        function.add_attribute(AttributeLoc::Function, import_name_attr);
        self.functions.insert(name.to_string(), function);
    }

    fn declare_runtime_functions(&mut self) {
        let i64_type = self.i64_type;
        match self.runtime_mode {
            LlvmRuntimeMode::Native => {
                let runtime = [
                    ("print", "__expr_print_host", vec![i64_type.into()]),
                    (
                        "list_print",
                        "__expr_list_print_host",
                        vec![i64_type.into()],
                    ),
                    (
                        "__box_value",
                        "__expr_box_value_host",
                        vec![i64_type.into(), i64_type.into()],
                    ),
                    (
                        "__alloc",
                        "__expr_alloc_host",
                        vec![i64_type.into(), i64_type.into()],
                    ),
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

                self.define_wasm_allocator("__alloc", "llvm_wasm_alloc");
                self.define_wasm_multi3();
            }
            #[cfg(feature = "wasi")]
            LlvmRuntimeMode::WasiPreview1Command => {
                self.declare_wasi_preview1_import("__wasi_fd_write", "fd_write");
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
            if func.inputs.is_empty() {
                self.define_int_result_wrapper(func, mode);
            }
        }
    }

    #[cfg(feature = "wasi")]
    fn define_wasi_preview1_command_start_wrapper(&self) {
        let _ = self
            .functions
            .get("main")
            .copied()
            .expect("missing main function for wasi command wrapper");
        let int_wrapper_name = int_result_symbol_name("main", LlvmOutputMode::WasiPreview1Command);
        let int_wrapper = self
            .module
            .get_function(&int_wrapper_name)
            .unwrap_or_else(|| panic!("missing int-result wrapper: {int_wrapper_name}"));
        let function = self.module.add_function(
            "_start",
            self.context.void_type().fn_type(&[], false),
            Some(Linkage::External),
        );
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let exit_code_i64 = self
            .builder
            .build_call(int_wrapper, &[], "wasi_main_exit_code")
            .expect("failed to call int-result wrapper")
            .try_as_basic_value()
            .unwrap_basic()
            .into_int_value();
        let exit_code = self
            .builder
            .build_int_truncate(
                exit_code_i64,
                self.context.i32_type(),
                "wasi_main_exit_code_i32",
            )
            .expect("failed to truncate exit code");
        let proc_exit = self.require_func("__wasi_proc_exit");
        self.builder
            .build_call(proc_exit, &[exit_code.into()], "wasi_proc_exit")
            .expect("failed to call proc_exit");
        self.builder
            .build_return(None)
            .expect("failed to return from _start");
    }

    fn define_int_result_wrapper(&self, func_def: &FunctionDefAst, mode: LlvmOutputMode) {
        let symbol = int_result_symbol_name(&func_def.name, mode);
        let linkage = Some(Linkage::External);
        let function =
            self.module
                .add_function(&symbol, self.i64_type.fn_type(&[], false), linkage);
        let internal = self.require_func(&func_def.name);
        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let value = self.build_user_call(
            internal,
            self.i64_type.const_zero(),
            &[],
            "int_result_value",
        );
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
        let env_slot = self
            .builder
            .build_alloca(self.i64_type, "env")
            .expect("failed to allocate env slot");
        let initial_env = function.get_first_param().unwrap().into_int_value();
        self.builder
            .build_store(env_slot, initial_env)
            .expect("failed to store initial env");
        for (index, name) in func_def.inputs.iter().enumerate() {
            let ptr = self
                .builder
                .build_alloca(self.pair_type(), name)
                .expect("failed to allocate function param");
            let tag = function
                .get_nth_param((index * 2 + 1) as u32)
                .unwrap_or_else(|| panic!("missing tag param {index} for {}", func_def.name))
                .into_int_value();
            let payload = function
                .get_nth_param((index * 2 + 2) as u32)
                .unwrap_or_else(|| panic!("missing payload param {index} for {}", func_def.name))
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

        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to branch to user loop");

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
        let trap_block = self
            .context
            .append_basic_block(function, &format!("{label}_trap"));
        let merge_block = self
            .context
            .append_basic_block(function, &format!("{label}_merge"));

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
            panic!("no unary functions are available for higher-order list builtins");
        }

        let first_check = self
            .context
            .append_basic_block(function, &format!("{label}_check0"));
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
            let call_block = self
                .context
                .append_basic_block(function, &format!("{label}_call_{index}"));
            let next_block = if index + 1 == candidates.len() {
                trap_block
            } else {
                self.context
                    .append_basic_block(function, &format!("{label}_check_{}", index + 1))
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
            let result_block = self
                .builder
                .get_insert_block()
                .expect("missing callback result block");
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
                (
                    &value.payload as &dyn inkwell::values::BasicValue<'ctx>,
                    *block,
                )
            })
            .collect();
        tag_phi.add_incoming(&tag_incoming);
        payload_phi.add_incoming(&payload_incoming);
        CompiledValue {
            tag: tag_phi.as_basic_value().into_int_value(),
            payload: payload_phi.as_basic_value().into_int_value(),
        }
    }

    fn validate_unary_callback_ast(
        &self,
        ast: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        builtin: &str,
    ) {
        match ast {
            Ast::FunctionRef(name) => {
                if self.function_arities.get(name) != Some(&1usize) {
                    panic!("{builtin} callback must take exactly 1 argument");
                }
            }
            Ast::Variable(name)
                if !vars.contains_key(name) && self.function_ordinals.contains_key(name) =>
            {
                if self.function_arities.get(name) != Some(&1usize) {
                    panic!("{builtin} callback must take exactly 1 argument");
                }
            }
            _ => {}
        }
    }

    fn compile_list_map(
        &self,
        args: &[Ast],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        assert_eq!(args.len(), 2, "list_map expects 2 arguments");
        self.validate_unary_callback_ast(&args[1], vars, "list_map");
        let input = self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
        let callback = self.compile_ast(&args[1], vars, capture_slots, env_ptr, function);
        let output =
            self.build_internal_call(self.require_func("__rt_list_new"), &[], "list_map_new");
        let len =
            self.build_internal_call(self.require_func("__rt_list_len"), &[input], "list_map_len");

        let loop_block = self.context.append_basic_block(function, "list_map_loop");
        let body_block = self.context.append_basic_block(function, "list_map_body");
        let latch_block = self.context.append_basic_block(function, "list_map_latch");
        let exit_block = self.context.append_basic_block(function, "list_map_exit");
        let entry_block = self
            .builder
            .get_insert_block()
            .expect("missing list_map entry block");

        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to branch to list_map loop");

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, "list_map_idx")
            .expect("failed to build list_map idx phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_block)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let has_more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len.payload, "list_map_has_more")
            .expect("failed to compare list_map idx");
        self.builder
            .build_conditional_branch(has_more, body_block, exit_block)
            .expect("failed to branch in list_map loop");

        self.builder.position_at_end(body_block);
        let index_value = self.int_value(idx);
        let item = self.build_internal_call(
            self.require_func("__rt_list_get"),
            &[input, index_value],
            "list_map_get",
        );
        let mapped = self.apply_function_value(callback, &[item], function, "list_map");
        let _ = self.build_internal_call(
            self.require_func("__rt_list_push"),
            &[output, mapped],
            "list_map_push",
        );
        self.builder
            .build_unconditional_branch(latch_block)
            .expect("failed to branch to list_map latch");

        self.builder.position_at_end(latch_block);
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "list_map_next")
            .expect("failed to increment list_map idx");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to jump to list_map loop");
        idx_phi.add_incoming(&[(&next, latch_block)]);

        self.builder.position_at_end(exit_block);
        output
    }

    fn compile_list_filter(
        &self,
        args: &[Ast],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        assert_eq!(args.len(), 2, "list_filter expects 2 arguments");
        self.validate_unary_callback_ast(&args[1], vars, "list_filter");
        let input = self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
        let callback = self.compile_ast(&args[1], vars, capture_slots, env_ptr, function);
        let output =
            self.build_internal_call(self.require_func("__rt_list_new"), &[], "list_filter_new");
        let len = self.build_internal_call(
            self.require_func("__rt_list_len"),
            &[input],
            "list_filter_len",
        );

        let loop_block = self
            .context
            .append_basic_block(function, "list_filter_loop");
        let body_block = self
            .context
            .append_basic_block(function, "list_filter_body");
        let push_block = self
            .context
            .append_basic_block(function, "list_filter_push");
        let skip_block = self
            .context
            .append_basic_block(function, "list_filter_skip");
        let continue_block = self
            .context
            .append_basic_block(function, "list_filter_continue");
        let latch_block = self
            .context
            .append_basic_block(function, "list_filter_latch");
        let exit_block = self
            .context
            .append_basic_block(function, "list_filter_exit");
        let entry_block = self
            .builder
            .get_insert_block()
            .expect("missing list_filter entry block");

        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to branch to list_filter loop");

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, "list_filter_idx")
            .expect("failed to build list_filter idx phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_block)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let has_more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len.payload, "list_filter_has_more")
            .expect("failed to compare list_filter idx");
        self.builder
            .build_conditional_branch(has_more, body_block, exit_block)
            .expect("failed to branch in list_filter loop");

        self.builder.position_at_end(body_block);
        let index_value = self.int_value(idx);
        let item = self.build_internal_call(
            self.require_func("__rt_list_get"),
            &[input, index_value],
            "list_filter_get",
        );
        let predicate = self.apply_function_value(callback, &[item], function, "list_filter");
        let truth = self.build_internal_scalar_call(
            self.require_func("__value_is_truthy"),
            &[predicate],
            "list_filter_truth",
        );
        let keep = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                truth,
                self.i64_type.const_zero(),
                "list_filter_keep",
            )
            .expect("failed to compare list_filter truth");
        self.builder
            .build_conditional_branch(keep, push_block, skip_block)
            .expect("failed to branch in list_filter body");

        self.builder.position_at_end(push_block);
        let _ = self.build_internal_call(
            self.require_func("__rt_list_push"),
            &[output, item],
            "list_filter_push",
        );
        self.builder
            .build_unconditional_branch(continue_block)
            .expect("failed to branch from list_filter push");

        self.builder.position_at_end(skip_block);
        self.builder
            .build_unconditional_branch(continue_block)
            .expect("failed to branch from list_filter skip");

        self.builder.position_at_end(continue_block);
        self.builder
            .build_unconditional_branch(latch_block)
            .expect("failed to branch to list_filter latch");

        self.builder.position_at_end(latch_block);
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "list_filter_next")
            .expect("failed to increment list_filter idx");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to jump to list_filter loop");
        idx_phi.add_incoming(&[(&next, latch_block)]);

        self.builder.position_at_end(exit_block);
        output
    }

    fn compile_list_range(
        &self,
        args: &[Ast],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        assert_eq!(args.len(), 2, "list_range expects 2 arguments");
        let start_value = self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
        let end_value = self.compile_ast(&args[1], vars, capture_slots, env_ptr, function);
        let start = self.build_internal_scalar_call(
            self.require_func("__value_to_i64"),
            &[start_value],
            "list_range_start",
        );
        let end = self.build_internal_scalar_call(
            self.require_func("__value_to_i64"),
            &[end_value],
            "list_range_end",
        );
        let output =
            self.build_internal_call(self.require_func("__rt_list_new"), &[], "list_range_new");

        let loop_block = self.context.append_basic_block(function, "list_range_loop");
        let body_block = self.context.append_basic_block(function, "list_range_body");
        let latch_block = self
            .context
            .append_basic_block(function, "list_range_latch");
        let exit_block = self.context.append_basic_block(function, "list_range_exit");
        let entry_block = self
            .builder
            .get_insert_block()
            .expect("missing list_range entry block");

        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to branch to list_range loop");

        self.builder.position_at_end(loop_block);
        let current_phi = self
            .builder
            .build_phi(self.i64_type, "list_range_current")
            .expect("failed to build list_range current phi");
        current_phi.add_incoming(&[(&start, entry_block)]);
        let current = current_phi.as_basic_value().into_int_value();
        let has_more = self
            .builder
            .build_int_compare(IntPredicate::SLT, current, end, "list_range_has_more")
            .expect("failed to compare list_range bounds");
        self.builder
            .build_conditional_branch(has_more, body_block, exit_block)
            .expect("failed to branch in list_range loop");

        self.builder.position_at_end(body_block);
        let current_value = self.int_value(current);
        let _ = self.build_internal_call(
            self.require_func("__rt_list_push"),
            &[output, current_value],
            "list_range_push",
        );
        self.builder
            .build_unconditional_branch(latch_block)
            .expect("failed to branch to list_range latch");

        self.builder.position_at_end(latch_block);
        let next = self
            .builder
            .build_int_add(
                current,
                self.i64_type.const_int(1, false),
                "list_range_next",
            )
            .expect("failed to increment list_range value");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to jump to list_range loop");
        current_phi.add_incoming(&[(&next, latch_block)]);

        self.builder.position_at_end(exit_block);
        output
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
            let _ = self.compile_ast(line, vars, capture_slots, current_env, function);
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
            Ast::Expression(ExpressionAst {
                function: name,
                args,
            }) if name == current_function_name && !is_builtin_name(name) => {
                let compiled = args
                    .iter()
                    .map(|arg| self.compile_ast(arg, vars, capture_slots, env_ptr, function))
                    .collect::<Vec<_>>();
                for (index, value) in compiled.iter().enumerate() {
                    let ptr = vars
                        .get(&current_function_inputs[index])
                        .unwrap_or_else(|| {
                            panic!("missing param slot {index} for {current_function_name}")
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
            Ast::If {
                condition,
                then,
                else_,
            } => {
                let cond_value =
                    self.compile_ast(condition, vars, capture_slots, env_ptr, function);
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
                let value = self.compile_ast(ast, vars, capture_slots, env_ptr, function);
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

    fn compile_ast(
        &self,
        ast: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        match ast {
            Ast::Literal(LiteralAst::Integer(n)) => {
                self.int_value(self.i64_type.const_int(*n as u64, true))
            }
            Ast::Literal(LiteralAst::BigInt(digits)) => {
                self.build_bigint_literal(digits, "bigint_literal")
            }
            Ast::Lambda { .. } => {
                panic!("anonymous functions are not implemented by the llvm backend yet");
            }
            Ast::FunctionRef(name) => {
                self.allocate_closure_for_function(name, vars, capture_slots, env_ptr, function)
            }
            Ast::ListLiteral(items) => {
                let list =
                    self.build_internal_call(self.require_func("__rt_list_new"), &[], "list_new");
                for item in items {
                    let value = self.compile_ast(item, vars, capture_slots, env_ptr, function);
                    let _ = self.build_internal_call(
                        self.require_func("__rt_list_push"),
                        &[list, value],
                        "list_push",
                    );
                }
                list
            }
            Ast::Index { collection, index } => {
                let collection =
                    self.compile_ast(collection, vars, capture_slots, env_ptr, function);
                let index = self.compile_ast(index, vars, capture_slots, env_ptr, function);
                self.build_internal_call(
                    self.require_func("__rt_list_get"),
                    &[collection, index],
                    "list_get",
                )
            }
            Ast::IndexAssign {
                collection,
                index,
                value,
            } => {
                let collection =
                    self.compile_ast(collection, vars, capture_slots, env_ptr, function);
                let index = self.compile_ast(index, vars, capture_slots, env_ptr, function);
                let value = self.compile_ast(value, vars, capture_slots, env_ptr, function);
                self.build_internal_call(
                    self.require_func("__rt_list_set"),
                    &[collection, index, value],
                    "list_set",
                )
            }
            Ast::Expression(ExpressionAst {
                function: name,
                args,
            }) => {
                if name == "list_map" {
                    return self.compile_list_map(args, vars, capture_slots, env_ptr, function);
                }
                if name == "list_filter" {
                    return self.compile_list_filter(args, vars, capture_slots, env_ptr, function);
                }
                if name == "list_range" {
                    return self.compile_list_range(args, vars, capture_slots, env_ptr, function);
                }
                let compiled = args
                    .iter()
                    .map(|arg| self.compile_ast(arg, vars, capture_slots, env_ptr, function))
                    .collect::<Vec<_>>();
                if name.is_empty() {
                    return compiled[0];
                }
                match name.as_str() {
                    "add" => self.build_internal_call(
                        self.require_func("__op_add"),
                        &[compiled[0], compiled[1]],
                        "add",
                    ),
                    "subtract" => self.build_internal_call(
                        self.require_func("__op_subtract"),
                        &[compiled[0], compiled[1]],
                        "subtract",
                    ),
                    "multiply" => self.build_internal_call(
                        self.require_func("__op_multiply"),
                        &[compiled[0], compiled[1]],
                        "multiply",
                    ),
                    "divide" => self.build_internal_call(
                        self.require_func("__op_divide"),
                        &[compiled[0], compiled[1]],
                        "divide",
                    ),
                    "modulo" => self.build_internal_call(
                        self.require_func("__op_modulo"),
                        &[compiled[0], compiled[1]],
                        "modulo",
                    ),
                    "gt" => self.build_internal_call(
                        self.require_func("__op_gt"),
                        &[compiled[0], compiled[1]],
                        "gt",
                    ),
                    "lt" => self.build_internal_call(
                        self.require_func("__op_lt"),
                        &[compiled[0], compiled[1]],
                        "lt",
                    ),
                    "gte" => self.build_internal_call(
                        self.require_func("__op_gte"),
                        &[compiled[0], compiled[1]],
                        "gte",
                    ),
                    "lte" => self.build_internal_call(
                        self.require_func("__op_lte"),
                        &[compiled[0], compiled[1]],
                        "lte",
                    ),
                    "eq" => self.build_internal_call(
                        self.require_func("__op_eq"),
                        &[compiled[0], compiled[1]],
                        "eq",
                    ),
                    "ne" => self.build_internal_call(
                        self.require_func("__op_ne"),
                        &[compiled[0], compiled[1]],
                        "ne",
                    ),
                    "bigint_add" | "bigint_subtract" | "bigint_multiply" | "bigint_divide"
                    | "bigint_modulo" | "bigint_compare" => {
                        self.compile_bigint_builtin(name, &compiled, function)
                    }
                    "print" => self.build_internal_call(
                        self.require_func("__rt_print"),
                        &compiled,
                        "print",
                    ),
                    "list_new" => self.build_internal_call(
                        self.require_func("__rt_list_new"),
                        &compiled,
                        "list_new",
                    ),
                    "list_push" => self.build_internal_call(
                        self.require_func("__rt_list_push"),
                        &compiled,
                        "list_push",
                    ),
                    "list_insert" => self.build_internal_call(
                        self.require_func("__rt_list_insert"),
                        &compiled,
                        "list_insert",
                    ),
                    "list_len" => self.build_internal_call(
                        self.require_func("__rt_list_len"),
                        &compiled,
                        "list_len",
                    ),
                    "list_get" => self.build_internal_call(
                        self.require_func("__rt_list_get"),
                        &compiled,
                        "list_get",
                    ),
                    "list_set" => self.build_internal_call(
                        self.require_func("__rt_list_set"),
                        &compiled,
                        "list_set",
                    ),
                    "list_swap" => self.build_internal_call(
                        self.require_func("__rt_list_swap"),
                        &compiled,
                        "list_swap",
                    ),
                    "list_pop" => self.build_internal_call(
                        self.require_func("__rt_list_pop"),
                        &compiled,
                        "list_pop",
                    ),
                    "list_copy" => self.build_internal_call(
                        self.require_func("__rt_list_copy"),
                        &compiled,
                        "list_copy",
                    ),
                    other => {
                        if vars.contains_key(other) || capture_slots.contains_key(other) {
                            let callee = self.resolve_named_value(
                                other,
                                vars,
                                capture_slots,
                                env_ptr,
                                function,
                            );
                            return self.apply_function_value(callee, &compiled, function, other);
                        }
                        if self.function_ordinals.contains_key(other) {
                            return self.build_user_call(
                                self.require_func(other),
                                self.i64_type.const_zero(),
                                &compiled,
                                other,
                            );
                        }
                        let callee = self.require_func(other);
                        self.build_internal_call(callee, &compiled, other)
                    }
                }
            }
            Ast::Block(block) => {
                let mut last = None;
                for line in &block.lines {
                    last = Some(self.compile_ast(line, vars, capture_slots, env_ptr, function));
                }
                last.expect("empty block")
            }
            Ast::Variable(name) => {
                self.resolve_named_value(name, vars, capture_slots, env_ptr, function)
            }
            Ast::Assign { name, value } => {
                let value = self.compile_ast(value, vars, capture_slots, env_ptr, function);
                let ptr = vars
                    .get(name)
                    .unwrap_or_else(|| panic!("undeclared variable: {name}"));
                self.builder
                    .build_store(*ptr, self.make_pair_value(value.tag, value.payload, name))
                    .expect("failed to assign variable");
                value
            }
            Ast::If {
                condition,
                then,
                else_,
            } => {
                let cond_value =
                    self.compile_ast(condition, vars, capture_slots, env_ptr, function);
                let truth = self.build_internal_scalar_call(
                    self.require_func("__value_is_truthy"),
                    &[cond_value],
                    "truthy",
                );
                let cond = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        truth,
                        self.i64_type.const_zero(),
                        "if_cond",
                    )
                    .expect("failed to build if condition");

                let then_block = self.context.append_basic_block(function, "then");
                let else_block = self.context.append_basic_block(function, "else");
                let merge_block = self.context.append_basic_block(function, "ifend");
                self.builder
                    .build_conditional_branch(cond, then_block, else_block)
                    .expect("failed to build conditional branch");

                self.builder.position_at_end(then_block);
                let mut then_value = self.int_value(self.i64_type.const_zero());
                for line in &then.lines {
                    then_value = self.compile_ast(line, vars, capture_slots, env_ptr, function);
                }
                self.builder
                    .build_unconditional_branch(merge_block)
                    .expect("failed to branch from then");
                let then_end = self
                    .builder
                    .get_insert_block()
                    .expect("then block should exist");

                self.builder.position_at_end(else_block);
                let mut else_value = self.int_value(self.i64_type.const_zero());
                if let Some(else_block_ast) = else_ {
                    for line in &else_block_ast.lines {
                        else_value = self.compile_ast(line, vars, capture_slots, env_ptr, function);
                    }
                }
                self.builder
                    .build_unconditional_branch(merge_block)
                    .expect("failed to branch from else");
                let else_end = self
                    .builder
                    .get_insert_block()
                    .expect("else block should exist");

                self.builder.position_at_end(merge_block);
                let tag_phi = self
                    .builder
                    .build_phi(self.i64_type, "if_tag")
                    .expect("failed to build tag phi");
                tag_phi.add_incoming(&[(&then_value.tag, then_end), (&else_value.tag, else_end)]);
                let payload_phi = self
                    .builder
                    .build_phi(self.i64_type, "if_payload")
                    .expect("failed to build payload phi");
                payload_phi.add_incoming(&[
                    (&then_value.payload, then_end),
                    (&else_value.payload, else_end),
                ]);
                CompiledValue {
                    tag: tag_phi.as_basic_value().into_int_value(),
                    payload: payload_phi.as_basic_value().into_int_value(),
                }
            }
            Ast::FunctionDef(_) => panic!("nested function definitions are not supported"),
        }
    }

    fn build_boxed_call(
        &self,
        function: FunctionValue<'ctx>,
        args: &[IntValue<'ctx>],
        label: &str,
    ) -> IntValue<'ctx> {
        let args = args
            .iter()
            .copied()
            .map(BasicMetadataValueEnum::from)
            .collect::<Vec<_>>();
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

    fn build_bigint_literal(&self, digits: &str, label: &str) -> CompiledValue<'ctx> {
        let zero = self.int_value(self.i64_type.const_zero());
        let mut acc = self.build_internal_call(
            self.require_func("bigint_from_int"),
            &[zero],
            &format!("{label}_init"),
        );
        let ten = self.build_internal_call(
            self.require_func("bigint_from_int"),
            &[self.int_value(self.i64_type.const_int(10, false))],
            &format!("{label}_ten"),
        );

        for (index, ch) in digits.chars().enumerate() {
            acc = self.build_internal_call(
                self.require_func("bigint_multiply"),
                &[acc, ten],
                &format!("{label}_mul_{index}"),
            );
            let digit = self.build_internal_call(
                self.require_func("bigint_from_int"),
                &[self.int_value(
                    self.i64_type
                        .const_int(ch.to_digit(10).unwrap() as u64, false),
                )],
                &format!("{label}_digit_{index}"),
            );
            acc = self.build_internal_call(
                self.require_func("bigint_add"),
                &[acc, digit],
                &format!("{label}_add_{index}"),
            );
        }

        acc
    }

    fn build_promote_value_to_bigint(
        &self,
        value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
        label: &str,
    ) -> CompiledValue<'ctx> {
        let entry_block = self.builder.get_insert_block().unwrap();
        let bigint_block = self
            .context
            .append_basic_block(function, &format!("{label}_bigint"));
        let int_check_block = self
            .context
            .append_basic_block(function, &format!("{label}_int_check"));
        let int_block = self
            .context
            .append_basic_block(function, &format!("{label}_int"));
        let trap_block = self
            .context
            .append_basic_block(function, &format!("{label}_trap"));
        let merge_block = self
            .context
            .append_basic_block(function, &format!("{label}_merge"));

        let is_bigint = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                &format!("{label}_is_bigint"),
            )
            .expect("failed bigint promotion bigint compare");
        self.builder
            .build_conditional_branch(is_bigint, bigint_block, int_check_block)
            .expect("failed bigint promotion first branch");

        self.builder.position_at_end(bigint_block);
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint promotion bigint merge");
        let bigint_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(int_check_block);
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                &format!("{label}_is_int"),
            )
            .expect("failed bigint promotion int compare");
        self.builder
            .build_conditional_branch(is_int, int_block, trap_block)
            .expect("failed bigint promotion second branch");

        self.builder.position_at_end(int_block);
        let promoted = self.build_internal_call(
            self.require_func("bigint_from_int"),
            &[value],
            &format!("{label}_promoted"),
        );
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint promotion int merge");
        let int_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(merge_block);
        let tag_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_tag_phi"))
            .expect("failed bigint promotion tag phi");
        let payload_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_payload_phi"))
            .expect("failed bigint promotion payload phi");
        tag_phi.add_incoming(&[(&value.tag, bigint_end), (&promoted.tag, int_end)]);
        payload_phi.add_incoming(&[(&value.payload, bigint_end), (&promoted.payload, int_end)]);
        let promoted_value = CompiledValue {
            tag: tag_phi.as_basic_value().into_int_value(),
            payload: payload_phi.as_basic_value().into_int_value(),
        };
        debug_assert_eq!(entry_block.get_parent(), merge_block.get_parent());
        promoted_value
    }

    fn compile_bigint_builtin(
        &self,
        name: &str,
        args: &[CompiledValue<'ctx>],
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        assert_eq!(args.len(), 2, "{name} expects 2 arguments");
        let lhs = self.build_promote_value_to_bigint(args[0], function, &format!("{name}_lhs"));
        let rhs = self.build_promote_value_to_bigint(args[1], function, &format!("{name}_rhs"));
        self.build_internal_call(self.require_func(name), &[lhs, rhs], name)
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

    fn require_func(&self, name: &str) -> FunctionValue<'ctx> {
        *self
            .functions
            .get(name)
            .unwrap_or_else(|| panic!("missing function declaration: {name}"))
    }

    fn pair_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context
            .struct_type(&[self.i64_type.into(), self.i64_type.into()], false)
    }

    fn make_pair_value(
        &self,
        tag: IntValue<'ctx>,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> inkwell::values::StructValue<'ctx> {
        let pair = self.pair_type().get_undef();
        let pair = self
            .builder
            .build_insert_value(pair, tag, 0, &format!("{label}_tag_insert"))
            .expect("failed to insert tag")
            .into_struct_value();
        self.builder
            .build_insert_value(pair, payload, 1, &format!("{label}_payload_insert"))
            .expect("failed to insert payload")
            .into_struct_value()
    }

    fn int_value(&self, raw: IntValue<'ctx>) -> CompiledValue<'ctx> {
        CompiledValue {
            tag: self.i64_type.const_int(TAG_INT as u64, false),
            payload: raw,
        }
    }

    fn load_compiled_value(&self, ptr: PointerValue<'ctx>, label: &str) -> CompiledValue<'ctx> {
        let pair = self
            .builder
            .build_load(self.pair_type(), ptr, label)
            .expect("failed to load pair")
            .into_struct_value();
        let tag = self
            .builder
            .build_extract_value(pair, 0, &format!("{label}_tag"))
            .expect("failed to extract loaded tag")
            .into_int_value();
        let payload = self
            .builder
            .build_extract_value(pair, 1, &format!("{label}_payload"))
            .expect("failed to extract loaded payload")
            .into_int_value();
        CompiledValue { tag, payload }
    }

    fn load_value_from_env(
        &self,
        env_ptr: IntValue<'ctx>,
        slot: usize,
        label: &str,
    ) -> CompiledValue<'ctx> {
        let env_raw = self
            .builder
            .build_int_to_ptr(
                env_ptr,
                self.context.ptr_type(Default::default()),
                &format!("{label}_env_ptr"),
            )
            .expect("failed to convert env ptr");
        let index = self.i64_type.const_int(slot as u64, false);
        self.build_list_value_load_from_data_ptr(env_raw, index, label)
    }

    fn allocate_closure_for_function(
        &self,
        name: &str,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        current_env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let captures = self
            .closure_metadata
            .get(name)
            .map(|metadata| metadata.captures.as_slice())
            .unwrap_or(&[]);
        let alloc = self.require_func("__alloc");
        let env_raw = if captures.is_empty() {
            self.i64_type.const_zero()
        } else {
            let env_bytes = self
                .i64_type
                .const_int((captures.len() as i64 * VALUE_SIZE) as u64, false);
            let align = self
                .i64_type
                .const_int(std::mem::align_of::<i64>() as u64, false);
            let env_ptr = self.build_boxed_call(alloc, &[env_bytes, align], "closure_env_alloc");
            let env_data_ptr = self
                .builder
                .build_int_to_ptr(
                    env_ptr,
                    self.context.ptr_type(Default::default()),
                    "closure_env_data_ptr",
                )
                .expect("failed to convert closure env ptr");
            for (index, capture_name) in captures.iter().enumerate() {
                let value = self.resolve_named_value(
                    capture_name,
                    vars,
                    capture_slots,
                    current_env_ptr,
                    function,
                );
                self.build_list_value_store_from_data_ptr(
                    env_data_ptr,
                    self.i64_type.const_int(index as u64, false),
                    value,
                    &format!("closure_capture_{index}"),
                );
            }
            env_ptr
        };

        let closure_size = self.i64_type.const_int(CLOSURE_SIZE as u64, false);
        let closure_align = self
            .i64_type
            .const_int(std::mem::align_of::<i64>() as u64, false);
        let closure_ptr =
            self.build_boxed_call(alloc, &[closure_size, closure_align], "closure_alloc");
        let closure_raw_ptr = self
            .builder
            .build_int_to_ptr(
                closure_ptr,
                self.context.ptr_type(Default::default()),
                "closure_raw_ptr",
            )
            .expect("failed to convert closure ptr");
        let ordinal_ptr = self
            .builder
            .build_struct_gep(
                self.closure_type(),
                closure_raw_ptr,
                0,
                "closure_ordinal_ptr",
            )
            .expect("failed to build closure ordinal ptr");
        let env_ptr_ptr = self
            .builder
            .build_struct_gep(
                self.closure_type(),
                closure_raw_ptr,
                1,
                "closure_env_ptr_ptr",
            )
            .expect("failed to build closure env ptr ptr");
        let ordinal = *self
            .function_ordinals
            .get(name)
            .unwrap_or_else(|| panic!("missing function ordinal for function reference: {name}"));
        self.builder
            .build_store(ordinal_ptr, self.i64_type.const_int(ordinal as u64, true))
            .expect("failed to store closure ordinal");
        self.builder
            .build_store(env_ptr_ptr, env_raw)
            .expect("failed to store closure env ptr");
        CompiledValue {
            tag: self.i64_type.const_int(TAG_FUNCTION as u64, false),
            payload: closure_ptr,
        }
    }

    fn resolve_named_value(
        &self,
        name: &str,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        if let Some(ptr) = vars.get(name) {
            self.load_compiled_value(*ptr, name)
        } else if let Some(&slot) = capture_slots.get(name) {
            self.load_value_from_env(env_ptr, slot, name)
        } else if self.function_ordinals.contains_key(name) {
            self.allocate_closure_for_function(name, vars, capture_slots, env_ptr, function)
        } else {
            panic!("undefined variable: {name}");
        }
    }

    fn box_compiled_value(&self, value: CompiledValue<'ctx>, label: &str) -> IntValue<'ctx> {
        let box_fn = self.require_func("__box_value");
        self.build_boxed_call(box_fn, &[value.tag, value.payload], label)
    }

    fn unbox_handle(&self, handle: IntValue<'ctx>, label: &str) -> CompiledValue<'ctx> {
        let ptr = self
            .builder
            .build_int_to_ptr(
                handle,
                self.context.ptr_type(Default::default()),
                &format!("{label}_ptr"),
            )
            .expect("failed to convert handle to pointer");
        let tag = self
            .builder
            .build_int_z_extend(
                self.build_value_tag_load(ptr, label),
                self.i64_type,
                &format!("{label}_tag_i64"),
            )
            .expect("failed to extend unboxed tag");
        let payload = self.build_value_payload_load(ptr, label);
        CompiledValue { tag, payload }
    }

    #[cfg(feature = "wasi")]
    fn get_or_create_static_bytes_global(
        &self,
        name: &str,
        bytes: &[u8],
    ) -> inkwell::values::GlobalValue<'ctx> {
        if let Some(global) = self.module.get_global(name) {
            return global;
        }

        let byte_type = self.context.i8_type();
        let array_type = byte_type.array_type(bytes.len() as u32);
        let global = self.module.add_global(array_type, None, name);
        global.set_linkage(Linkage::Internal);
        global.set_constant(true);
        let values = bytes
            .iter()
            .map(|byte| byte_type.const_int(*byte as u64, false))
            .collect::<Vec<_>>();
        global.set_initializer(&byte_type.const_array(&values));
        global
    }

    #[cfg(feature = "wasi")]
    fn build_static_bytes_ptr(&self, name: &str, bytes: &[u8], label: &str) -> PointerValue<'ctx> {
        let global = self.get_or_create_static_bytes_global(name, bytes);
        let array_type = self.context.i8_type().array_type(bytes.len() as u32);
        let zero = self.context.i32_type().const_zero();
        unsafe {
            self.builder
                .build_gep(
                    array_type,
                    global.as_pointer_value(),
                    &[zero, zero],
                    &format!("{label}_ptr"),
                )
                .expect("failed to build static bytes ptr")
        }
    }

    #[cfg(feature = "wasi")]
    fn build_wasi_write_const(&self, global_name: &str, bytes: &[u8], label: &str) {
        let write_bytes = self.require_func("__wasi_write_bytes");
        let ptr = self.build_static_bytes_ptr(global_name, bytes, label);
        self.builder
            .build_call(
                write_bytes,
                &[
                    ptr.into(),
                    self.context
                        .i32_type()
                        .const_int(bytes.len() as u64, false)
                        .into(),
                ],
                &format!("{label}_write"),
            )
            .expect("failed to write static bytes");
    }

    fn expect_tag_payload(
        &self,
        value: CompiledValue<'ctx>,
        expected_tag: i64,
        label: &str,
        ok_block: inkwell::basic_block::BasicBlock<'ctx>,
        trap_block: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> IntValue<'ctx> {
        let is_expected = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(expected_tag as u64, false),
                &format!("{label}_tag_ok"),
            )
            .expect("failed to compare expected tag");
        self.builder
            .build_conditional_branch(is_expected, ok_block, trap_block)
            .expect("failed to branch on expected tag");
        value.payload
    }

    fn expect_tag_int(
        &self,
        value: CompiledValue<'ctx>,
        label: &str,
        trap_block: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> IntValue<'ctx> {
        let idx_ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                &format!("{label}_is_int"),
            )
            .expect("failed to compare int index tag");
        let idx_block = self.context.append_basic_block(
            self.builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap(),
            &format!("{label}_ok"),
        );
        self.builder
            .build_conditional_branch(idx_ok, idx_block, trap_block)
            .expect("failed to branch on int index tag");
        self.builder.position_at_end(idx_block);
        let non_neg = self
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                value.payload,
                self.i64_type.const_zero(),
                &format!("{label}_non_neg"),
            )
            .expect("failed to compare non-negative index");
        let non_neg_block = self.context.append_basic_block(
            self.builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap(),
            &format!("{label}_non_neg_ok"),
        );
        self.builder
            .build_conditional_branch(non_neg, non_neg_block, trap_block)
            .expect("failed to branch on non-negative index");
        self.builder.position_at_end(non_neg_block);
        value.payload
    }

    fn build_list_header_ptr(&self, payload: IntValue<'ctx>, label: &str) -> PointerValue<'ctx> {
        self.builder
            .build_int_to_ptr(
                payload,
                self.context.ptr_type(Default::default()),
                &format!("{label}_header_ptr"),
            )
            .expect("failed to convert list payload to pointer")
    }

    fn build_bigint_header_ptr(&self, payload: IntValue<'ctx>, label: &str) -> PointerValue<'ctx> {
        self.builder
            .build_int_to_ptr(
                payload,
                self.context.ptr_type(Default::default()),
                &format!("{label}_bigint_header_ptr"),
            )
            .expect("failed to convert bigint payload to pointer")
    }

    fn build_bigint_sign_load(&self, payload: IntValue<'ctx>, label: &str) -> IntValue<'ctx> {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let sign_ptr = self
            .builder
            .build_struct_gep(
                self.bigint_header_type(),
                ptr,
                0,
                &format!("{label}_sign_ptr"),
            )
            .expect("failed to build bigint sign gep");
        self.builder
            .build_load(self.i64_type, sign_ptr, &format!("{label}_sign"))
            .expect("failed to load bigint sign")
            .into_int_value()
    }

    fn build_bigint_sign_store(&self, payload: IntValue<'ctx>, sign: IntValue<'ctx>, label: &str) {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let sign_ptr = self
            .builder
            .build_struct_gep(
                self.bigint_header_type(),
                ptr,
                0,
                &format!("{label}_sign_ptr"),
            )
            .expect("failed to build bigint sign gep");
        self.builder
            .build_store(sign_ptr, sign)
            .expect("failed to store bigint sign");
    }

    fn build_bigint_len_load(&self, payload: IntValue<'ctx>, label: &str) -> IntValue<'ctx> {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let len_ptr = self
            .builder
            .build_struct_gep(
                self.bigint_header_type(),
                ptr,
                1,
                &format!("{label}_len_ptr"),
            )
            .expect("failed to build bigint len gep");
        self.builder
            .build_load(self.i64_type, len_ptr, &format!("{label}_len"))
            .expect("failed to load bigint len")
            .into_int_value()
    }

    fn build_bigint_len_store(&self, payload: IntValue<'ctx>, len: IntValue<'ctx>, label: &str) {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let len_ptr = self
            .builder
            .build_struct_gep(
                self.bigint_header_type(),
                ptr,
                1,
                &format!("{label}_len_ptr"),
            )
            .expect("failed to build bigint len gep");
        self.builder
            .build_store(len_ptr, len)
            .expect("failed to store bigint len");
    }

    fn build_bigint_cap_store(&self, payload: IntValue<'ctx>, cap: IntValue<'ctx>, label: &str) {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let cap_ptr = self
            .builder
            .build_struct_gep(
                self.bigint_header_type(),
                ptr,
                2,
                &format!("{label}_cap_ptr"),
            )
            .expect("failed to build bigint cap gep");
        self.builder
            .build_store(cap_ptr, cap)
            .expect("failed to store bigint cap");
    }

    fn build_bigint_ptr_load(&self, payload: IntValue<'ctx>, label: &str) -> PointerValue<'ctx> {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(
                self.bigint_header_type(),
                ptr,
                3,
                &format!("{label}_ptr_ptr"),
            )
            .expect("failed to build bigint data ptr gep");
        self.builder
            .build_load(
                self.context.ptr_type(Default::default()),
                data_ptr_ptr,
                &format!("{label}_ptr"),
            )
            .expect("failed to load bigint data ptr")
            .into_pointer_value()
    }

    fn build_bigint_ptr_store(
        &self,
        payload: IntValue<'ctx>,
        ptr_value: PointerValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(
                self.bigint_header_type(),
                ptr,
                3,
                &format!("{label}_ptr_ptr"),
            )
            .expect("failed to build bigint data ptr gep");
        self.builder
            .build_store(data_ptr_ptr, ptr_value)
            .expect("failed to store bigint data ptr");
    }

    fn build_bigint_limb_ptr(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        let data_ptr = self.build_bigint_ptr_load(payload, label);
        let byte_off = self
            .builder
            .build_left_shift(
                index,
                self.i64_type.const_int(2, false),
                &format!("{label}_byte_off"),
            )
            .expect("failed to shift bigint limb offset");
        let base = self
            .builder
            .build_ptr_to_int(data_ptr, self.i64_type, &format!("{label}_base_i64"))
            .expect("failed to ptr-to-int bigint data ptr");
        let addr = self
            .builder
            .build_int_add(base, byte_off, &format!("{label}_addr"))
            .expect("failed to compute bigint limb addr");
        self.builder
            .build_int_to_ptr(
                addr,
                self.context.ptr_type(Default::default()),
                &format!("{label}_limb_ptr"),
            )
            .expect("failed to int-to-ptr bigint limb ptr")
    }

    fn build_bigint_limb_load(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let ptr = self.build_bigint_limb_ptr(payload, index, label);
        let limb32 = self
            .builder
            .build_load(self.context.i32_type(), ptr, &format!("{label}_limb32"))
            .expect("failed to load bigint limb")
            .into_int_value();
        self.builder
            .build_int_z_extend(limb32, self.i64_type, &format!("{label}_limb64"))
            .expect("failed to zext bigint limb")
    }

    fn build_bigint_limb_store(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        limb: IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_bigint_limb_ptr(payload, index, label);
        let limb32 = self
            .builder
            .build_int_truncate(limb, self.context.i32_type(), &format!("{label}_limb32"))
            .expect("failed to truncate bigint limb");
        self.builder
            .build_store(ptr, limb32)
            .expect("failed to store bigint limb");
    }

    fn build_bigint_alloc(&self, cap: IntValue<'ctx>, label: &str) -> IntValue<'ctx> {
        let alloc = self.require_func("__alloc");
        let limb_bytes = self
            .builder
            .build_int_mul(
                cap,
                self.i64_type.const_int(BIGINT_LIMB_SIZE as u64, false),
                &format!("{label}_limb_bytes"),
            )
            .expect("failed to build bigint limb bytes");
        let limb_ptr_raw = self.build_boxed_call(
            alloc,
            &[limb_bytes, self.i64_type.const_int(4, false)],
            &format!("{label}_limb_alloc"),
        );
        let limb_ptr = self
            .builder
            .build_int_to_ptr(
                limb_ptr_raw,
                self.context.ptr_type(Default::default()),
                &format!("{label}_limb_ptr"),
            )
            .expect("failed to convert bigint limb ptr");
        let header_ptr_raw = self.build_boxed_call(
            alloc,
            &[
                self.i64_type.const_int(BIGINT_HEADER_SIZE as u64, false),
                self.i64_type.const_int(8, false),
            ],
            &format!("{label}_header_alloc"),
        );
        self.build_bigint_sign_store(header_ptr_raw, self.i64_type.const_zero(), label);
        self.build_bigint_len_store(header_ptr_raw, self.i64_type.const_zero(), label);
        self.build_bigint_cap_store(header_ptr_raw, cap, label);
        self.build_bigint_ptr_store(header_ptr_raw, limb_ptr, label);
        header_ptr_raw
    }

    fn build_bigint_zero(&self, label: &str) -> IntValue<'ctx> {
        let zero = self.i64_type.const_zero();
        let ptr = self.build_bigint_alloc(zero, label);
        self.build_bigint_sign_store(ptr, zero, label);
        self.build_bigint_len_store(ptr, zero, label);
        ptr
    }

    fn build_bigint_one(&self, label: &str) -> IntValue<'ctx> {
        let one = self.i64_type.const_int(1, false);
        let ptr = self.build_bigint_alloc(one, label);
        self.build_bigint_sign_store(ptr, one, label);
        self.build_bigint_len_store(ptr, one, label);
        self.build_bigint_limb_store(ptr, self.i64_type.const_zero(), one, label);
        ptr
    }

    fn build_bigint_normalize(&self, payload: IntValue<'ctx>, label: &str) {
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .expect("missing function for bigint normalize");
        let loop_block = self
            .context
            .append_basic_block(function, &format!("{label}_norm_loop"));
        let body_block = self
            .context
            .append_basic_block(function, &format!("{label}_norm_body"));
        let done_block = self
            .context
            .append_basic_block(function, &format!("{label}_norm_done"));
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to branch to bigint normalize loop");

        self.builder.position_at_end(loop_block);
        let len = self.build_bigint_len_load(payload, label);
        let has_len = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                len,
                self.i64_type.const_zero(),
                &format!("{label}_norm_has_len"),
            )
            .expect("failed to compare bigint normalize len");
        self.builder
            .build_conditional_branch(has_len, body_block, done_block)
            .expect("failed to branch bigint normalize len");

        self.builder.position_at_end(body_block);
        let last_index = self
            .builder
            .build_int_sub(
                len,
                self.i64_type.const_int(1, false),
                &format!("{label}_last_idx"),
            )
            .expect("failed to build bigint normalize last idx");
        let last = self.build_bigint_limb_load(payload, last_index, &format!("{label}_last"));
        let is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                last,
                self.i64_type.const_zero(),
                &format!("{label}_norm_is_zero"),
            )
            .expect("failed to compare bigint normalize last limb");
        let trim_block = self
            .context
            .append_basic_block(function, &format!("{label}_norm_trim"));
        let keep_block = self
            .context
            .append_basic_block(function, &format!("{label}_norm_keep"));
        self.builder
            .build_conditional_branch(is_zero, trim_block, keep_block)
            .expect("failed to branch bigint normalize zero");

        self.builder.position_at_end(trim_block);
        self.build_bigint_len_store(payload, last_index, &format!("{label}_trim"));
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to loop bigint normalize");

        self.builder.position_at_end(keep_block);
        self.builder
            .build_unconditional_branch(done_block)
            .expect("failed to branch bigint normalize done");

        self.builder.position_at_end(done_block);
        let final_len = self.build_bigint_len_load(payload, &format!("{label}_final"));
        let is_zero_len = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                final_len,
                self.i64_type.const_zero(),
                &format!("{label}_final_zero"),
            )
            .expect("failed to compare bigint final len");
        let set_zero_block = self
            .context
            .append_basic_block(function, &format!("{label}_set_zero_sign"));
        let end_block = self
            .context
            .append_basic_block(function, &format!("{label}_norm_end"));
        self.builder
            .build_conditional_branch(is_zero_len, set_zero_block, end_block)
            .expect("failed to branch bigint final zero");

        self.builder.position_at_end(set_zero_block);
        self.build_bigint_sign_store(
            payload,
            self.i64_type.const_zero(),
            &format!("{label}_zero"),
        );
        self.builder
            .build_unconditional_branch(end_block)
            .expect("failed to branch bigint normalize end");

        self.builder.position_at_end(end_block);
    }

    fn build_bigint_cmp_abs(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .expect("missing function for bigint cmp abs");
        let merge_block = self
            .context
            .append_basic_block(function, &format!("{label}_merge"));
        let lhs_len = self.build_bigint_len_load(lhs, &format!("{label}_lhs"));
        let rhs_len = self.build_bigint_len_load(rhs, &format!("{label}_rhs"));
        let len_eq = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_len,
                rhs_len,
                &format!("{label}_len_eq"),
            )
            .expect("failed bigint len eq compare");
        let len_cmp_block = self
            .context
            .append_basic_block(function, &format!("{label}_len_cmp"));
        let same_len_block = self
            .context
            .append_basic_block(function, &format!("{label}_same_len"));
        self.builder
            .build_conditional_branch(len_eq, same_len_block, len_cmp_block)
            .expect("failed bigint len branch");

        self.builder.position_at_end(len_cmp_block);
        let lhs_gt = self
            .builder
            .build_int_compare(
                IntPredicate::UGT,
                lhs_len,
                rhs_len,
                &format!("{label}_lhs_len_gt"),
            )
            .expect("failed bigint len gt compare");
        let len_cmp = self
            .builder
            .build_select(
                lhs_gt,
                self.i64_type.const_int(1, true),
                self.i64_type.const_int((-1i64) as u64, true),
                &format!("{label}_len_cmp_value"),
            )
            .expect("failed bigint len cmp select")
            .into_int_value();
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint len cmp jump");
        let len_cmp_block_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(same_len_block);
        let loop_block = self
            .context
            .append_basic_block(function, &format!("{label}_loop"));
        let body_block = self
            .context
            .append_basic_block(function, &format!("{label}_body"));
        let equal_block = self
            .context
            .append_basic_block(function, &format!("{label}_equal"));
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed bigint cmp loop jump");
        let same_len_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let remaining_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_remaining"))
            .expect("failed bigint cmp phi");
        remaining_phi.add_incoming(&[(&lhs_len, same_len_end)]);
        let remaining = remaining_phi.as_basic_value().into_int_value();
        let has_more = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                remaining,
                self.i64_type.const_zero(),
                &format!("{label}_has_more"),
            )
            .expect("failed bigint cmp remaining compare");
        self.builder
            .build_conditional_branch(has_more, body_block, equal_block)
            .expect("failed bigint cmp loop branch");

        self.builder.position_at_end(body_block);
        let index = self
            .builder
            .build_int_sub(
                remaining,
                self.i64_type.const_int(1, false),
                &format!("{label}_index"),
            )
            .expect("failed bigint cmp index");
        let lhs_limb = self.build_bigint_limb_load(lhs, index, &format!("{label}_lhs_limb"));
        let rhs_limb = self.build_bigint_limb_load(rhs, index, &format!("{label}_rhs_limb"));
        let limb_eq = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_limb,
                rhs_limb,
                &format!("{label}_limb_eq"),
            )
            .expect("failed bigint cmp limb eq");
        let next_block = self
            .context
            .append_basic_block(function, &format!("{label}_next"));
        let diff_block = self
            .context
            .append_basic_block(function, &format!("{label}_diff"));
        self.builder
            .build_conditional_branch(limb_eq, next_block, diff_block)
            .expect("failed bigint cmp limb branch");

        self.builder.position_at_end(next_block);
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed bigint cmp continue");
        let next_end = self.builder.get_insert_block().unwrap();
        remaining_phi.add_incoming(&[(&index, next_end)]);

        self.builder.position_at_end(diff_block);
        let lhs_gt = self
            .builder
            .build_int_compare(
                IntPredicate::UGT,
                lhs_limb,
                rhs_limb,
                &format!("{label}_limb_gt"),
            )
            .expect("failed bigint cmp limb gt");
        let limb_cmp = self
            .builder
            .build_select(
                lhs_gt,
                self.i64_type.const_int(1, true),
                self.i64_type.const_int((-1i64) as u64, true),
                &format!("{label}_limb_cmp"),
            )
            .expect("failed bigint cmp limb select")
            .into_int_value();
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint cmp diff jump");
        let diff_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(equal_block);
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint cmp equal jump");
        let equal_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_block);
        let phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_cmp"))
            .expect("failed bigint cmp result phi");
        phi.add_incoming(&[
            (&len_cmp, len_cmp_block_end),
            (&limb_cmp, diff_end),
            (&self.i64_type.const_zero(), equal_end),
        ]);
        phi.as_basic_value().into_int_value()
    }

    fn build_bigint_add_abs(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let lhs_len = self.build_bigint_len_load(lhs, &format!("{label}_lhs"));
        let rhs_len = self.build_bigint_len_load(rhs, &format!("{label}_rhs"));
        let lhs_ge = self
            .builder
            .build_int_compare(
                IntPredicate::UGE,
                lhs_len,
                rhs_len,
                &format!("{label}_lhs_ge"),
            )
            .expect("failed bigint add abs len compare");
        let max_len = self
            .builder
            .build_select(lhs_ge, lhs_len, rhs_len, &format!("{label}_max_len"))
            .expect("failed bigint add abs max len")
            .into_int_value();
        let cap = self
            .builder
            .build_int_add(
                max_len,
                self.i64_type.const_int(1, false),
                &format!("{label}_cap"),
            )
            .expect("failed bigint add abs cap");
        let result = self.build_bigint_alloc(cap, &format!("{label}_alloc"));
        self.build_bigint_len_store(result, cap, label);

        let loop_block = self
            .context
            .append_basic_block(function, &format!("{label}_loop"));
        let body_block = self
            .context
            .append_basic_block(function, &format!("{label}_body"));
        let done_block = self
            .context
            .append_basic_block(function, &format!("{label}_done"));
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed bigint add abs loop jump");
        let entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_idx"))
            .expect("failed bigint add abs idx phi");
        let carry_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_carry"))
            .expect("failed bigint add abs carry phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_end)]);
        carry_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_end)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let carry = carry_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, max_len, &format!("{label}_more"))
            .expect("failed bigint add abs loop compare");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed bigint add abs loop branch");

        self.builder.position_at_end(body_block);
        let lhs_in = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, lhs_len, &format!("{label}_lhs_in"))
            .expect("failed bigint add abs lhs in");
        let lhs_read = self
            .context
            .append_basic_block(function, &format!("{label}_lhs_read"));
        let lhs_zero = self
            .context
            .append_basic_block(function, &format!("{label}_lhs_zero"));
        let lhs_merge = self
            .context
            .append_basic_block(function, &format!("{label}_lhs_merge"));
        self.builder
            .build_conditional_branch(lhs_in, lhs_read, lhs_zero)
            .expect("failed bigint add abs lhs branch");

        self.builder.position_at_end(lhs_read);
        let lhs_limb_val = self.build_bigint_limb_load(lhs, idx, &format!("{label}_lhs_limb"));
        self.builder
            .build_unconditional_branch(lhs_merge)
            .expect("failed lhs merge jump");
        let lhs_read_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(lhs_zero);
        self.builder
            .build_unconditional_branch(lhs_merge)
            .expect("failed lhs zero merge jump");
        let lhs_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(lhs_merge);
        let lhs_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_lhs_phi"))
            .expect("failed lhs phi");
        lhs_phi.add_incoming(&[
            (&lhs_limb_val, lhs_read_end),
            (&self.i64_type.const_zero(), lhs_zero_end),
        ]);
        let lhs_limb = lhs_phi.as_basic_value().into_int_value();

        let rhs_in = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, rhs_len, &format!("{label}_rhs_in"))
            .expect("failed bigint add abs rhs in");
        let rhs_read = self
            .context
            .append_basic_block(function, &format!("{label}_rhs_read"));
        let rhs_zero = self
            .context
            .append_basic_block(function, &format!("{label}_rhs_zero"));
        let rhs_merge = self
            .context
            .append_basic_block(function, &format!("{label}_rhs_merge"));
        self.builder
            .build_conditional_branch(rhs_in, rhs_read, rhs_zero)
            .expect("failed bigint add abs rhs branch");

        self.builder.position_at_end(rhs_read);
        let rhs_limb_val = self.build_bigint_limb_load(rhs, idx, &format!("{label}_rhs_limb"));
        self.builder
            .build_unconditional_branch(rhs_merge)
            .expect("failed rhs merge jump");
        let rhs_read_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_zero);
        self.builder
            .build_unconditional_branch(rhs_merge)
            .expect("failed rhs zero merge jump");
        let rhs_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_merge);
        let rhs_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_rhs_phi"))
            .expect("failed rhs phi");
        rhs_phi.add_incoming(&[
            (&rhs_limb_val, rhs_read_end),
            (&self.i64_type.const_zero(), rhs_zero_end),
        ]);
        let rhs_limb = rhs_phi.as_basic_value().into_int_value();

        let tmp = self
            .builder
            .build_int_add(lhs_limb, rhs_limb, &format!("{label}_tmp"))
            .expect("failed bigint add abs tmp");
        let sum = self
            .builder
            .build_int_add(tmp, carry, &format!("{label}_sum"))
            .expect("failed bigint add abs sum");
        let low = self
            .builder
            .build_and(
                sum,
                self.i64_type.const_int(0xffff_ffff, false),
                &format!("{label}_low"),
            )
            .expect("failed bigint add abs low");
        self.build_bigint_limb_store(result, idx, low, &format!("{label}_store"));
        let next_carry = self
            .builder
            .build_right_shift(
                sum,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_next_carry"),
            )
            .expect("failed bigint add abs next carry");
        let next_idx = self
            .builder
            .build_int_add(
                idx,
                self.i64_type.const_int(1, false),
                &format!("{label}_next_idx"),
            )
            .expect("failed bigint add abs next idx");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed bigint add abs continue");
        let body_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next_idx, body_end)]);
        carry_phi.add_incoming(&[(&next_carry, body_end)]);

        self.builder.position_at_end(done_block);
        let final_carry = carry_phi.as_basic_value().into_int_value();
        self.build_bigint_limb_store(result, max_len, final_carry, &format!("{label}_final"));
        self.build_bigint_normalize(result, label);
        result
    }

    fn build_bigint_sub_abs(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let lhs_len = self.build_bigint_len_load(lhs, &format!("{label}_lhs"));
        let rhs_len = self.build_bigint_len_load(rhs, &format!("{label}_rhs"));
        let result = self.build_bigint_alloc(lhs_len, &format!("{label}_alloc"));
        self.build_bigint_len_store(result, lhs_len, label);

        let loop_block = self
            .context
            .append_basic_block(function, &format!("{label}_loop"));
        let body_block = self
            .context
            .append_basic_block(function, &format!("{label}_body"));
        let done_block = self
            .context
            .append_basic_block(function, &format!("{label}_done"));
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed bigint sub abs loop jump");
        let entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_idx"))
            .expect("failed bigint sub abs idx phi");
        let borrow_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_borrow"))
            .expect("failed bigint sub abs borrow phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_end)]);
        borrow_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_end)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let borrow = borrow_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, lhs_len, &format!("{label}_more"))
            .expect("failed bigint sub abs loop compare");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed bigint sub abs loop branch");

        self.builder.position_at_end(body_block);
        let lhs_limb = self.build_bigint_limb_load(lhs, idx, &format!("{label}_lhs_limb"));
        let rhs_in = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, rhs_len, &format!("{label}_rhs_in"))
            .expect("failed bigint sub abs rhs in");
        let rhs_read = self
            .context
            .append_basic_block(function, &format!("{label}_rhs_read"));
        let rhs_zero = self
            .context
            .append_basic_block(function, &format!("{label}_rhs_zero"));
        let rhs_merge = self
            .context
            .append_basic_block(function, &format!("{label}_rhs_merge"));
        self.builder
            .build_conditional_branch(rhs_in, rhs_read, rhs_zero)
            .expect("failed bigint sub abs rhs branch");

        self.builder.position_at_end(rhs_read);
        let rhs_limb_val = self.build_bigint_limb_load(rhs, idx, &format!("{label}_rhs_limb"));
        self.builder
            .build_unconditional_branch(rhs_merge)
            .expect("failed bigint sub abs rhs merge jump");
        let rhs_read_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_zero);
        self.builder
            .build_unconditional_branch(rhs_merge)
            .expect("failed bigint sub abs rhs zero merge jump");
        let rhs_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_merge);
        let rhs_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_rhs_phi"))
            .expect("failed bigint sub abs rhs phi");
        rhs_phi.add_incoming(&[
            (&rhs_limb_val, rhs_read_end),
            (&self.i64_type.const_zero(), rhs_zero_end),
        ]);
        let rhs_limb = rhs_phi.as_basic_value().into_int_value();

        let rhs_plus_borrow = self
            .builder
            .build_int_add(rhs_limb, borrow, &format!("{label}_rhs_plus_borrow"))
            .expect("failed bigint sub abs rhs_plus_borrow");
        let enough = self
            .builder
            .build_int_compare(
                IntPredicate::UGE,
                lhs_limb,
                rhs_plus_borrow,
                &format!("{label}_enough"),
            )
            .expect("failed bigint sub abs enough compare");
        let no_borrow_block = self
            .context
            .append_basic_block(function, &format!("{label}_no_borrow"));
        let borrow_block = self
            .context
            .append_basic_block(function, &format!("{label}_borrow_block"));
        let merge = self
            .context
            .append_basic_block(function, &format!("{label}_merge"));
        self.builder
            .build_conditional_branch(enough, no_borrow_block, borrow_block)
            .expect("failed bigint sub abs enough branch");

        self.builder.position_at_end(no_borrow_block);
        let diff_no_borrow = self
            .builder
            .build_int_sub(
                lhs_limb,
                rhs_plus_borrow,
                &format!("{label}_diff_no_borrow"),
            )
            .expect("failed bigint sub abs diff no borrow");
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed bigint sub abs no borrow jump");
        let no_borrow_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(borrow_block);
        let lhs_with_base = self
            .builder
            .build_int_add(
                lhs_limb,
                self.i64_type.const_int(1u64 << 32, false),
                &format!("{label}_lhs_with_base"),
            )
            .expect("failed bigint sub abs lhs_with_base");
        let diff_borrow = self
            .builder
            .build_int_sub(
                lhs_with_base,
                rhs_plus_borrow,
                &format!("{label}_diff_borrow"),
            )
            .expect("failed bigint sub abs diff borrow");
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed bigint sub abs borrow jump");
        let borrow_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge);
        let diff_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_diff_phi"))
            .expect("failed bigint sub abs diff phi");
        let next_borrow_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_next_borrow_phi"))
            .expect("failed bigint sub abs next borrow phi");
        diff_phi.add_incoming(&[(&diff_no_borrow, no_borrow_end), (&diff_borrow, borrow_end)]);
        next_borrow_phi.add_incoming(&[
            (&self.i64_type.const_zero(), no_borrow_end),
            (&self.i64_type.const_int(1, false), borrow_end),
        ]);
        let out_limb = diff_phi.as_basic_value().into_int_value();
        let next_borrow = next_borrow_phi.as_basic_value().into_int_value();
        self.build_bigint_limb_store(result, idx, out_limb, &format!("{label}_store"));
        let next_idx = self
            .builder
            .build_int_add(
                idx,
                self.i64_type.const_int(1, false),
                &format!("{label}_next_idx"),
            )
            .expect("failed bigint sub abs next idx");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed bigint sub abs continue");
        let body_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next_idx, body_end)]);
        borrow_phi.add_incoming(&[(&next_borrow, body_end)]);

        self.builder.position_at_end(done_block);
        self.build_bigint_normalize(result, label);
        result
    }

    fn build_bigint_mul_abs(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let lhs_len = self.build_bigint_len_load(lhs, &format!("{label}_lhs"));
        let rhs_len = self.build_bigint_len_load(rhs, &format!("{label}_rhs"));
        let cap = self
            .builder
            .build_int_add(lhs_len, rhs_len, &format!("{label}_cap"))
            .expect("failed bigint mul abs cap");
        let result = self.build_bigint_alloc(cap, &format!("{label}_alloc"));
        self.build_bigint_len_store(result, cap, label);

        let init_loop = self
            .context
            .append_basic_block(function, &format!("{label}_init_loop"));
        let init_body = self
            .context
            .append_basic_block(function, &format!("{label}_init_body"));
        let init_done = self
            .context
            .append_basic_block(function, &format!("{label}_init_done"));
        self.builder
            .build_unconditional_branch(init_loop)
            .expect("failed bigint mul init jump");
        let init_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(init_loop);
        let init_idx_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_init_idx"))
            .expect("failed bigint mul init idx phi");
        init_idx_phi.add_incoming(&[(&self.i64_type.const_zero(), init_entry_end)]);
        let init_idx = init_idx_phi.as_basic_value().into_int_value();
        let init_more = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                init_idx,
                cap,
                &format!("{label}_init_more"),
            )
            .expect("failed bigint mul init compare");
        self.builder
            .build_conditional_branch(init_more, init_body, init_done)
            .expect("failed bigint mul init branch");

        self.builder.position_at_end(init_body);
        self.build_bigint_limb_store(
            result,
            init_idx,
            self.i64_type.const_zero(),
            &format!("{label}_init_store"),
        );
        let init_next = self
            .builder
            .build_int_add(
                init_idx,
                self.i64_type.const_int(1, false),
                &format!("{label}_init_next"),
            )
            .expect("failed bigint mul init next");
        self.builder
            .build_unconditional_branch(init_loop)
            .expect("failed bigint mul init loop");
        let init_body_end = self.builder.get_insert_block().unwrap();
        init_idx_phi.add_incoming(&[(&init_next, init_body_end)]);

        self.builder.position_at_end(init_done);
        let outer_loop = self
            .context
            .append_basic_block(function, &format!("{label}_outer_loop"));
        let outer_body = self
            .context
            .append_basic_block(function, &format!("{label}_outer_body"));
        let outer_done = self
            .context
            .append_basic_block(function, &format!("{label}_outer_done"));
        self.builder
            .build_unconditional_branch(outer_loop)
            .expect("failed bigint mul outer jump");
        let outer_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(outer_loop);
        let i_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_outer_i"))
            .expect("failed bigint mul outer i phi");
        i_phi.add_incoming(&[(&self.i64_type.const_zero(), outer_entry_end)]);
        let i = i_phi.as_basic_value().into_int_value();
        let outer_more = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                i,
                lhs_len,
                &format!("{label}_outer_more"),
            )
            .expect("failed bigint mul outer compare");
        self.builder
            .build_conditional_branch(outer_more, outer_body, outer_done)
            .expect("failed bigint mul outer branch");

        self.builder.position_at_end(outer_body);
        let lhs_limb = self.build_bigint_limb_load(lhs, i, &format!("{label}_lhs_limb"));
        let inner_loop = self
            .context
            .append_basic_block(function, &format!("{label}_inner_loop"));
        let inner_body = self
            .context
            .append_basic_block(function, &format!("{label}_inner_body"));
        let inner_done = self
            .context
            .append_basic_block(function, &format!("{label}_inner_done"));
        self.builder
            .build_unconditional_branch(inner_loop)
            .expect("failed bigint mul inner jump");
        let inner_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(inner_loop);
        let j_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_inner_j"))
            .expect("failed bigint mul inner j phi");
        let carry_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_inner_carry"))
            .expect("failed bigint mul inner carry phi");
        j_phi.add_incoming(&[(&self.i64_type.const_zero(), inner_entry_end)]);
        carry_phi.add_incoming(&[(&self.i64_type.const_zero(), inner_entry_end)]);
        let j = j_phi.as_basic_value().into_int_value();
        let carry = carry_phi.as_basic_value().into_int_value();
        let inner_more = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                j,
                rhs_len,
                &format!("{label}_inner_more"),
            )
            .expect("failed bigint mul inner compare");
        self.builder
            .build_conditional_branch(inner_more, inner_body, inner_done)
            .expect("failed bigint mul inner branch");

        self.builder.position_at_end(inner_body);
        let rhs_limb = self.build_bigint_limb_load(rhs, j, &format!("{label}_rhs_limb"));
        let idx = self
            .builder
            .build_int_add(i, j, &format!("{label}_idx"))
            .expect("failed bigint mul idx");
        let existing = self.build_bigint_limb_load(result, idx, &format!("{label}_existing"));
        let prod = self
            .builder
            .build_int_mul(lhs_limb, rhs_limb, &format!("{label}_prod"))
            .expect("failed bigint mul prod");
        let tmp = self
            .builder
            .build_int_add(existing, prod, &format!("{label}_tmp"))
            .expect("failed bigint mul tmp");
        let total = self
            .builder
            .build_int_add(tmp, carry, &format!("{label}_total"))
            .expect("failed bigint mul total");
        let low = self
            .builder
            .build_and(
                total,
                self.i64_type.const_int(0xffff_ffff, false),
                &format!("{label}_low"),
            )
            .expect("failed bigint mul low");
        self.build_bigint_limb_store(result, idx, low, &format!("{label}_store"));
        let next_carry = self
            .builder
            .build_right_shift(
                total,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_next_carry"),
            )
            .expect("failed bigint mul carry shift");
        let next_j = self
            .builder
            .build_int_add(
                j,
                self.i64_type.const_int(1, false),
                &format!("{label}_next_j"),
            )
            .expect("failed bigint mul next j");
        self.builder
            .build_unconditional_branch(inner_loop)
            .expect("failed bigint mul inner loop");
        let inner_body_end = self.builder.get_insert_block().unwrap();
        j_phi.add_incoming(&[(&next_j, inner_body_end)]);
        carry_phi.add_incoming(&[(&next_carry, inner_body_end)]);

        self.builder.position_at_end(inner_done);
        let carry_loop = self
            .context
            .append_basic_block(function, &format!("{label}_carry_loop"));
        let carry_body = self
            .context
            .append_basic_block(function, &format!("{label}_carry_body"));
        let carry_done = self
            .context
            .append_basic_block(function, &format!("{label}_carry_done"));
        let carry_start_idx = self
            .builder
            .build_int_add(i, rhs_len, &format!("{label}_carry_start_idx"))
            .expect("failed bigint mul carry start idx");
        self.builder
            .build_unconditional_branch(carry_loop)
            .expect("failed bigint mul carry jump");
        let carry_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(carry_loop);
        let carry_idx_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_carry_idx"))
            .expect("failed bigint mul carry idx phi");
        let carry_val_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_carry_val"))
            .expect("failed bigint mul carry val phi");
        carry_idx_phi.add_incoming(&[(&carry_start_idx, carry_entry_end)]);
        carry_val_phi.add_incoming(&[(
            &carry_phi.as_basic_value().into_int_value(),
            carry_entry_end,
        )]);
        let carry_idx = carry_idx_phi.as_basic_value().into_int_value();
        let carry_val = carry_val_phi.as_basic_value().into_int_value();
        let carry_more = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                carry_val,
                self.i64_type.const_zero(),
                &format!("{label}_carry_more"),
            )
            .expect("failed bigint mul carry more");
        self.builder
            .build_conditional_branch(carry_more, carry_body, carry_done)
            .expect("failed bigint mul carry branch");

        self.builder.position_at_end(carry_body);
        let existing =
            self.build_bigint_limb_load(result, carry_idx, &format!("{label}_carry_existing"));
        let total = self
            .builder
            .build_int_add(existing, carry_val, &format!("{label}_carry_total"))
            .expect("failed bigint mul carry total");
        let low = self
            .builder
            .build_and(
                total,
                self.i64_type.const_int(0xffff_ffff, false),
                &format!("{label}_carry_low"),
            )
            .expect("failed bigint mul carry low");
        self.build_bigint_limb_store(result, carry_idx, low, &format!("{label}_carry_store"));
        let next_carry = self
            .builder
            .build_right_shift(
                total,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_carry_next_carry"),
            )
            .expect("failed bigint mul carry shift");
        let next_idx = self
            .builder
            .build_int_add(
                carry_idx,
                self.i64_type.const_int(1, false),
                &format!("{label}_carry_next_idx"),
            )
            .expect("failed bigint mul carry next idx");
        self.builder
            .build_unconditional_branch(carry_loop)
            .expect("failed bigint mul carry loop");
        let carry_body_end = self.builder.get_insert_block().unwrap();
        carry_idx_phi.add_incoming(&[(&next_idx, carry_body_end)]);
        carry_val_phi.add_incoming(&[(&next_carry, carry_body_end)]);

        self.builder.position_at_end(carry_done);
        let next_i = self
            .builder
            .build_int_add(
                i,
                self.i64_type.const_int(1, false),
                &format!("{label}_next_i"),
            )
            .expect("failed bigint mul next i");
        self.builder
            .build_unconditional_branch(outer_loop)
            .expect("failed bigint mul outer loop");
        let outer_body_end = self.builder.get_insert_block().unwrap();
        i_phi.add_incoming(&[(&next_i, outer_body_end)]);

        self.builder.position_at_end(outer_done);
        self.build_bigint_normalize(result, &format!("{label}_norm"));
        result
    }

    fn build_bigint_signed_addsub(
        &self,
        lhs: IntValue<'ctx>,
        lhs_sign: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        rhs_sign: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let merge = self
            .context
            .append_basic_block(function, &format!("{label}_merge"));
        let lhs_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                self.i64_type.const_zero(),
                &format!("{label}_lhs_zero"),
            )
            .expect("failed bigint signed lhs_zero");
        let lhs_zero_block = self
            .context
            .append_basic_block(function, &format!("{label}_lhs_zero_block"));
        let rhs_zero_check = self
            .context
            .append_basic_block(function, &format!("{label}_rhs_zero_check"));
        self.builder
            .build_conditional_branch(lhs_zero, lhs_zero_block, rhs_zero_check)
            .expect("failed bigint signed lhs_zero branch");

        self.builder.position_at_end(lhs_zero_block);
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed lhs_zero merge");
        let lhs_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_zero_check);
        let rhs_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rhs_sign,
                self.i64_type.const_zero(),
                &format!("{label}_rhs_zero"),
            )
            .expect("failed bigint signed rhs_zero");
        let rhs_zero_block = self
            .context
            .append_basic_block(function, &format!("{label}_rhs_zero_block"));
        let same_sign_block = self
            .context
            .append_basic_block(function, &format!("{label}_same_sign"));
        self.builder
            .build_conditional_branch(rhs_zero, rhs_zero_block, same_sign_block)
            .expect("failed bigint signed rhs_zero branch");

        self.builder.position_at_end(rhs_zero_block);
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed rhs_zero merge");
        let rhs_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(same_sign_block);
        let signs_equal = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                rhs_sign,
                &format!("{label}_signs_equal"),
            )
            .expect("failed bigint signed signs_equal");
        let add_block = self
            .context
            .append_basic_block(function, &format!("{label}_add_block"));
        let diff_sign_block = self
            .context
            .append_basic_block(function, &format!("{label}_diff_sign"));
        self.builder
            .build_conditional_branch(signs_equal, add_block, diff_sign_block)
            .expect("failed bigint signed sign branch");

        self.builder.position_at_end(add_block);
        let sum_ptr = self.build_bigint_add_abs(lhs, rhs, &format!("{label}_add_abs"));
        self.build_bigint_sign_store(sum_ptr, lhs_sign, &format!("{label}_sum_sign"));
        self.build_bigint_normalize(sum_ptr, &format!("{label}_sum_norm"));
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed add merge");
        let add_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(diff_sign_block);
        let cmp = self.build_bigint_cmp_abs(lhs, rhs, &format!("{label}_cmp_abs"));
        let cmp_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                cmp,
                self.i64_type.const_zero(),
                &format!("{label}_cmp_zero"),
            )
            .expect("failed bigint signed cmp_zero");
        let equal_block = self
            .context
            .append_basic_block(function, &format!("{label}_equal"));
        let non_zero_block = self
            .context
            .append_basic_block(function, &format!("{label}_non_zero"));
        self.builder
            .build_conditional_branch(cmp_zero, equal_block, non_zero_block)
            .expect("failed bigint signed cmp branch");

        self.builder.position_at_end(equal_block);
        let zero_ptr =
            self.build_bigint_alloc(self.i64_type.const_zero(), &format!("{label}_zero_alloc"));
        self.build_bigint_sign_store(
            zero_ptr,
            self.i64_type.const_zero(),
            &format!("{label}_zero_sign"),
        );
        self.build_bigint_len_store(
            zero_ptr,
            self.i64_type.const_zero(),
            &format!("{label}_zero_len"),
        );
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed equal merge");
        let equal_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(non_zero_block);
        let lhs_gt = self
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                cmp,
                self.i64_type.const_zero(),
                &format!("{label}_lhs_gt"),
            )
            .expect("failed bigint signed lhs_gt");
        let lhs_gt_block = self
            .context
            .append_basic_block(function, &format!("{label}_lhs_gt_block"));
        let rhs_gt_block = self
            .context
            .append_basic_block(function, &format!("{label}_rhs_gt_block"));
        self.builder
            .build_conditional_branch(lhs_gt, lhs_gt_block, rhs_gt_block)
            .expect("failed bigint signed lhs_gt branch");

        self.builder.position_at_end(lhs_gt_block);
        let lhs_diff = self.build_bigint_sub_abs(lhs, rhs, &format!("{label}_lhs_diff"));
        self.build_bigint_sign_store(lhs_diff, lhs_sign, &format!("{label}_lhs_diff_sign"));
        self.build_bigint_normalize(lhs_diff, &format!("{label}_lhs_diff_norm"));
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed lhs_gt merge");
        let lhs_gt_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_gt_block);
        let rhs_diff = self.build_bigint_sub_abs(rhs, lhs, &format!("{label}_rhs_diff"));
        self.build_bigint_sign_store(rhs_diff, rhs_sign, &format!("{label}_rhs_diff_sign"));
        self.build_bigint_normalize(rhs_diff, &format!("{label}_rhs_diff_norm"));
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed rhs_gt merge");
        let rhs_gt_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge);
        let result_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_result_phi"))
            .expect("failed bigint signed result phi");
        result_phi.add_incoming(&[
            (&rhs, lhs_zero_end),
            (&lhs, rhs_zero_end),
            (&sum_ptr, add_end),
            (&zero_ptr, equal_end),
            (&lhs_diff, lhs_gt_end),
            (&rhs_diff, rhs_gt_end),
        ]);
        result_phi.as_basic_value().into_int_value()
    }

    fn build_bigint_signed_compare(
        &self,
        lhs: IntValue<'ctx>,
        lhs_sign: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        rhs_sign: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let merge = self
            .context
            .append_basic_block(function, &format!("{label}_merge"));
        let signs_equal_block = self
            .context
            .append_basic_block(function, &format!("{label}_signs_equal"));
        let signs_diff_block = self
            .context
            .append_basic_block(function, &format!("{label}_signs_diff"));
        let signs_equal = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                rhs_sign,
                &format!("{label}_signs_equal_cmp"),
            )
            .expect("failed bigint signed compare signs_equal");
        self.builder
            .build_conditional_branch(signs_equal, signs_equal_block, signs_diff_block)
            .expect("failed bigint signed compare branch");

        self.builder.position_at_end(signs_diff_block);
        let lhs_gt = self
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                lhs_sign,
                rhs_sign,
                &format!("{label}_lhs_sign_gt"),
            )
            .expect("failed bigint signed compare lhs_gt");
        let diff_cmp = self
            .builder
            .build_select(
                lhs_gt,
                self.i64_type.const_int(1, true),
                self.i64_type.const_int((-1i64) as u64, true),
                &format!("{label}_diff_cmp"),
            )
            .expect("failed bigint signed compare diff select")
            .into_int_value();
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed bigint signed compare diff merge");
        let signs_diff_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(signs_equal_block);
        let sign_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                self.i64_type.const_zero(),
                &format!("{label}_sign_zero"),
            )
            .expect("failed bigint signed compare sign_zero");
        let zero_block = self
            .context
            .append_basic_block(function, &format!("{label}_zero"));
        let cmp_block = self
            .context
            .append_basic_block(function, &format!("{label}_cmp"));
        self.builder
            .build_conditional_branch(sign_zero, zero_block, cmp_block)
            .expect("failed bigint signed compare zero branch");

        self.builder.position_at_end(zero_block);
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed bigint signed compare zero merge");
        let zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(cmp_block);
        let cmp = self.build_bigint_cmp_abs(lhs, rhs, &format!("{label}_cmp_abs"));
        let sign_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                lhs_sign,
                self.i64_type.const_zero(),
                &format!("{label}_sign_negative"),
            )
            .expect("failed bigint signed compare sign_negative");
        let neg_block = self
            .context
            .append_basic_block(function, &format!("{label}_neg"));
        let pos_block = self
            .context
            .append_basic_block(function, &format!("{label}_pos"));
        self.builder
            .build_conditional_branch(sign_negative, neg_block, pos_block)
            .expect("failed bigint signed compare neg branch");

        self.builder.position_at_end(pos_block);
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed bigint signed compare pos merge");
        let pos_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(neg_block);
        let neg_cmp = self
            .builder
            .build_int_neg(cmp, &format!("{label}_neg_cmp"))
            .expect("failed bigint signed compare neg cmp");
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed bigint signed compare neg merge");
        let neg_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge);
        let result_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_result_phi"))
            .expect("failed bigint signed compare result phi");
        result_phi.add_incoming(&[
            (&diff_cmp, signs_diff_end),
            (&self.i64_type.const_zero(), zero_end),
            (&cmp, pos_end),
            (&neg_cmp, neg_end),
        ]);
        result_phi.as_basic_value().into_int_value()
    }

    fn build_list_len_load(&self, payload: IntValue<'ctx>, label: &str) -> IntValue<'ctx> {
        let list_ptr = self.build_list_header_ptr(payload, label);
        let len_ptr = self
            .builder
            .build_struct_gep(
                self.list_header_type(),
                list_ptr,
                1,
                &format!("{label}_len_ptr"),
            )
            .expect("failed to build list len gep");
        self.builder
            .build_load(self.i64_type, len_ptr, &format!("{label}_len"))
            .expect("failed to load list len")
            .into_int_value()
    }

    fn build_list_len_store(&self, payload: IntValue<'ctx>, len: IntValue<'ctx>, label: &str) {
        let list_ptr = self.build_list_header_ptr(payload, label);
        let len_ptr = self
            .builder
            .build_struct_gep(
                self.list_header_type(),
                list_ptr,
                1,
                &format!("{label}_len_ptr"),
            )
            .expect("failed to build list len gep");
        self.builder
            .build_store(len_ptr, len)
            .expect("failed to store list len");
    }

    fn build_list_cap_load(&self, payload: IntValue<'ctx>, label: &str) -> IntValue<'ctx> {
        let list_ptr = self.build_list_header_ptr(payload, label);
        let cap_ptr = self
            .builder
            .build_struct_gep(
                self.list_header_type(),
                list_ptr,
                2,
                &format!("{label}_cap_ptr"),
            )
            .expect("failed to build list cap gep");
        self.builder
            .build_load(self.i64_type, cap_ptr, &format!("{label}_cap"))
            .expect("failed to load list cap")
            .into_int_value()
    }

    fn build_list_cap_store(&self, payload: IntValue<'ctx>, cap: IntValue<'ctx>, label: &str) {
        let list_ptr = self.build_list_header_ptr(payload, label);
        let cap_ptr = self
            .builder
            .build_struct_gep(
                self.list_header_type(),
                list_ptr,
                2,
                &format!("{label}_cap_ptr"),
            )
            .expect("failed to build list cap gep");
        self.builder
            .build_store(cap_ptr, cap)
            .expect("failed to store list cap");
    }

    fn build_list_data_ptr_load(&self, payload: IntValue<'ctx>, label: &str) -> PointerValue<'ctx> {
        let list_ptr = self.build_list_header_ptr(payload, label);
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(
                self.list_header_type(),
                list_ptr,
                0,
                &format!("{label}_data_ptr_ptr"),
            )
            .expect("failed to build list data ptr gep");
        match self.runtime_mode {
            LlvmRuntimeMode::Native => self
                .builder
                .build_load(
                    self.context.ptr_type(Default::default()),
                    data_ptr_ptr,
                    &format!("{label}_data_ptr"),
                )
                .expect("failed to load list data ptr")
                .into_pointer_value(),
            LlvmRuntimeMode::Wasm => {
                let raw = self
                    .builder
                    .build_load(self.i64_type, data_ptr_ptr, &format!("{label}_data_raw"))
                    .expect("failed to load wasm list data ptr")
                    .into_int_value();
                self.builder
                    .build_int_to_ptr(
                        raw,
                        self.context.ptr_type(Default::default()),
                        &format!("{label}_data_ptr"),
                    )
                    .expect("failed to convert wasm list data ptr")
            }
            #[cfg(feature = "wasi")]
            LlvmRuntimeMode::WasiPreview1Command => {
                let raw = self
                    .builder
                    .build_load(self.i64_type, data_ptr_ptr, &format!("{label}_data_raw"))
                    .expect("failed to load wasi list data ptr")
                    .into_int_value();
                self.builder
                    .build_int_to_ptr(
                        raw,
                        self.context.ptr_type(Default::default()),
                        &format!("{label}_data_ptr"),
                    )
                    .expect("failed to convert wasi list data ptr")
            }
        }
    }

    fn build_list_data_ptr_store(
        &self,
        list_ptr: PointerValue<'ctx>,
        data_ptr: PointerValue<'ctx>,
        label: &str,
    ) {
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(
                self.list_header_type(),
                list_ptr,
                0,
                &format!("{label}_data_ptr_ptr"),
            )
            .expect("failed to build list data ptr gep");
        match self.runtime_mode {
            LlvmRuntimeMode::Native => {
                self.builder
                    .build_store(data_ptr_ptr, data_ptr)
                    .expect("failed to store list data ptr");
            }
            LlvmRuntimeMode::Wasm => {
                let raw = self
                    .builder
                    .build_ptr_to_int(data_ptr, self.i64_type, &format!("{label}_data_raw"))
                    .expect("failed to convert wasm list data ptr");
                self.builder
                    .build_store(data_ptr_ptr, raw)
                    .expect("failed to store wasm list data ptr");
            }
            #[cfg(feature = "wasi")]
            LlvmRuntimeMode::WasiPreview1Command => {
                let raw = self
                    .builder
                    .build_ptr_to_int(data_ptr, self.i64_type, &format!("{label}_data_raw"))
                    .expect("failed to convert wasi list data ptr");
                self.builder
                    .build_store(data_ptr_ptr, raw)
                    .expect("failed to store wasi list data ptr");
            }
        }
    }

    fn build_list_value_ptr(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        let data_ptr = self.build_list_data_ptr_load(payload, label);
        unsafe {
            self.builder
                .build_gep(
                    self.value_type(),
                    data_ptr,
                    &[index],
                    &format!("{label}_value_ptr"),
                )
                .expect("failed to build list value gep")
        }
    }

    fn build_list_value_ptr_from_data_ptr(
        &self,
        data_ptr: PointerValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        unsafe {
            self.builder
                .build_gep(
                    self.value_type(),
                    data_ptr,
                    &[index],
                    &format!("{label}_value_ptr"),
                )
                .expect("failed to build list value gep from data ptr")
        }
    }

    fn build_list_value_load(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) -> CompiledValue<'ctx> {
        let value_ptr = self.build_list_value_ptr(payload, index, label);
        let tag = self
            .builder
            .build_int_z_extend(
                self.build_value_tag_load(value_ptr, label),
                self.i64_type,
                &format!("{label}_tag_i64"),
            )
            .expect("failed to extend list value tag");
        let payload = self.build_value_payload_load(value_ptr, label);
        CompiledValue { tag, payload }
    }

    fn build_list_value_load_from_data_ptr(
        &self,
        data_ptr: PointerValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) -> CompiledValue<'ctx> {
        let value_ptr = self.build_list_value_ptr_from_data_ptr(data_ptr, index, label);
        let tag = self
            .builder
            .build_int_z_extend(
                self.build_value_tag_load(value_ptr, label),
                self.i64_type,
                &format!("{label}_tag_i64"),
            )
            .expect("failed to extend list value tag");
        let payload = self.build_value_payload_load(value_ptr, label);
        CompiledValue { tag, payload }
    }

    fn build_list_value_store(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        value: CompiledValue<'ctx>,
        label: &str,
    ) {
        let value_ptr = self.build_list_value_ptr(payload, index, label);
        let tag_ptr = self
            .builder
            .build_struct_gep(self.value_type(), value_ptr, 0, &format!("{label}_tag_ptr"))
            .expect("failed to build list value tag gep");
        let payload_ptr = self
            .builder
            .build_struct_gep(
                self.value_type(),
                value_ptr,
                2,
                &format!("{label}_payload_ptr"),
            )
            .expect("failed to build list value payload gep");
        let tag = self
            .builder
            .build_int_truncate(
                value.tag,
                self.context.i8_type(),
                &format!("{label}_tag_i8"),
            )
            .expect("failed to truncate list value tag");
        self.builder
            .build_store(tag_ptr, tag)
            .expect("failed to store list value tag");
        self.builder
            .build_store(payload_ptr, value.payload)
            .expect("failed to store list value payload");
    }

    fn build_list_value_store_from_data_ptr(
        &self,
        data_ptr: PointerValue<'ctx>,
        index: IntValue<'ctx>,
        value: CompiledValue<'ctx>,
        label: &str,
    ) {
        let value_ptr = self.build_list_value_ptr_from_data_ptr(data_ptr, index, label);
        let tag_ptr = self
            .builder
            .build_struct_gep(self.value_type(), value_ptr, 0, &format!("{label}_tag_ptr"))
            .expect("failed to build list value tag gep");
        let payload_ptr = self
            .builder
            .build_struct_gep(
                self.value_type(),
                value_ptr,
                2,
                &format!("{label}_payload_ptr"),
            )
            .expect("failed to build list value payload gep");
        let tag = self
            .builder
            .build_int_truncate(
                value.tag,
                self.context.i8_type(),
                &format!("{label}_tag_i8"),
            )
            .expect("failed to truncate list value tag");
        self.builder
            .build_store(tag_ptr, tag)
            .expect("failed to store list value tag");
        self.builder
            .build_store(payload_ptr, value.payload)
            .expect("failed to store list value payload");
    }

    fn build_index_bounds_check(
        &self,
        list_payload: IntValue<'ctx>,
        idx: IntValue<'ctx>,
        label: &str,
        trap_block: inkwell::basic_block::BasicBlock<'ctx>,
    ) {
        let len = self.build_list_len_load(list_payload, label);
        let in_bounds = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, &format!("{label}_in_bounds"))
            .expect("failed to compare list bounds");
        let ok_block = self.context.append_basic_block(
            self.builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap(),
            &format!("{label}_bounds_ok"),
        );
        self.builder
            .build_conditional_branch(in_bounds, ok_block, trap_block)
            .expect("failed to branch on list bounds");
        self.builder.position_at_end(ok_block);
    }

    fn value_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context.struct_type(
            &[
                self.context.i8_type().into(),
                self.context.i8_type().array_type(7).into(),
                self.i64_type.into(),
            ],
            false,
        )
    }

    fn closure_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context
            .struct_type(&[self.i64_type.into(), self.i64_type.into()], false)
    }

    fn list_header_type(&self) -> inkwell::types::StructType<'ctx> {
        let data_ptr_field = match self.runtime_mode {
            LlvmRuntimeMode::Native => self.context.ptr_type(Default::default()).into(),
            LlvmRuntimeMode::Wasm => self.i64_type.into(),
            #[cfg(feature = "wasi")]
            LlvmRuntimeMode::WasiPreview1Command => self.i64_type.into(),
        };
        self.context.struct_type(
            &[data_ptr_field, self.i64_type.into(), self.i64_type.into()],
            false,
        )
    }

    fn bigint_header_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context.struct_type(
            &[
                self.i64_type.into(),
                self.i64_type.into(),
                self.i64_type.into(),
                self.context.ptr_type(Default::default()).into(),
            ],
            false,
        )
    }

    fn build_value_tag_load(&self, value_ptr: PointerValue<'ctx>, label: &str) -> IntValue<'ctx> {
        let tag_ptr = self
            .builder
            .build_struct_gep(self.value_type(), value_ptr, 0, &format!("{label}_tag_ptr"))
            .expect("failed to build value tag gep");
        self.builder
            .build_load(self.context.i8_type(), tag_ptr, &format!("{label}_tag"))
            .expect("failed to load value tag")
            .into_int_value()
    }

    fn build_value_payload_load(
        &self,
        value_ptr: PointerValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let payload_ptr = self
            .builder
            .build_struct_gep(
                self.value_type(),
                value_ptr,
                2,
                &format!("{label}_payload_ptr"),
            )
            .expect("failed to build value payload gep");
        self.builder
            .build_load(self.i64_type, payload_ptr, &format!("{label}_payload"))
            .expect("failed to load value payload")
            .into_int_value()
    }

    fn define_value_to_i64(&mut self) {
        let function = self.module.add_function(
            "llvm_rt_value_to_i64",
            self.i64_type
                .fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions
            .insert("__value_to_i64".to_string(), function);

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

        self.builder
            .build_return(Some(&raw))
            .expect("failed to return raw int");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_value_is_truthy(&mut self) {
        let function = self.module.add_function(
            "llvm_rt_value_is_truthy",
            self.i64_type
                .fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions
            .insert("__value_is_truthy".to_string(), function);

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
        self.builder
            .build_return(Some(&int_raw))
            .expect("failed to return int truthiness");

        self.builder.position_at_end(list_check_block);
        self.builder
            .build_conditional_branch(is_list, list_block, trap_block)
            .expect("failed to validate list truthiness");

        self.builder.position_at_end(list_block);
        let list_ptr = self
            .builder
            .build_int_to_ptr(
                payload,
                self.context.ptr_type(Default::default()),
                "truthy_list_ptr",
            )
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

        self.builder
            .build_return(Some(&list_raw))
            .expect("failed to return truthiness");

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
        let int_ok_block = self.context.append_basic_block(function, "int_ok");
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
        let both_int = self
            .builder
            .build_and(lhs_is_int, rhs_is_int, "both_int")
            .expect("failed both_int");
        self.builder
            .build_conditional_branch(both_int, int_block, non_int_block)
            .expect("failed to branch on int operands");

        self.builder.position_at_end(int_block);
        let lhs_raw = lhs_payload;
        let rhs_raw = rhs_payload;
        let raw = match op {
            BinaryArithOp::Add => {
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
                let rem_ok = self.build_division_safe_check(lhs_raw, rhs_raw, "rem");
                self.builder
                    .build_conditional_branch(rem_ok, int_ok_block, trap_block)
                    .expect("failed to build rem branch");
                self.builder.position_at_end(int_ok_block);
                self.builder
                    .build_int_signed_rem(lhs_raw, rhs_raw, "rem")
                    .expect("failed to modulo")
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
            let both_bigint = self
                .builder
                .build_and(lhs_is_bigint, rhs_is_bigint, "both_bigint")
                .expect("failed both_bigint");
            let bigint_block = self.context.append_basic_block(function, "bigint");
            let lhs_promote_check_block = self
                .context
                .append_basic_block(function, "lhs_promote_check");
            let lhs_promote_block = self.context.append_basic_block(function, "lhs_promote");
            let rhs_maybe_promote_block = self
                .context
                .append_basic_block(function, "rhs_maybe_promote");
            let rhs_promote_block = self.context.append_basic_block(function, "rhs_promote");
            self.builder
                .build_conditional_branch(both_bigint, bigint_block, lhs_promote_check_block)
                .expect("failed bigint branch");

            self.builder.position_at_end(bigint_block);
            let result = self.build_internal_call(
                self.require_func(bigint_name),
                &[
                    CompiledValue {
                        tag: lhs_tag,
                        payload: lhs_payload,
                    },
                    CompiledValue {
                        tag: rhs_tag,
                        payload: rhs_payload,
                    },
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
                &[CompiledValue {
                    tag: lhs_tag,
                    payload: lhs_payload,
                }],
                "lhs_promoted_bigint",
            );
            let result = self.build_internal_call(
                self.require_func(bigint_name),
                &[
                    lhs_big,
                    CompiledValue {
                        tag: rhs_tag,
                        payload: rhs_payload,
                    },
                ],
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
                &[CompiledValue {
                    tag: rhs_tag,
                    payload: rhs_payload,
                }],
                "rhs_promoted_bigint",
            );
            let result = self.build_internal_call(
                self.require_func(bigint_name),
                &[
                    CompiledValue {
                        tag: lhs_tag,
                        payload: lhs_payload,
                    },
                    rhs_big,
                ],
                "mixed_bigint_op_rhs",
            );
            self.builder
                .build_return(Some(&self.make_pair_value(
                    result.tag,
                    result.payload,
                    "mixed_bigint_op_rhs_result",
                )))
                .expect("failed to return mixed bigint rhs op result");
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
            let lhs_promote_check_block = self
                .context
                .append_basic_block(function, "lhs_promote_check");
            let lhs_promote_block = self.context.append_basic_block(function, "lhs_promote");
            let rhs_maybe_promote_block = self
                .context
                .append_basic_block(function, "rhs_maybe_promote");
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

    fn define_pair_bigint_from_int(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type()
                .fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);
        self.functions
            .insert("bigint_from_int".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let value = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let trap_block = self.context.append_basic_block(function, "trap");
        let raw = self.expect_tag_int(value, "bigint_from_int", trap_block);
        let zero_block = self.context.append_basic_block(function, "zero");
        let non_zero_block = self.context.append_basic_block(function, "non_zero");
        let merge_block = self.context.append_basic_block(function, "merge");
        let is_zero = self
            .builder
            .build_int_compare(IntPredicate::EQ, raw, self.i64_type.const_zero(), "is_zero")
            .expect("failed bigint_from_int zero compare");
        self.builder
            .build_conditional_branch(is_zero, zero_block, non_zero_block)
            .expect("failed bigint_from_int zero branch");

        self.builder.position_at_end(zero_block);
        let zero_ptr = self.build_bigint_alloc(self.i64_type.const_zero(), "bigint_zero");
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint_from_int zero merge");
        let zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(non_zero_block);
        let is_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                raw,
                self.i64_type.const_zero(),
                "is_negative",
            )
            .expect("failed bigint_from_int neg compare");
        let neg_block = self.context.append_basic_block(function, "neg");
        let pos_block = self.context.append_basic_block(function, "pos");
        let sign_merge = self.context.append_basic_block(function, "sign_merge");
        self.builder
            .build_conditional_branch(is_negative, neg_block, pos_block)
            .expect("failed bigint_from_int sign branch");

        self.builder.position_at_end(neg_block);
        let neg_abs = self
            .builder
            .build_int_neg(raw, "neg_abs")
            .expect("failed bigint_from_int neg abs");
        self.builder
            .build_unconditional_branch(sign_merge)
            .expect("failed bigint_from_int neg merge");
        let neg_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(pos_block);
        self.builder
            .build_unconditional_branch(sign_merge)
            .expect("failed bigint_from_int pos merge");
        let pos_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(sign_merge);
        let sign_phi = self
            .builder
            .build_phi(self.i64_type, "sign_phi")
            .expect("failed bigint_from_int sign phi");
        let abs_phi = self
            .builder
            .build_phi(self.i64_type, "abs_phi")
            .expect("failed bigint_from_int abs phi");
        sign_phi.add_incoming(&[
            (&self.i64_type.const_int((-1i64) as u64, true), neg_end),
            (&self.i64_type.const_int(1, true), pos_end),
        ]);
        abs_phi.add_incoming(&[(&neg_abs, neg_end), (&raw, pos_end)]);
        let sign = sign_phi.as_basic_value().into_int_value();
        let abs = abs_phi.as_basic_value().into_int_value();
        let high = self
            .builder
            .build_right_shift(abs, self.i64_type.const_int(32, false), false, "high")
            .expect("failed bigint_from_int high");
        let has_high = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                high,
                self.i64_type.const_zero(),
                "has_high",
            )
            .expect("failed bigint_from_int has_high");
        let cap_merge = self.context.append_basic_block(function, "cap_merge");
        let high_block = self.context.append_basic_block(function, "high_block");
        let low_block = self.context.append_basic_block(function, "low_block");
        self.builder
            .build_conditional_branch(has_high, high_block, low_block)
            .expect("failed bigint_from_int cap branch");

        self.builder.position_at_end(high_block);
        self.builder
            .build_unconditional_branch(cap_merge)
            .expect("failed bigint_from_int high merge");
        let high_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(low_block);
        self.builder
            .build_unconditional_branch(cap_merge)
            .expect("failed bigint_from_int low merge");
        let low_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(cap_merge);
        let cap_phi = self
            .builder
            .build_phi(self.i64_type, "cap_phi")
            .expect("failed bigint_from_int cap phi");
        cap_phi.add_incoming(&[
            (&self.i64_type.const_int(2, false), high_end),
            (&self.i64_type.const_int(1, false), low_end),
        ]);
        let cap = cap_phi.as_basic_value().into_int_value();
        let ptr = self.build_bigint_alloc(cap, "bigint_from_int_alloc");
        self.build_bigint_sign_store(ptr, sign, "bigint_from_int_sign");
        self.build_bigint_len_store(ptr, cap, "bigint_from_int_len");
        let low = self
            .builder
            .build_and(abs, self.i64_type.const_int(0xffff_ffff, false), "low")
            .expect("failed bigint_from_int low");
        self.build_bigint_limb_store(ptr, self.i64_type.const_zero(), low, "bigint_from_int_low");
        let has_second = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                cap,
                self.i64_type.const_int(2, false),
                "has_second",
            )
            .expect("failed bigint_from_int has_second");
        let second_block = self.context.append_basic_block(function, "second");
        let second_done = self.context.append_basic_block(function, "second_done");
        self.builder
            .build_conditional_branch(has_second, second_block, second_done)
            .expect("failed bigint_from_int second branch");

        self.builder.position_at_end(second_block);
        self.build_bigint_limb_store(
            ptr,
            self.i64_type.const_int(1, false),
            high,
            "bigint_from_int_high",
        );
        self.builder
            .build_unconditional_branch(second_done)
            .expect("failed bigint_from_int second done");

        self.builder.position_at_end(second_done);
        self.build_bigint_normalize(ptr, "bigint_from_int_norm");
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint_from_int merge");
        let non_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_block);
        let result_phi = self
            .builder
            .build_phi(self.i64_type, "result_phi")
            .expect("failed bigint_from_int result phi");
        result_phi.add_incoming(&[(&zero_ptr, zero_end), (&ptr, non_zero_end)]);
        let result_ptr = result_phi.as_basic_value().into_int_value();
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                result_ptr,
                "bigint_from_int_result",
            )))
            .expect("failed bigint_from_int return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_bigint_add(&mut self, name: &str, symbol: &str) {
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
        self.functions.insert("bigint_add".to_string(), function);
        let entry = self.context.append_basic_block(function, "entry");
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
        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_add_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);
        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_add_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);
        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_add_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_add_rhs");
        let result_ptr =
            self.build_bigint_signed_addsub(lhs_ptr, lhs_sign, rhs_ptr, rhs_sign, "bigint_add");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                result_ptr,
                "bigint_add_result",
            )))
            .expect("failed bigint_add return");
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_bigint_compare(&mut self, name: &str, symbol: &str) {
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
        self.functions
            .insert("bigint_compare".to_string(), function);
        let entry = self.context.append_basic_block(function, "entry");
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
        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_cmp_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);
        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_cmp_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);
        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_cmp_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_cmp_rhs");
        let raw =
            self.build_bigint_signed_compare(lhs_ptr, lhs_sign, rhs_ptr, rhs_sign, "bigint_cmp");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                raw,
                "bigint_cmp_result",
            )))
            .expect("failed bigint_compare return");
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_bigint_subtract(&mut self, name: &str, symbol: &str) {
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
        self.functions
            .insert("bigint_subtract".to_string(), function);
        let entry = self.context.append_basic_block(function, "entry");
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
        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_sub_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);
        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_sub_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);
        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_sub_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_sub_rhs");
        let neg_rhs_sign = self
            .builder
            .build_int_sub(self.i64_type.const_zero(), rhs_sign, "neg_rhs_sign")
            .expect("failed bigint_sub neg rhs sign");
        let result_ptr =
            self.build_bigint_signed_addsub(lhs_ptr, lhs_sign, rhs_ptr, neg_rhs_sign, "bigint_sub");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                result_ptr,
                "bigint_sub_result",
            )))
            .expect("failed bigint_sub return");
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_bigint_multiply(&mut self, name: &str, symbol: &str) {
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
        self.functions
            .insert("bigint_multiply".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
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

        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_mul_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);

        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_mul_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);

        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_mul_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_mul_rhs");

        let lhs_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                self.i64_type.const_zero(),
                "bigint_mul_lhs_zero",
            )
            .expect("failed bigint_mul lhs zero compare");
        let rhs_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rhs_sign,
                self.i64_type.const_zero(),
                "bigint_mul_rhs_zero",
            )
            .expect("failed bigint_mul rhs zero compare");
        let is_zero = self
            .builder
            .build_or(lhs_zero, rhs_zero, "bigint_mul_is_zero")
            .expect("failed bigint_mul zero or");

        let zero_block = self.context.append_basic_block(function, "zero");
        let mul_block = self.context.append_basic_block(function, "mul");
        let merge_block = self.context.append_basic_block(function, "merge");
        self.builder
            .build_conditional_branch(is_zero, zero_block, mul_block)
            .expect("failed bigint_mul zero branch");

        self.builder.position_at_end(zero_block);
        let zero_ptr = self.build_bigint_alloc(self.i64_type.const_zero(), "bigint_mul_zero");
        self.build_bigint_sign_store(zero_ptr, self.i64_type.const_zero(), "bigint_mul_zero");
        self.build_bigint_len_store(zero_ptr, self.i64_type.const_zero(), "bigint_mul_zero");
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint_mul zero merge");
        let zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(mul_block);
        let abs_ptr = self.build_bigint_mul_abs(lhs_ptr, rhs_ptr, "bigint_mul_abs");
        let same_sign = self
            .builder
            .build_int_compare(IntPredicate::EQ, lhs_sign, rhs_sign, "bigint_mul_same_sign")
            .expect("failed bigint_mul same sign compare");
        let result_sign = self
            .builder
            .build_select(
                same_sign,
                self.i64_type.const_int(1, true),
                self.i64_type.const_int((-1i64) as u64, true),
                "bigint_mul_result_sign",
            )
            .expect("failed bigint_mul sign select")
            .into_int_value();
        self.build_bigint_sign_store(abs_ptr, result_sign, "bigint_mul_sign");
        self.build_bigint_normalize(abs_ptr, "bigint_mul_norm");
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint_mul mul merge");
        let mul_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_block);
        let result_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_mul_result_ptr")
            .expect("failed bigint_mul result phi");
        result_phi.add_incoming(&[(&zero_ptr, zero_end), (&abs_ptr, mul_end)]);
        let result_ptr = result_phi.as_basic_value().into_int_value();
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                result_ptr,
                "bigint_mul_result",
            )))
            .expect("failed bigint_mul return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_bigint_divide(&mut self, name: &str, symbol: &str) {
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
        self.functions.insert("bigint_divide".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
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

        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_div_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);

        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_div_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);

        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_div_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_div_rhs");

        let rhs_is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rhs_sign,
                self.i64_type.const_zero(),
                "bigint_div_rhs_zero",
            )
            .expect("failed bigint_div rhs zero compare");
        let lhs_is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                self.i64_type.const_zero(),
                "bigint_div_lhs_zero",
            )
            .expect("failed bigint_div lhs zero compare");

        let zero_block = self.context.append_basic_block(function, "zero");
        let work_block = self.context.append_basic_block(function, "work");
        let init_block = self.context.append_basic_block(function, "init");
        self.builder
            .build_conditional_branch(rhs_is_zero, trap_block, work_block)
            .expect("failed bigint_div rhs zero branch");

        self.builder.position_at_end(work_block);
        let outer_loop = self.context.append_basic_block(function, "outer_loop");
        let outer_body = self.context.append_basic_block(function, "outer_body");
        let outer_done = self.context.append_basic_block(function, "outer_done");
        self.builder
            .build_conditional_branch(lhs_is_zero, zero_block, init_block)
            .expect("failed bigint_div lhs zero branch");

        self.builder.position_at_end(zero_block);
        let zero_ptr = self.build_bigint_zero("bigint_div_zero");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                zero_ptr,
                "bigint_div_zero_result",
            )))
            .expect("failed bigint_div zero return");

        self.builder.position_at_end(init_block);
        let quotient0 = self.build_bigint_zero("bigint_div_q0");
        self.builder
            .build_unconditional_branch(outer_loop)
            .expect("failed bigint_div init jump");
        let init_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(outer_loop);
        let quotient_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_div_quotient")
            .expect("failed bigint_div quotient phi");
        let remainder_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_div_remainder")
            .expect("failed bigint_div remainder phi");
        quotient_phi.add_incoming(&[(&quotient0, init_end)]);
        remainder_phi.add_incoming(&[(&lhs_ptr, init_end)]);
        let quotient = quotient_phi.as_basic_value().into_int_value();
        let remainder = remainder_phi.as_basic_value().into_int_value();
        let cmp = self.build_bigint_cmp_abs(remainder, rhs_ptr, "bigint_div_outer_cmp");
        let has_more = self
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                cmp,
                self.i64_type.const_zero(),
                "bigint_div_has_more",
            )
            .expect("failed bigint_div outer cmp check");
        self.builder
            .build_conditional_branch(has_more, outer_body, outer_done)
            .expect("failed bigint_div outer branch");

        self.builder.position_at_end(outer_body);
        let inner_loop = self.context.append_basic_block(function, "inner_loop");
        let inner_body = self.context.append_basic_block(function, "inner_body");
        let inner_done = self.context.append_basic_block(function, "inner_done");
        let multiple0 = self.build_bigint_one("bigint_div_m1");
        self.builder
            .build_unconditional_branch(inner_loop)
            .expect("failed bigint_div inner jump");
        let inner_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(inner_loop);
        let current_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_div_current")
            .expect("failed bigint_div current phi");
        let multiple_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_div_multiple")
            .expect("failed bigint_div multiple phi");
        current_phi.add_incoming(&[(&rhs_ptr, inner_entry_end)]);
        multiple_phi.add_incoming(&[(&multiple0, inner_entry_end)]);
        let current = current_phi.as_basic_value().into_int_value();
        let multiple = multiple_phi.as_basic_value().into_int_value();
        let doubled = self.build_bigint_add_abs(current, current, "bigint_div_doubled");
        let doubled_cmp = self.build_bigint_cmp_abs(doubled, remainder, "bigint_div_doubled_cmp");
        let can_double = self
            .builder
            .build_int_compare(
                IntPredicate::SLE,
                doubled_cmp,
                self.i64_type.const_zero(),
                "bigint_div_can_double",
            )
            .expect("failed bigint_div can_double");
        self.builder
            .build_conditional_branch(can_double, inner_body, inner_done)
            .expect("failed bigint_div inner branch");

        self.builder.position_at_end(inner_body);
        let doubled_multiple =
            self.build_bigint_add_abs(multiple, multiple, "bigint_div_doubled_multiple");
        self.builder
            .build_unconditional_branch(inner_loop)
            .expect("failed bigint_div inner loop");
        let inner_body_end = self.builder.get_insert_block().unwrap();
        current_phi.add_incoming(&[(&doubled, inner_body_end)]);
        multiple_phi.add_incoming(&[(&doubled_multiple, inner_body_end)]);

        self.builder.position_at_end(inner_done);
        let best_current = current_phi.as_basic_value().into_int_value();
        let best_multiple = multiple_phi.as_basic_value().into_int_value();
        let next_remainder =
            self.build_bigint_sub_abs(remainder, best_current, "bigint_div_next_remainder");
        let next_quotient =
            self.build_bigint_add_abs(quotient, best_multiple, "bigint_div_next_quotient");
        self.builder
            .build_unconditional_branch(outer_loop)
            .expect("failed bigint_div outer continue");
        let inner_done_end = self.builder.get_insert_block().unwrap();
        quotient_phi.add_incoming(&[(&next_quotient, inner_done_end)]);
        remainder_phi.add_incoming(&[(&next_remainder, inner_done_end)]);

        self.builder.position_at_end(outer_done);
        let raw_quotient = quotient_phi.as_basic_value().into_int_value();
        let same_sign = self
            .builder
            .build_int_compare(IntPredicate::EQ, lhs_sign, rhs_sign, "bigint_div_same_sign")
            .expect("failed bigint_div same sign compare");
        let out_sign = self
            .builder
            .build_select(
                same_sign,
                self.i64_type.const_int(1, true),
                self.i64_type.const_int((-1i64) as u64, true),
                "bigint_div_out_sign",
            )
            .expect("failed bigint_div sign select")
            .into_int_value();
        self.build_bigint_sign_store(raw_quotient, out_sign, "bigint_div_sign");
        self.build_bigint_normalize(raw_quotient, "bigint_div_norm");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                raw_quotient,
                "bigint_div_result",
            )))
            .expect("failed bigint_div return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_bigint_modulo(&mut self, name: &str, symbol: &str) {
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
        self.functions.insert("bigint_modulo".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
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

        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_mod_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);

        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_mod_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);

        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_mod_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_mod_rhs");

        let rhs_is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rhs_sign,
                self.i64_type.const_zero(),
                "bigint_mod_rhs_zero",
            )
            .expect("failed bigint_mod rhs zero compare");
        let lhs_is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                self.i64_type.const_zero(),
                "bigint_mod_lhs_zero",
            )
            .expect("failed bigint_mod lhs zero compare");

        let zero_block = self.context.append_basic_block(function, "zero");
        let work_block = self.context.append_basic_block(function, "work");
        let init_block = self.context.append_basic_block(function, "init");
        self.builder
            .build_conditional_branch(rhs_is_zero, trap_block, work_block)
            .expect("failed bigint_mod rhs zero branch");

        self.builder.position_at_end(work_block);
        let outer_loop = self.context.append_basic_block(function, "outer_loop");
        let outer_body = self.context.append_basic_block(function, "outer_body");
        let outer_done = self.context.append_basic_block(function, "outer_done");
        self.builder
            .build_conditional_branch(lhs_is_zero, zero_block, init_block)
            .expect("failed bigint_mod lhs zero branch");

        self.builder.position_at_end(zero_block);
        let zero_ptr = self.build_bigint_zero("bigint_mod_zero");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                zero_ptr,
                "bigint_mod_zero_result",
            )))
            .expect("failed bigint_mod zero return");

        self.builder.position_at_end(init_block);
        self.builder
            .build_unconditional_branch(outer_loop)
            .expect("failed bigint_mod init jump");
        let init_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(outer_loop);
        let remainder_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_mod_remainder")
            .expect("failed bigint_mod remainder phi");
        remainder_phi.add_incoming(&[(&lhs_ptr, init_end)]);
        let remainder = remainder_phi.as_basic_value().into_int_value();
        let cmp = self.build_bigint_cmp_abs(remainder, rhs_ptr, "bigint_mod_outer_cmp");
        let has_more = self
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                cmp,
                self.i64_type.const_zero(),
                "bigint_mod_has_more",
            )
            .expect("failed bigint_mod outer cmp check");
        self.builder
            .build_conditional_branch(has_more, outer_body, outer_done)
            .expect("failed bigint_mod outer branch");

        self.builder.position_at_end(outer_body);
        let inner_loop = self.context.append_basic_block(function, "inner_loop");
        let inner_body = self.context.append_basic_block(function, "inner_body");
        let inner_done = self.context.append_basic_block(function, "inner_done");
        self.builder
            .build_unconditional_branch(inner_loop)
            .expect("failed bigint_mod inner jump");
        let inner_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(inner_loop);
        let current_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_mod_current")
            .expect("failed bigint_mod current phi");
        current_phi.add_incoming(&[(&rhs_ptr, inner_entry_end)]);
        let current = current_phi.as_basic_value().into_int_value();
        let doubled = self.build_bigint_add_abs(current, current, "bigint_mod_doubled");
        let doubled_cmp = self.build_bigint_cmp_abs(doubled, remainder, "bigint_mod_doubled_cmp");
        let can_double = self
            .builder
            .build_int_compare(
                IntPredicate::SLE,
                doubled_cmp,
                self.i64_type.const_zero(),
                "bigint_mod_can_double",
            )
            .expect("failed bigint_mod can_double");
        self.builder
            .build_conditional_branch(can_double, inner_body, inner_done)
            .expect("failed bigint_mod inner branch");

        self.builder.position_at_end(inner_body);
        self.builder
            .build_unconditional_branch(inner_loop)
            .expect("failed bigint_mod inner loop");
        let inner_body_end = self.builder.get_insert_block().unwrap();
        current_phi.add_incoming(&[(&doubled, inner_body_end)]);

        self.builder.position_at_end(inner_done);
        let best_current = current_phi.as_basic_value().into_int_value();
        let next_remainder =
            self.build_bigint_sub_abs(remainder, best_current, "bigint_mod_next_remainder");
        self.builder
            .build_unconditional_branch(outer_loop)
            .expect("failed bigint_mod outer continue");
        let inner_done_end = self.builder.get_insert_block().unwrap();
        remainder_phi.add_incoming(&[(&next_remainder, inner_done_end)]);

        self.builder.position_at_end(outer_done);
        let raw_remainder = remainder_phi.as_basic_value().into_int_value();
        self.build_bigint_sign_store(raw_remainder, lhs_sign, "bigint_mod_sign");
        self.build_bigint_normalize(raw_remainder, "bigint_mod_norm");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                raw_remainder,
                "bigint_mod_result",
            )))
            .expect("failed bigint_mod return");

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
            self.pair_type()
                .fn_type(&vec![self.i64_type.into(); arg_count * 2], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let mut args = Vec::with_capacity(arg_count);
        for index in 0..arg_count {
            args.push(CompiledValue {
                tag: function
                    .get_nth_param((index * 2) as u32)
                    .unwrap()
                    .into_int_value(),
                payload: function
                    .get_nth_param((index * 2 + 1) as u32)
                    .unwrap()
                    .into_int_value(),
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
            self.pair_type()
                .fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
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

    #[cfg(feature = "wasi")]
    fn define_wasi_preview1_print_runtime(&mut self) {
        let ptr_type = self.context.ptr_type(Default::default());
        let void_type = self.context.void_type();

        let write_bytes = self.module.add_function(
            "llvm_wasi_write_bytes",
            void_type.fn_type(&[ptr_type.into(), self.context.i32_type().into()], false),
            Some(Linkage::Private),
        );
        self.functions
            .insert("__wasi_write_bytes".to_string(), write_bytes);

        let write_i64 = self.module.add_function(
            "llvm_wasi_write_i64",
            void_type.fn_type(&[self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions
            .insert("__wasi_write_i64".to_string(), write_i64);

        let write_value = self.module.add_function(
            "llvm_wasi_write_value",
            void_type.fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions
            .insert("__wasi_write_value".to_string(), write_value);

        let write_bigint = self.module.add_function(
            "llvm_wasi_write_bigint",
            void_type.fn_type(&[self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions
            .insert("__wasi_write_bigint".to_string(), write_bigint);

        let write_list = self.module.add_function(
            "llvm_wasi_write_list",
            void_type.fn_type(&[self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions
            .insert("__wasi_write_list".to_string(), write_list);

        self.define_wasi_write_bytes_body(write_bytes);
        self.define_wasi_write_i64_body(write_i64);
        self.define_wasi_write_value_body(write_value);
        self.define_wasi_write_bigint_body(write_bigint);
        self.define_wasi_write_list_body(write_list);
        self.define_wasi_preview1_pair_print_wrapper("__rt_print", "llvm_rt_print");
        self.define_wasi_preview1_pair_print_wrapper("__rt_list_print", "llvm_rt_list_print");
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
            .build_pointer_cast(
                dst,
                self.context.ptr_type(Default::default()),
                "multi3_i64_ptr",
            )
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
        self.builder
            .build_store(lo_ptr, lo)
            .expect("failed to store multi3 lo");
        self.builder
            .build_store(hi_ptr, high)
            .expect("failed to store multi3 hi");
        self.builder
            .build_return(None)
            .expect("failed to return from multi3");
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

        let p0 = self
            .builder
            .build_int_mul(a0, b0, &format!("{label}_p0"))
            .expect("failed to mul p0");
        let p1 = self
            .builder
            .build_int_mul(a0, b1, &format!("{label}_p1"))
            .expect("failed to mul p1");
        let p2 = self
            .builder
            .build_int_mul(a1, b0, &format!("{label}_p2"))
            .expect("failed to mul p2");
        let p3 = self
            .builder
            .build_int_mul(a1, b1, &format!("{label}_p3"))
            .expect("failed to mul p3");

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

    #[cfg(feature = "wasi")]
    fn define_wasi_preview1_pair_print_wrapper(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type()
                .fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let tag = function.get_first_param().unwrap().into_int_value();
        let payload = function.get_nth_param(1).unwrap().into_int_value();
        let write_value = self.require_func("__wasi_write_value");
        self.builder
            .build_call(
                write_value,
                &[tag.into(), payload.into()],
                "wasi_write_value",
            )
            .expect("failed to call preview1 write_value");
        self.build_wasi_write_const("__wasi_newline", b"\n", "preview1_newline");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                self.i64_type.const_zero(),
                &format!("{symbol}_result"),
            )))
            .expect("failed to return preview1 print wrapper");
    }

    #[cfg(feature = "wasi")]
    fn define_wasi_write_bytes_body(&self, function: FunctionValue<'ctx>) {
        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let ptr = function.get_first_param().unwrap().into_pointer_value();
        let len = function.get_nth_param(1).unwrap().into_int_value();
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(Default::default());
        let iovec_type = self
            .context
            .struct_type(&[ptr_type.into(), i32_type.into()], false);
        let iovec_ptr = self
            .builder
            .build_alloca(iovec_type, "wasi_iovec")
            .expect("failed to allocate wasi iovec");
        let nwritten_ptr = self
            .builder
            .build_alloca(i32_type, "wasi_nwritten")
            .expect("failed to allocate wasi nwritten");
        let buf_ptr = self
            .builder
            .build_struct_gep(iovec_type, iovec_ptr, 0, "wasi_iovec_buf")
            .expect("failed to build iovec buf gep");
        let len_ptr = self
            .builder
            .build_struct_gep(iovec_type, iovec_ptr, 1, "wasi_iovec_len")
            .expect("failed to build iovec len gep");
        self.builder
            .build_store(buf_ptr, ptr)
            .expect("failed to store iovec buf");
        self.builder
            .build_store(len_ptr, len)
            .expect("failed to store iovec len");

        let fd_write = self.require_func("__wasi_fd_write");
        let iovec_raw = self
            .builder
            .build_ptr_to_int(iovec_ptr, i32_type, "wasi_iovec_raw")
            .expect("failed to convert iovec ptr");
        let nwritten_raw = self
            .builder
            .build_ptr_to_int(nwritten_ptr, i32_type, "wasi_nwritten_raw")
            .expect("failed to convert nwritten ptr");
        let status = self
            .builder
            .build_call(
                fd_write,
                &[
                    i32_type.const_int(1, false).into(),
                    iovec_raw.into(),
                    i32_type.const_int(1, false).into(),
                    nwritten_raw.into(),
                ],
                "wasi_fd_write",
            )
            .expect("failed to call fd_write")
            .try_as_basic_value()
            .unwrap_basic()
            .into_int_value();
        let success = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                i32_type.const_zero(),
                "wasi_fd_write_ok",
            )
            .expect("failed to compare fd_write status");
        self.builder
            .build_conditional_branch(success, ok_block, trap_block)
            .expect("failed to branch on fd_write status");

        self.builder.position_at_end(ok_block);
        self.builder
            .build_return(None)
            .expect("failed to return from write_bytes");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    #[cfg(feature = "wasi")]
    fn define_wasi_write_i64_body(&self, function: FunctionValue<'ctx>) {
        let entry = self.context.append_basic_block(function, "entry");
        let zero_block = self.context.append_basic_block(function, "zero");
        let non_zero_block = self.context.append_basic_block(function, "non_zero");
        let sign_check_block = self.context.append_basic_block(function, "sign_check");
        let loop_block = self.context.append_basic_block(function, "loop");
        let done_block = self.context.append_basic_block(function, "done");
        self.builder.position_at_end(entry);

        let value = function.get_first_param().unwrap().into_int_value();
        let zero = self.i64_type.const_zero();
        let is_zero = self
            .builder
            .build_int_compare(IntPredicate::EQ, value, zero, "wasi_i64_is_zero")
            .expect("failed to compare i64 zero");
        self.builder
            .build_conditional_branch(is_zero, zero_block, non_zero_block)
            .expect("failed to branch on i64 zero");

        self.builder.position_at_end(zero_block);
        self.build_wasi_write_const("__wasi_digit_zero", b"0", "wasi_zero");
        self.builder
            .build_return(None)
            .expect("failed to return from zero i64 writer");

        self.builder.position_at_end(non_zero_block);
        let buffer_type = self.context.i8_type().array_type(32);
        let buffer = self
            .builder
            .build_alloca(buffer_type, "wasi_i64_buffer")
            .expect("failed to allocate i64 buffer");
        let idx_ptr = self
            .builder
            .build_alloca(self.context.i32_type(), "wasi_i64_idx")
            .expect("failed to allocate i64 idx");
        let current_ptr = self
            .builder
            .build_alloca(self.i64_type, "wasi_i64_current")
            .expect("failed to allocate i64 current");
        self.builder
            .build_store(idx_ptr, self.context.i32_type().const_int(32, false))
            .expect("failed to store initial i64 idx");
        self.builder
            .build_unconditional_branch(sign_check_block)
            .expect("failed to branch to sign check");

        self.builder.position_at_end(sign_check_block);
        let is_negative = self
            .builder
            .build_int_compare(IntPredicate::SLT, value, zero, "wasi_i64_is_negative")
            .expect("failed to compare i64 sign");
        let neg_block = self.context.append_basic_block(function, "negative");
        let pos_block = self.context.append_basic_block(function, "positive");
        self.builder
            .build_conditional_branch(is_negative, neg_block, pos_block)
            .expect("failed to branch on i64 sign");

        self.builder.position_at_end(neg_block);
        self.build_wasi_write_const("__wasi_minus", b"-", "wasi_minus");
        let magnitude = self
            .builder
            .build_int_sub(zero, value, "wasi_i64_magnitude")
            .expect("failed to compute i64 magnitude");
        self.builder
            .build_store(current_ptr, magnitude)
            .expect("failed to store negative magnitude");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to branch to i64 loop");

        self.builder.position_at_end(pos_block);
        self.builder
            .build_store(current_ptr, value)
            .expect("failed to store positive i64");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to branch to i64 loop");

        self.builder.position_at_end(loop_block);
        let current = self
            .builder
            .build_load(self.i64_type, current_ptr, "wasi_i64_current_load")
            .expect("failed to load current i64")
            .into_int_value();
        let quotient = self
            .builder
            .build_int_unsigned_div(
                current,
                self.i64_type.const_int(10, false),
                "wasi_i64_quotient",
            )
            .expect("failed to divide current i64");
        let remainder = self
            .builder
            .build_int_unsigned_rem(
                current,
                self.i64_type.const_int(10, false),
                "wasi_i64_remainder",
            )
            .expect("failed to mod current i64");
        let idx = self
            .builder
            .build_load(self.context.i32_type(), idx_ptr, "wasi_i64_idx_load")
            .expect("failed to load i64 idx")
            .into_int_value();
        let next_idx = self
            .builder
            .build_int_sub(
                idx,
                self.context.i32_type().const_int(1, false),
                "wasi_i64_next_idx",
            )
            .expect("failed to decrement i64 idx");
        self.builder
            .build_store(idx_ptr, next_idx)
            .expect("failed to store next i64 idx");
        let digit = self
            .builder
            .build_int_add(
                self.builder
                    .build_int_truncate(remainder, self.context.i8_type(), "wasi_i64_digit_raw")
                    .expect("failed to truncate digit"),
                self.context.i8_type().const_int(b'0' as u64, false),
                "wasi_i64_digit",
            )
            .expect("failed to build digit");
        let zero32 = self.context.i32_type().const_zero();
        let digit_ptr = unsafe {
            self.builder
                .build_gep(
                    buffer_type,
                    buffer,
                    &[zero32, next_idx],
                    "wasi_i64_digit_ptr",
                )
                .expect("failed to build digit ptr")
        };
        self.builder
            .build_store(digit_ptr, digit)
            .expect("failed to store digit");
        self.builder
            .build_store(current_ptr, quotient)
            .expect("failed to store quotient");
        let more = self
            .builder
            .build_int_compare(IntPredicate::NE, quotient, zero, "wasi_i64_more_digits")
            .expect("failed to compare quotient");
        self.builder
            .build_conditional_branch(more, loop_block, done_block)
            .expect("failed to branch in i64 loop");

        self.builder.position_at_end(done_block);
        let final_idx = self
            .builder
            .build_load(self.context.i32_type(), idx_ptr, "wasi_i64_final_idx")
            .expect("failed to load final i64 idx")
            .into_int_value();
        let start_ptr = unsafe {
            self.builder
                .build_gep(
                    buffer_type,
                    buffer,
                    &[zero32, final_idx],
                    "wasi_i64_start_ptr",
                )
                .expect("failed to build start ptr")
        };
        let len = self
            .builder
            .build_int_sub(
                self.context.i32_type().const_int(32, false),
                final_idx,
                "wasi_i64_len",
            )
            .expect("failed to compute i64 len");
        let write_bytes = self.require_func("__wasi_write_bytes");
        self.builder
            .build_call(
                write_bytes,
                &[start_ptr.into(), len.into()],
                "wasi_write_digits",
            )
            .expect("failed to write digit buffer");
        self.builder
            .build_return(None)
            .expect("failed to return from i64 writer");
    }

    #[cfg(feature = "wasi")]
    fn define_wasi_write_value_body(&self, function: FunctionValue<'ctx>) {
        let entry = self.context.append_basic_block(function, "entry");
        let int_block = self.context.append_basic_block(function, "int");
        let list_block = self.context.append_basic_block(function, "list");
        let bigint_block = self.context.append_basic_block(function, "bigint");
        let string_block = self.context.append_basic_block(function, "string");
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
                "wasi_value_is_int",
            )
            .expect("failed to compare value tag int");
        let tag_dispatch = self.context.append_basic_block(function, "dispatch");
        self.builder
            .build_conditional_branch(is_int, int_block, tag_dispatch)
            .expect("failed to branch on int tag");

        self.builder.position_at_end(tag_dispatch);
        let is_list = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                self.i64_type.const_int(TAG_LIST as u64, false),
                "wasi_value_is_list",
            )
            .expect("failed to compare value tag list");
        let bigint_tag = self.i64_type.const_int(TAG_BIGINT as u64, false);
        let is_bigint = self
            .builder
            .build_int_compare(IntPredicate::EQ, tag, bigint_tag, "wasi_value_is_bigint")
            .expect("failed to compare value tag bigint");
        let string_tag = self.i64_type.const_int(3, false);
        let is_string = self
            .builder
            .build_int_compare(IntPredicate::EQ, tag, string_tag, "wasi_value_is_string")
            .expect("failed to compare value tag string");
        let after_list = self.context.append_basic_block(function, "after_list");
        self.builder
            .build_conditional_branch(is_list, list_block, after_list)
            .expect("failed to branch on list tag");

        self.builder.position_at_end(after_list);
        let after_bigint = self.context.append_basic_block(function, "after_bigint");
        self.builder
            .build_conditional_branch(is_bigint, bigint_block, after_bigint)
            .expect("failed to branch on bigint tag");

        self.builder.position_at_end(after_bigint);
        self.builder
            .build_conditional_branch(is_string, string_block, trap_block)
            .expect("failed to branch on string tag");

        self.builder.position_at_end(int_block);
        let write_i64 = self.require_func("__wasi_write_i64");
        self.builder
            .build_call(write_i64, &[payload.into()], "wasi_write_i64")
            .expect("failed to call write_i64");
        self.builder
            .build_return(None)
            .expect("failed to return from value int writer");

        self.builder.position_at_end(list_block);
        let write_list = self.require_func("__wasi_write_list");
        self.builder
            .build_call(write_list, &[payload.into()], "wasi_write_list")
            .expect("failed to call write_list");
        self.builder
            .build_return(None)
            .expect("failed to return from value list writer");

        self.builder.position_at_end(bigint_block);
        let write_bigint = self.require_func("__wasi_write_bigint");
        self.builder
            .build_call(write_bigint, &[payload.into()], "wasi_write_bigint")
            .expect("failed to call write_bigint");
        self.builder
            .build_return(None)
            .expect("failed to return from value bigint writer");

        self.builder.position_at_end(string_block);
        self.build_wasi_write_const("__wasi_string_placeholder", b"<string>", "wasi_string");
        self.builder
            .build_return(None)
            .expect("failed to return from value string writer");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    #[cfg(feature = "wasi")]
    fn define_wasi_write_list_body(&self, function: FunctionValue<'ctx>) {
        let entry = self.context.append_basic_block(function, "entry");
        let loop_check = self.context.append_basic_block(function, "loop_check");
        let loop_body = self.context.append_basic_block(function, "loop_body");
        let separator_block = self.context.append_basic_block(function, "separator");
        let element_block = self.context.append_basic_block(function, "element");
        let done_block = self.context.append_basic_block(function, "done");
        self.builder.position_at_end(entry);

        let payload = function.get_first_param().unwrap().into_int_value();
        self.build_wasi_write_const("__wasi_list_open", b"[", "wasi_list_open");
        let len = self.build_list_len_load(payload, "wasi_list_len");
        let idx_ptr = self
            .builder
            .build_alloca(self.i64_type, "wasi_list_idx")
            .expect("failed to allocate list idx");
        self.builder
            .build_store(idx_ptr, self.i64_type.const_zero())
            .expect("failed to init list idx");
        self.builder
            .build_unconditional_branch(loop_check)
            .expect("failed to branch to list loop");

        self.builder.position_at_end(loop_check);
        let idx = self
            .builder
            .build_load(self.i64_type, idx_ptr, "wasi_list_idx_load")
            .expect("failed to load list idx")
            .into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, "wasi_list_more")
            .expect("failed to compare list idx");
        self.builder
            .build_conditional_branch(more, loop_body, done_block)
            .expect("failed to branch on list idx");

        self.builder.position_at_end(loop_body);
        let is_first = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                idx,
                self.i64_type.const_zero(),
                "wasi_list_is_first",
            )
            .expect("failed to compare first list element");
        self.builder
            .build_conditional_branch(is_first, element_block, separator_block)
            .expect("failed to branch on first list element");

        self.builder.position_at_end(separator_block);
        self.build_wasi_write_const("__wasi_list_separator", b", ", "wasi_list_sep");
        self.builder
            .build_unconditional_branch(element_block)
            .expect("failed to branch to list element");

        self.builder.position_at_end(element_block);
        let value = self.build_list_value_load(payload, idx, "wasi_list_value");
        let write_value = self.require_func("__wasi_write_value");
        self.builder
            .build_call(
                write_value,
                &[value.tag.into(), value.payload.into()],
                "wasi_list_write_value",
            )
            .expect("failed to write list element");
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "wasi_list_next")
            .expect("failed to increment list idx");
        self.builder
            .build_store(idx_ptr, next)
            .expect("failed to store next list idx");
        self.builder
            .build_unconditional_branch(loop_check)
            .expect("failed to loop over list");

        self.builder.position_at_end(done_block);
        self.build_wasi_write_const("__wasi_list_close", b"]", "wasi_list_close");
        self.builder
            .build_return(None)
            .expect("failed to return from list writer");
    }

    #[cfg(feature = "wasi")]
    fn define_wasi_write_bigint_body(&self, function: FunctionValue<'ctx>) {
        let entry = self.context.append_basic_block(function, "entry");
        let zero_block = self.context.append_basic_block(function, "zero");
        let non_zero_block = self.context.append_basic_block(function, "non_zero");
        let sign_block = self.context.append_basic_block(function, "sign");
        let digit_loop_check = self
            .context
            .append_basic_block(function, "digit_loop_check");
        let digit_loop_body = self.context.append_basic_block(function, "digit_loop_body");
        let digit_loop_done = self.context.append_basic_block(function, "digit_loop_done");
        let limb_loop_check = self.context.append_basic_block(function, "limb_loop_check");
        let limb_loop_body = self.context.append_basic_block(function, "limb_loop_body");
        let limb_loop_done = self.context.append_basic_block(function, "limb_loop_done");
        let write_block = self.context.append_basic_block(function, "write");
        self.builder.position_at_end(entry);

        let payload = function.get_first_param().unwrap().into_int_value();
        let sign = self.build_bigint_sign_load(payload, "wasi_bigint_sign");
        let len = self.build_bigint_len_load(payload, "wasi_bigint_len");
        let is_zero_sign = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sign,
                self.i64_type.const_zero(),
                "wasi_bigint_sign_zero",
            )
            .expect("failed to compare bigint sign to zero");
        let is_zero_len = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                len,
                self.i64_type.const_zero(),
                "wasi_bigint_len_zero",
            )
            .expect("failed to compare bigint len to zero");
        let is_zero = self
            .builder
            .build_or(is_zero_sign, is_zero_len, "wasi_bigint_is_zero")
            .expect("failed to combine bigint zero checks");
        self.builder
            .build_conditional_branch(is_zero, zero_block, non_zero_block)
            .expect("failed to branch on bigint zero");

        self.builder.position_at_end(zero_block);
        self.build_wasi_write_const("__wasi_bigint_zero", b"0", "wasi_bigint_zero");
        self.builder
            .build_return(None)
            .expect("failed to return from zero bigint writer");

        self.builder.position_at_end(non_zero_block);
        let alloc = self.require_func("__alloc");
        let temp_payload = self.build_bigint_alloc(len, "wasi_bigint_temp");
        self.build_bigint_sign_store(temp_payload, sign, "wasi_bigint_temp_sign");
        self.build_bigint_len_store(temp_payload, len, "wasi_bigint_temp_len");

        let copy_idx_ptr = self
            .builder
            .build_alloca(self.i64_type, "wasi_bigint_copy_idx")
            .expect("failed to allocate bigint copy idx");
        let rem_ptr = self
            .builder
            .build_alloca(self.i64_type, "wasi_bigint_rem")
            .expect("failed to allocate bigint rem");
        let digit_cap = self
            .builder
            .build_int_add(
                self.builder
                    .build_int_mul(
                        len,
                        self.i64_type.const_int(10, false),
                        "wasi_bigint_digit_mul",
                    )
                    .expect("failed to compute bigint digit cap mul"),
                self.i64_type.const_int(1, false),
                "wasi_bigint_digit_cap",
            )
            .expect("failed to compute bigint digit cap");
        let digit_buf_raw = self.build_boxed_call(
            alloc,
            &[digit_cap, self.i64_type.const_int(1, false)],
            "wasi_bigint_digit_buf",
        );
        let digit_pos_ptr = self
            .builder
            .build_alloca(self.i64_type, "wasi_bigint_digit_pos")
            .expect("failed to allocate bigint digit pos");
        self.builder
            .build_store(copy_idx_ptr, self.i64_type.const_zero())
            .expect("failed to init bigint copy idx");
        self.builder
            .build_store(digit_pos_ptr, digit_cap)
            .expect("failed to init bigint digit pos");

        let copy_loop_check = self.context.append_basic_block(function, "copy_loop_check");
        let copy_loop_body = self.context.append_basic_block(function, "copy_loop_body");
        let copy_loop_done = self.context.append_basic_block(function, "copy_loop_done");
        self.builder
            .build_unconditional_branch(copy_loop_check)
            .expect("failed to branch to bigint copy loop");

        self.builder.position_at_end(copy_loop_check);
        let copy_idx = self
            .builder
            .build_load(self.i64_type, copy_idx_ptr, "wasi_bigint_copy_idx_load")
            .expect("failed to load bigint copy idx")
            .into_int_value();
        let copy_more = self
            .builder
            .build_int_compare(IntPredicate::ULT, copy_idx, len, "wasi_bigint_copy_more")
            .expect("failed to compare bigint copy idx");
        self.builder
            .build_conditional_branch(copy_more, copy_loop_body, copy_loop_done)
            .expect("failed to branch bigint copy loop");

        self.builder.position_at_end(copy_loop_body);
        let copied_limb = self.build_bigint_limb_load(payload, copy_idx, "wasi_bigint_copy_src");
        self.build_bigint_limb_store(temp_payload, copy_idx, copied_limb, "wasi_bigint_copy_dst");
        let copy_next = self
            .builder
            .build_int_add(
                copy_idx,
                self.i64_type.const_int(1, false),
                "wasi_bigint_copy_next",
            )
            .expect("failed to increment bigint copy idx");
        self.builder
            .build_store(copy_idx_ptr, copy_next)
            .expect("failed to store bigint copy next");
        self.builder
            .build_unconditional_branch(copy_loop_check)
            .expect("failed to loop bigint copy");

        self.builder.position_at_end(copy_loop_done);
        self.builder
            .build_unconditional_branch(sign_block)
            .expect("failed to branch to bigint sign block");

        self.builder.position_at_end(sign_block);
        let is_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                sign,
                self.i64_type.const_zero(),
                "wasi_bigint_is_negative",
            )
            .expect("failed to compare bigint sign");
        let neg_block = self.context.append_basic_block(function, "negative");
        let pos_block = self.context.append_basic_block(function, "positive");
        self.builder
            .build_conditional_branch(is_negative, neg_block, pos_block)
            .expect("failed to branch on bigint sign");

        self.builder.position_at_end(neg_block);
        self.build_wasi_write_const("__wasi_minus", b"-", "wasi_bigint_minus");
        self.builder
            .build_unconditional_branch(pos_block)
            .expect("failed to branch after bigint minus");

        self.builder.position_at_end(pos_block);
        self.builder
            .build_unconditional_branch(digit_loop_check)
            .expect("failed to branch to bigint digit loop");

        self.builder.position_at_end(digit_loop_check);
        let temp_len = self.build_bigint_len_load(temp_payload, "wasi_bigint_temp_len");
        let has_digits = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                temp_len,
                self.i64_type.const_zero(),
                "wasi_bigint_has_digits",
            )
            .expect("failed to compare bigint temp len");
        self.builder
            .build_conditional_branch(has_digits, digit_loop_body, digit_loop_done)
            .expect("failed to branch bigint digit loop");

        self.builder.position_at_end(digit_loop_body);
        self.builder
            .build_store(rem_ptr, self.i64_type.const_zero())
            .expect("failed to reset bigint remainder");
        self.builder
            .build_store(copy_idx_ptr, temp_len)
            .expect("failed to init bigint limb loop idx");
        self.builder
            .build_unconditional_branch(limb_loop_check)
            .expect("failed to branch to bigint limb loop");

        self.builder.position_at_end(limb_loop_check);
        let limb_remaining = self
            .builder
            .build_load(self.i64_type, copy_idx_ptr, "wasi_bigint_limb_remaining")
            .expect("failed to load bigint limb remaining")
            .into_int_value();
        let limb_more = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                limb_remaining,
                self.i64_type.const_zero(),
                "wasi_bigint_limb_more",
            )
            .expect("failed to compare bigint limb remaining");
        self.builder
            .build_conditional_branch(limb_more, limb_loop_body, limb_loop_done)
            .expect("failed to branch bigint limb loop");

        self.builder.position_at_end(limb_loop_body);
        let limb_idx = self
            .builder
            .build_int_sub(
                limb_remaining,
                self.i64_type.const_int(1, false),
                "wasi_bigint_limb_idx",
            )
            .expect("failed to decrement bigint limb idx");
        let limb = self.build_bigint_limb_load(temp_payload, limb_idx, "wasi_bigint_div_limb");
        let remainder = self
            .builder
            .build_load(self.i64_type, rem_ptr, "wasi_bigint_rem_load")
            .expect("failed to load bigint remainder")
            .into_int_value();
        let high = self
            .builder
            .build_left_shift(
                remainder,
                self.i64_type.const_int(32, false),
                "wasi_bigint_cur_high",
            )
            .expect("failed to shift bigint remainder");
        let current = self
            .builder
            .build_int_add(high, limb, "wasi_bigint_cur")
            .expect("failed to build bigint current");
        let quotient = self
            .builder
            .build_int_unsigned_div(
                current,
                self.i64_type.const_int(10, false),
                "wasi_bigint_quotient",
            )
            .expect("failed to divide bigint current");
        let next_remainder = self
            .builder
            .build_int_unsigned_rem(
                current,
                self.i64_type.const_int(10, false),
                "wasi_bigint_remainder",
            )
            .expect("failed to mod bigint current");
        self.build_bigint_limb_store(temp_payload, limb_idx, quotient, "wasi_bigint_quot_store");
        self.builder
            .build_store(rem_ptr, next_remainder)
            .expect("failed to store bigint remainder");
        self.builder
            .build_store(copy_idx_ptr, limb_idx)
            .expect("failed to store next bigint limb idx");
        self.builder
            .build_unconditional_branch(limb_loop_check)
            .expect("failed to loop bigint limb division");

        self.builder.position_at_end(limb_loop_done);
        self.build_bigint_normalize(temp_payload, "wasi_bigint_norm");
        let digit_pos = self
            .builder
            .build_load(self.i64_type, digit_pos_ptr, "wasi_bigint_digit_pos_load")
            .expect("failed to load bigint digit pos")
            .into_int_value();
        let next_digit_pos = self
            .builder
            .build_int_sub(
                digit_pos,
                self.i64_type.const_int(1, false),
                "wasi_bigint_next_digit_pos",
            )
            .expect("failed to decrement bigint digit pos");
        self.builder
            .build_store(digit_pos_ptr, next_digit_pos)
            .expect("failed to store bigint next digit pos");
        let digit_addr = self
            .builder
            .build_int_add(digit_buf_raw, next_digit_pos, "wasi_bigint_digit_addr")
            .expect("failed to compute bigint digit addr");
        let digit_ptr = self
            .builder
            .build_int_to_ptr(
                digit_addr,
                self.context.ptr_type(Default::default()),
                "wasi_bigint_digit_ptr",
            )
            .expect("failed to convert bigint digit ptr");
        let digit = self
            .builder
            .build_int_add(
                self.builder
                    .build_int_truncate(
                        self.builder
                            .build_load(self.i64_type, rem_ptr, "wasi_bigint_digit_rem")
                            .expect("failed to reload bigint remainder")
                            .into_int_value(),
                        self.context.i8_type(),
                        "wasi_bigint_digit_raw",
                    )
                    .expect("failed to truncate bigint digit"),
                self.context.i8_type().const_int(b'0' as u64, false),
                "wasi_bigint_digit",
            )
            .expect("failed to build bigint digit");
        self.builder
            .build_store(digit_ptr, digit)
            .expect("failed to store bigint digit");
        self.builder
            .build_unconditional_branch(digit_loop_check)
            .expect("failed to loop bigint digits");

        self.builder.position_at_end(digit_loop_done);
        self.builder
            .build_unconditional_branch(write_block)
            .expect("failed to branch to bigint write");

        self.builder.position_at_end(write_block);
        let final_digit_pos = self
            .builder
            .build_load(self.i64_type, digit_pos_ptr, "wasi_bigint_final_digit_pos")
            .expect("failed to load final bigint digit pos")
            .into_int_value();
        let write_addr = self
            .builder
            .build_int_add(digit_buf_raw, final_digit_pos, "wasi_bigint_write_addr")
            .expect("failed to compute bigint write addr");
        let write_ptr = self
            .builder
            .build_int_to_ptr(
                write_addr,
                self.context.ptr_type(Default::default()),
                "wasi_bigint_write_ptr",
            )
            .expect("failed to convert bigint write ptr");
        let remaining_len = self
            .builder
            .build_int_sub(digit_cap, final_digit_pos, "wasi_bigint_write_len")
            .expect("failed to compute bigint write len");
        let write_bytes = self.require_func("__wasi_write_bytes");
        let write_len_i32 = self
            .builder
            .build_int_truncate(
                remaining_len,
                self.context.i32_type(),
                "wasi_bigint_write_len_i32",
            )
            .expect("failed to truncate bigint write len");
        self.builder
            .build_call(
                write_bytes,
                &[write_ptr.into(), write_len_i32.into()],
                "wasi_bigint_write_bytes",
            )
            .expect("failed to write bigint bytes");
        self.builder
            .build_return(None)
            .expect("failed to return from bigint writer");
    }

    fn define_pair_list_len(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type()
                .fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let list = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let list_payload =
            self.expect_tag_payload(list, TAG_LIST, "list_len", ok_block, trap_block);

        self.builder.position_at_end(ok_block);
        let len = self.build_list_len_load(list_payload, "list_len");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                len,
                "list_len_result",
            )))
            .expect("failed to return list len");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_list_new(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(&[], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let align = self.i64_type.const_int(8, false);
        let cap = self.i64_type.const_int(1024, false);
        let bytes = self.i64_type.const_int(1024 * 16, false);
        let alloc = self.require_func("__alloc");
        let data_ptr_raw = self.build_boxed_call(alloc, &[bytes, align], "list_new_data");
        let header_size = self.i64_type.const_int(24, false);
        let header_ptr_raw = self.build_boxed_call(alloc, &[header_size, align], "list_new_header");
        let header_ptr = self.build_list_header_ptr(header_ptr_raw, "list_new");
        let data_ptr = self
            .builder
            .build_int_to_ptr(
                data_ptr_raw,
                self.context.ptr_type(Default::default()),
                "list_new_data_ptr",
            )
            .expect("failed to convert data ptr");
        self.build_list_data_ptr_store(header_ptr, data_ptr, "list_new");
        self.build_list_len_store(header_ptr_raw, self.i64_type.const_zero(), "list_new");
        self.build_list_cap_store(header_ptr_raw, cap, "list_new");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_LIST as u64, false),
                header_ptr_raw,
                "list_new_result",
            )))
            .expect("failed to return list_new");
    }

    fn define_pair_list_push(&mut self, name: &str, symbol: &str) {
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
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        let grow_check_block = self.context.append_basic_block(function, "grow_check");
        let grow_block = self.context.append_basic_block(function, "grow");
        let copy_loop_block = self.context.append_basic_block(function, "copy_loop");
        let copy_body_block = self.context.append_basic_block(function, "copy_body");
        let store_block = self.context.append_basic_block(function, "store");
        self.builder.position_at_end(entry);

        let list = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let value = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let list_payload =
            self.expect_tag_payload(list, TAG_LIST, "list_push_list", ok_block, trap_block);

        self.builder.position_at_end(ok_block);
        self.builder
            .build_unconditional_branch(grow_check_block)
            .expect("failed to branch to list push check");

        self.builder.position_at_end(grow_check_block);
        let len = self.build_list_len_load(list_payload, "list_push");
        let cap = self.build_list_cap_load(list_payload, "list_push");
        let has_room = self
            .builder
            .build_int_compare(IntPredicate::ULT, len, cap, "list_push_has_room")
            .expect("failed to compare list push capacity");
        self.builder
            .build_conditional_branch(has_room, store_block, grow_block)
            .expect("failed to branch on list push capacity");

        self.builder.position_at_end(grow_block);
        let alloc = self.require_func("__alloc");
        let two = self.i64_type.const_int(2, false);
        let old_data_ptr = self.build_list_data_ptr_load(list_payload, "list_push_old_data");
        let new_cap = self
            .builder
            .build_int_mul(cap, two, "list_push_new_cap")
            .expect("failed to multiply list cap");
        let bytes = self
            .builder
            .build_int_mul(
                new_cap,
                self.i64_type.const_int(16, false),
                "list_push_bytes",
            )
            .expect("failed to build list push bytes");
        let align = self.i64_type.const_int(8, false);
        let new_data_raw = self.build_boxed_call(alloc, &[bytes, align], "list_push_new_data");
        let new_data_ptr = self
            .builder
            .build_int_to_ptr(
                new_data_raw,
                self.context.ptr_type(Default::default()),
                "list_push_new_data_ptr",
            )
            .expect("failed to convert new data ptr");
        let header_ptr = self.build_list_header_ptr(list_payload, "list_push_header");
        self.build_list_data_ptr_store(header_ptr, new_data_ptr, "list_push");
        self.build_list_cap_store(list_payload, new_cap, "list_push");
        self.builder
            .build_unconditional_branch(copy_loop_block)
            .expect("failed to branch to list push copy loop");

        self.builder.position_at_end(copy_loop_block);
        let copy_idx_phi = self
            .builder
            .build_phi(self.i64_type, "list_push_copy_idx")
            .expect("failed to build list push copy idx phi");
        copy_idx_phi.add_incoming(&[(&self.i64_type.const_zero(), grow_block)]);
        let copy_idx = copy_idx_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, copy_idx, len, "list_push_copy_more")
            .expect("failed to compare list push copy idx");
        self.builder
            .build_conditional_branch(more, copy_body_block, store_block)
            .expect("failed to branch list push copy loop");

        self.builder.position_at_end(copy_body_block);
        let moved =
            self.build_list_value_load_from_data_ptr(old_data_ptr, copy_idx, "list_push_old");
        self.build_list_value_store_from_data_ptr(new_data_ptr, copy_idx, moved, "list_push_new");
        let next = self
            .builder
            .build_int_add(
                copy_idx,
                self.i64_type.const_int(1, false),
                "list_push_copy_next",
            )
            .expect("failed to increment push copy idx");
        self.builder
            .build_unconditional_branch(copy_loop_block)
            .expect("failed to loop list push copy");
        copy_idx_phi.add_incoming(&[(&next, copy_body_block)]);

        self.builder.position_at_end(store_block);
        self.build_list_value_store(list_payload, len, value, "list_push_store");
        let new_len = self
            .builder
            .build_int_add(len, self.i64_type.const_int(1, false), "list_push_new_len")
            .expect("failed to increment list len");
        self.build_list_len_store(list_payload, new_len, "list_push");
        self.builder
            .build_return(Some(&self.make_pair_value(
                list.tag,
                list.payload,
                "list_push_result",
            )))
            .expect("failed to return list_push");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_list_get(&mut self, name: &str, symbol: &str) {
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
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let list = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let index = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let list_payload =
            self.expect_tag_payload(list, TAG_LIST, "list_get_list", ok_block, trap_block);

        self.builder.position_at_end(ok_block);
        let idx = self.expect_tag_int(index, "list_get_index", trap_block);
        self.build_index_bounds_check(list_payload, idx, "list_get", trap_block);
        let result = self.build_list_value_load(list_payload, idx, "list_get");
        self.builder
            .build_return(Some(&self.make_pair_value(
                result.tag,
                result.payload,
                "list_get_pair",
            )))
            .expect("failed to return list get");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_list_set(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(
                &[
                    self.i64_type.into(),
                    self.i64_type.into(),
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
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let list = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let index = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let value = CompiledValue {
            tag: function.get_nth_param(4).unwrap().into_int_value(),
            payload: function.get_nth_param(5).unwrap().into_int_value(),
        };
        let list_payload =
            self.expect_tag_payload(list, TAG_LIST, "list_set_list", ok_block, trap_block);

        self.builder.position_at_end(ok_block);
        let idx = self.expect_tag_int(index, "list_set_index", trap_block);
        self.build_index_bounds_check(list_payload, idx, "list_set", trap_block);
        self.build_list_value_store(list_payload, idx, value, "list_set");
        self.builder
            .build_return(Some(&self.make_pair_value(
                value.tag,
                value.payload,
                "list_set_result",
            )))
            .expect("failed to return list set");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_list_insert(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(
                &[
                    self.i64_type.into(),
                    self.i64_type.into(),
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
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        let idx_ok_block = self.context.append_basic_block(function, "idx_ok");
        let grow_check_block = self.context.append_basic_block(function, "grow_check");
        let grow_block = self.context.append_basic_block(function, "grow");
        let copy_loop_block = self.context.append_basic_block(function, "copy_loop");
        let copy_body_block = self.context.append_basic_block(function, "copy_body");
        let shift_loop_block = self.context.append_basic_block(function, "shift_loop");
        let shift_body_block = self.context.append_basic_block(function, "shift_body");
        let store_block = self.context.append_basic_block(function, "store");
        self.builder.position_at_end(entry);

        let list = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let index = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let value = CompiledValue {
            tag: function.get_nth_param(4).unwrap().into_int_value(),
            payload: function.get_nth_param(5).unwrap().into_int_value(),
        };
        let list_payload =
            self.expect_tag_payload(list, TAG_LIST, "list_insert_list", ok_block, trap_block);

        self.builder.position_at_end(ok_block);
        let idx = self.expect_tag_int(index, "list_insert_index", trap_block);
        let len = self.build_list_len_load(list_payload, "list_insert");
        let in_bounds = self
            .builder
            .build_int_compare(IntPredicate::ULE, idx, len, "list_insert_in_bounds")
            .expect("failed to compare insert bounds");
        self.builder
            .build_conditional_branch(in_bounds, idx_ok_block, trap_block)
            .expect("failed to branch on insert bounds");

        self.builder.position_at_end(idx_ok_block);
        self.builder
            .build_unconditional_branch(grow_check_block)
            .expect("failed to branch to insert grow check");

        self.builder.position_at_end(grow_check_block);
        let cap = self.build_list_cap_load(list_payload, "list_insert");
        let has_room = self
            .builder
            .build_int_compare(IntPredicate::ULT, len, cap, "list_insert_has_room")
            .expect("failed to compare insert capacity");
        self.builder
            .build_conditional_branch(has_room, shift_loop_block, grow_block)
            .expect("failed to branch on insert capacity");

        self.builder.position_at_end(grow_block);
        let alloc = self.require_func("__alloc");
        let old_data_ptr = self.build_list_data_ptr_load(list_payload, "list_insert_old_data");
        let new_cap = self
            .builder
            .build_int_mul(
                cap,
                self.i64_type.const_int(2, false),
                "list_insert_new_cap",
            )
            .expect("failed to multiply insert cap");
        let bytes = self
            .builder
            .build_int_mul(
                new_cap,
                self.i64_type.const_int(16, false),
                "list_insert_bytes",
            )
            .expect("failed to build insert bytes");
        let align = self.i64_type.const_int(8, false);
        let new_data_raw = self.build_boxed_call(alloc, &[bytes, align], "list_insert_new_data");
        let new_data_ptr = self
            .builder
            .build_int_to_ptr(
                new_data_raw,
                self.context.ptr_type(Default::default()),
                "list_insert_new_data_ptr",
            )
            .expect("failed to convert insert data ptr");
        let header_ptr = self.build_list_header_ptr(list_payload, "list_insert_header");
        self.build_list_data_ptr_store(header_ptr, new_data_ptr, "list_insert");
        self.build_list_cap_store(list_payload, new_cap, "list_insert");
        self.builder
            .build_unconditional_branch(copy_loop_block)
            .expect("failed to branch to insert copy loop");

        self.builder.position_at_end(copy_loop_block);
        let copy_idx_phi = self
            .builder
            .build_phi(self.i64_type, "list_insert_copy_idx")
            .expect("failed to build insert copy idx phi");
        copy_idx_phi.add_incoming(&[(&self.i64_type.const_zero(), grow_block)]);
        let copy_idx = copy_idx_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, copy_idx, len, "list_insert_copy_more")
            .expect("failed to compare insert copy idx");
        self.builder
            .build_conditional_branch(more, copy_body_block, shift_loop_block)
            .expect("failed to branch insert copy loop");

        self.builder.position_at_end(copy_body_block);
        let moved =
            self.build_list_value_load_from_data_ptr(old_data_ptr, copy_idx, "list_insert_old");
        self.build_list_value_store_from_data_ptr(new_data_ptr, copy_idx, moved, "list_insert_new");
        let next = self
            .builder
            .build_int_add(
                copy_idx,
                self.i64_type.const_int(1, false),
                "list_insert_copy_next",
            )
            .expect("failed to increment insert copy idx");
        self.builder
            .build_unconditional_branch(copy_loop_block)
            .expect("failed to loop insert copy");
        copy_idx_phi.add_incoming(&[(&next, copy_body_block)]);

        self.builder.position_at_end(shift_loop_block);
        let shift_idx_phi = self
            .builder
            .build_phi(self.i64_type, "list_insert_shift_idx")
            .expect("failed to build insert shift idx phi");
        shift_idx_phi.add_incoming(&[(&len, grow_check_block), (&len, copy_loop_block)]);
        let shift_idx = shift_idx_phi.as_basic_value().into_int_value();
        let should_shift = self
            .builder
            .build_int_compare(
                IntPredicate::UGT,
                shift_idx,
                idx,
                "list_insert_should_shift",
            )
            .expect("failed to compare insert shift idx");
        self.builder
            .build_conditional_branch(should_shift, shift_body_block, store_block)
            .expect("failed to branch insert shift loop");

        self.builder.position_at_end(shift_body_block);
        let src_idx = self
            .builder
            .build_int_sub(
                shift_idx,
                self.i64_type.const_int(1, false),
                "list_insert_src_idx",
            )
            .expect("failed to decrement insert shift idx");
        let moved = self.build_list_value_load(list_payload, src_idx, "list_insert_src");
        self.build_list_value_store(list_payload, shift_idx, moved, "list_insert_dst");
        self.builder
            .build_unconditional_branch(shift_loop_block)
            .expect("failed to loop insert shift");
        shift_idx_phi.add_incoming(&[(&src_idx, shift_body_block)]);

        self.builder.position_at_end(store_block);
        self.build_list_value_store(list_payload, idx, value, "list_insert_store");
        let new_len = self
            .builder
            .build_int_add(
                len,
                self.i64_type.const_int(1, false),
                "list_insert_new_len",
            )
            .expect("failed to increment insert len");
        self.build_list_len_store(list_payload, new_len, "list_insert");
        self.builder
            .build_return(Some(&self.make_pair_value(
                list.tag,
                list.payload,
                "list_insert_result",
            )))
            .expect("failed to return list_insert");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_list_swap(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(
                &[
                    self.i64_type.into(),
                    self.i64_type.into(),
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
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let list = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let index_a = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let index_b = CompiledValue {
            tag: function.get_nth_param(4).unwrap().into_int_value(),
            payload: function.get_nth_param(5).unwrap().into_int_value(),
        };
        let list_payload =
            self.expect_tag_payload(list, TAG_LIST, "list_swap_list", ok_block, trap_block);

        self.builder.position_at_end(ok_block);
        let idx_a = self.expect_tag_int(index_a, "list_swap_a", trap_block);
        let idx_b = self.expect_tag_int(index_b, "list_swap_b", trap_block);
        self.build_index_bounds_check(list_payload, idx_a, "list_swap_a", trap_block);
        self.build_index_bounds_check(list_payload, idx_b, "list_swap_b", trap_block);
        let value_a = self.build_list_value_load(list_payload, idx_a, "list_swap_a");
        let value_b = self.build_list_value_load(list_payload, idx_b, "list_swap_b");
        self.build_list_value_store(list_payload, idx_a, value_b, "list_swap_store_a");
        self.build_list_value_store(list_payload, idx_b, value_a, "list_swap_store_b");
        self.builder
            .build_return(Some(&self.make_pair_value(
                list.tag,
                list.payload,
                "list_swap_result",
            )))
            .expect("failed to return list swap");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_list_pop(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type()
                .fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let list = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let list_payload =
            self.expect_tag_payload(list, TAG_LIST, "list_pop_list", ok_block, trap_block);

        self.builder.position_at_end(ok_block);
        let len = self.build_list_len_load(list_payload, "list_pop");
        let non_empty = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                len,
                self.i64_type.const_zero(),
                "list_pop_non_empty",
            )
            .expect("failed to compare list pop len");
        let pop_block = self.context.append_basic_block(function, "pop");
        self.builder
            .build_conditional_branch(non_empty, pop_block, trap_block)
            .expect("failed to branch on list pop len");

        self.builder.position_at_end(pop_block);
        let new_len = self
            .builder
            .build_int_sub(len, self.i64_type.const_int(1, false), "list_pop_new_len")
            .expect("failed to decrement list len");
        self.build_list_len_store(list_payload, new_len, "list_pop");
        let result = self.build_list_value_load(list_payload, new_len, "list_pop");
        self.builder
            .build_return(Some(&self.make_pair_value(
                result.tag,
                result.payload,
                "list_pop_pair",
            )))
            .expect("failed to return list pop");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_pair_list_copy(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type()
                .fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        let loop_block = self.context.append_basic_block(function, "loop");
        let body_block = self.context.append_basic_block(function, "body");
        let done_block = self.context.append_basic_block(function, "done");
        self.builder.position_at_end(entry);

        let list = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let list_payload =
            self.expect_tag_payload(list, TAG_LIST, "list_copy_list", ok_block, trap_block);

        self.builder.position_at_end(ok_block);
        let len = self.build_list_len_load(list_payload, "list_copy");
        let cap = self.build_list_cap_load(list_payload, "list_copy");
        let alloc = self.require_func("__alloc");
        let align = self.i64_type.const_int(8, false);
        let bytes = self
            .builder
            .build_int_mul(cap, self.i64_type.const_int(16, false), "list_copy_bytes")
            .expect("failed to build list copy bytes");
        let new_data_raw = self.build_boxed_call(alloc, &[bytes, align], "list_copy_data");
        let header_size = self.i64_type.const_int(24, false);
        let new_header_raw =
            self.build_boxed_call(alloc, &[header_size, align], "list_copy_header");
        let new_header_ptr = self.build_list_header_ptr(new_header_raw, "list_copy_header");
        let new_data_ptr = self
            .builder
            .build_int_to_ptr(
                new_data_raw,
                self.context.ptr_type(Default::default()),
                "list_copy_data_ptr",
            )
            .expect("failed to convert copy data ptr");
        self.build_list_data_ptr_store(new_header_ptr, new_data_ptr, "list_copy");
        self.build_list_len_store(new_header_raw, len, "list_copy");
        self.build_list_cap_store(new_header_raw, cap, "list_copy");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to branch to list_copy loop");

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, "list_copy_idx")
            .expect("failed to build list copy idx phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), ok_block)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, "list_copy_more")
            .expect("failed to compare list copy idx");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed to branch list copy loop");

        self.builder.position_at_end(body_block);
        let value = self.build_list_value_load(list_payload, idx, "list_copy_src");
        self.build_list_value_store_from_data_ptr(new_data_ptr, idx, value, "list_copy_dst");
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "list_copy_next")
            .expect("failed to increment list copy idx");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to loop list_copy");
        idx_phi.add_incoming(&[(&next, body_block)]);

        self.builder.position_at_end(done_block);
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_LIST as u64, false),
                new_header_raw,
                "list_copy_result",
            )))
            .expect("failed to return list_copy");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_wasm_allocator(&mut self, name: &str, symbol: &str) {
        let offset = self
            .module
            .add_global(self.i64_type, None, "__llvm_wasm_arena_offset");
        offset.set_linkage(Linkage::Internal);
        offset.set_initializer(&self.i64_type.const_int(WASM_ARENA_BASE as u64, false));
        let _ = offset.set_alignment(8);

        let function = self.module.add_function(
            symbol,
            self.i64_type
                .fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
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
            .build_int_sub(
                align,
                self.i64_type.const_int(1, false),
                "alloc_align_minus_one",
            )
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
        self.builder
            .build_return(Some(&aligned))
            .expect("failed to return wasm alloc ptr");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn build_trap_and_unreachable(&self) {
        let void_type = self.context.void_type();
        let trap_fn = self.module.get_function("llvm.trap").unwrap_or_else(|| {
            self.module
                .add_function("llvm.trap", void_type.fn_type(&[], false), None)
        });
        self.builder
            .build_call(trap_fn, &[], "trap")
            .expect("failed to build trap call");
        self.builder
            .build_unreachable()
            .expect("failed to build unreachable");
    }

    fn invert_i1(&self, value: IntValue<'ctx>, name: &str) -> IntValue<'ctx> {
        let one = self.context.bool_type().const_all_ones();
        self.builder
            .build_xor(value, one, name)
            .expect("failed to invert i1")
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
            .build_int_compare(
                IntPredicate::NE,
                rhs,
                zero,
                &format!("{prefix}_rhs_non_zero"),
            )
            .expect("failed to compare rhs");
        let min_i64 = self.i64_type.const_int(i64::MIN as u64, true);
        let neg_one = self.i64_type.const_all_ones();
        let lhs_is_min = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs,
                min_i64,
                &format!("{prefix}_lhs_is_min"),
            )
            .expect("failed to compare lhs min");
        let rhs_is_neg_one = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rhs,
                neg_one,
                &format!("{prefix}_rhs_is_neg_one"),
            )
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
        let result_type = self.context.struct_type(
            &[self.i64_type.into(), self.context.bool_type().into()],
            false,
        );
        let function = self.module.get_function(intrinsic_name).unwrap_or_else(|| {
            self.module.add_function(
                intrinsic_name,
                result_type.fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
                None,
            )
        });
        let call = self
            .builder
            .build_call(
                function,
                &[lhs.into(), rhs.into()],
                &format!("{label}_overflow"),
            )
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
        (
            "list_print",
            crate::runtime::__expr_list_print_host as usize,
        ),
        (
            "__box_value",
            crate::runtime::__expr_box_value_host as usize,
        ),
        ("__alloc", crate::runtime::__expr_alloc_host as usize),
    ];

    for (name, addr) in mappings {
        let function = functions
            .get(name)
            .unwrap_or_else(|| panic!("missing function declaration: {name}"));
        execution_engine.add_global_mapping(function, addr);
    }
}
