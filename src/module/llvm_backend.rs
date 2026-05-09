use super::{Module, function_arities, function_ordinals, is_builtin_name, local_var_names};
use crate::parser::{Ast, ExpressionAst, FunctionDefAst, LiteralAst};
use crate::value::{TAG_FUNCTION, TAG_INT, TAG_LIST};
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
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
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
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
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
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
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
        compiler.function_ordinals = function_ordinals(&expr_module.functions);
        compiler.function_arities = function_arities(&expr_module.functions);
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
    functions: HashMap<String, FunctionValue<'ctx>>,
    function_ordinals: HashMap<String, i64>,
    function_arities: HashMap<String, usize>,
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
            functions: HashMap::new(),
            function_ordinals: HashMap::new(),
            function_arities: HashMap::new(),
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
        self.define_runtime_operation("__op_add", "llvm_rt_add", BinaryArithOp::Add);
        self.define_runtime_operation("__op_subtract", "llvm_rt_subtract", BinaryArithOp::Subtract);
        self.define_runtime_operation("__op_multiply", "llvm_rt_multiply", BinaryArithOp::Multiply);
        self.define_runtime_operation("__op_divide", "llvm_rt_divide", BinaryArithOp::Divide);
        self.define_runtime_operation("__op_modulo", "llvm_rt_modulo", BinaryArithOp::Modulo);
        self.define_runtime_compare("__op_gt", "llvm_rt_gt", IntPredicate::SGT);
        self.define_runtime_compare("__op_lt", "llvm_rt_lt", IntPredicate::SLT);
        self.define_runtime_compare("__op_gte", "llvm_rt_gte", IntPredicate::SGE);
        self.define_runtime_compare("__op_lte", "llvm_rt_lte", IntPredicate::SLE);
        self.define_runtime_compare("__op_eq", "llvm_rt_eq", IntPredicate::EQ);
        self.define_runtime_compare("__op_ne", "llvm_rt_ne", IntPredicate::NE);
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
        self.define_pair_list_new("__rt_list_new", "llvm_rt_list_new");
        self.define_pair_list_push("__rt_list_push", "llvm_rt_list_push");
        self.define_pair_list_insert("__rt_list_insert", "llvm_rt_list_insert");
        self.define_pair_list_len("__rt_list_len", "llvm_rt_list_len");
        self.define_pair_list_get("__rt_list_get", "llvm_rt_list_get");
        self.define_pair_list_set("__rt_list_set", "llvm_rt_list_set");
        self.define_pair_list_swap("__rt_list_swap", "llvm_rt_list_swap");
        self.define_pair_list_pop("__rt_list_pop", "llvm_rt_list_pop");
        self.define_pair_list_copy("__rt_list_copy", "llvm_rt_list_copy");
    }

    fn declare_user_functions(&mut self, functions: &[FunctionDefAst], mode: LlvmOutputMode) {
        let _ = mode;
        for func in functions {
            let internal_params = vec![self.i64_type.into(); func.inputs.len() * 2];
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

        let value = self.build_internal_call(internal, &[], "int_result_value");
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
        self.builder.position_at_end(entry);

        let mut vars = HashMap::new();
        for (index, name) in func_def.inputs.iter().enumerate() {
            let ptr = self
                .builder
                .build_alloca(self.pair_type(), name)
                .expect("failed to allocate function param");
            let tag = function
                .get_nth_param((index * 2) as u32)
                .unwrap_or_else(|| panic!("missing tag param {index} for {}", func_def.name))
                .into_int_value();
            let payload = function
                .get_nth_param((index * 2 + 1) as u32)
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

        let mut last_val = None;
        for line in &func_def.block.lines {
            last_val = Some(self.compile_ast(line, &vars, function));
        }

        if let Some(value) = last_val {
            self.builder
                .build_return(Some(&self.make_pair_value(
                    value.tag,
                    value.payload,
                    "return_pair",
                )))
                .expect("failed to build return");
        } else {
            let zero = self.int_value(self.i64_type.const_zero());
            self.builder
                .build_return(Some(&self.make_pair_value(
                    zero.tag,
                    zero.payload,
                    "empty_pair",
                )))
                .expect("failed to build empty return");
        }
    }

    fn apply_unary_function_value(
        &self,
        callback: CompiledValue<'ctx>,
        arg: CompiledValue<'ctx>,
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
                (self.function_arities.get(name) == Some(&1usize))
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

        let mut current_check = first_check;
        let mut incomings = Vec::with_capacity(candidates.len());
        for (index, (ordinal, name)) in candidates.iter().enumerate() {
            self.builder.position_at_end(current_check);
            let matched = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    callback.payload,
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
            let result = self.build_internal_call(
                self.require_func(name),
                &[arg],
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
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        assert_eq!(args.len(), 2, "list_map expects 2 arguments");
        self.validate_unary_callback_ast(&args[1], vars, "list_map");
        let input = self.compile_ast(&args[0], vars, function);
        let callback = self.compile_ast(&args[1], vars, function);
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
        let mapped = self.apply_unary_function_value(callback, item, function, "list_map");
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
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        assert_eq!(args.len(), 2, "list_filter expects 2 arguments");
        self.validate_unary_callback_ast(&args[1], vars, "list_filter");
        let input = self.compile_ast(&args[0], vars, function);
        let callback = self.compile_ast(&args[1], vars, function);
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
        let predicate = self.apply_unary_function_value(callback, item, function, "list_filter");
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
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        assert_eq!(args.len(), 2, "list_range expects 2 arguments");
        let start_value = self.compile_ast(&args[0], vars, function);
        let end_value = self.compile_ast(&args[1], vars, function);
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

    fn compile_ast(
        &self,
        ast: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        match ast {
            Ast::Literal(LiteralAst::Integer(n)) => {
                self.int_value(self.i64_type.const_int(*n as u64, true))
            }
            Ast::Lambda { .. } => {
                panic!("anonymous functions are not implemented by the llvm backend yet");
            }
            Ast::FunctionRef(name) => {
                let ordinal = *self.function_ordinals.get(name).unwrap_or_else(|| {
                    panic!("missing function ordinal for function reference: {name}")
                });
                CompiledValue {
                    tag: self.i64_type.const_int(TAG_FUNCTION as u64, false),
                    payload: self.i64_type.const_int(ordinal as u64, true),
                }
            }
            Ast::ListLiteral(items) => {
                let list =
                    self.build_internal_call(self.require_func("__rt_list_new"), &[], "list_new");
                for item in items {
                    let value = self.compile_ast(item, vars, function);
                    let _ = self.build_internal_call(
                        self.require_func("__rt_list_push"),
                        &[list, value],
                        "list_push",
                    );
                }
                list
            }
            Ast::Index { collection, index } => {
                let collection = self.compile_ast(collection, vars, function);
                let index = self.compile_ast(index, vars, function);
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
                let collection = self.compile_ast(collection, vars, function);
                let index = self.compile_ast(index, vars, function);
                let value = self.compile_ast(value, vars, function);
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
                    return self.compile_list_map(args, vars, function);
                }
                if name == "list_filter" {
                    return self.compile_list_filter(args, vars, function);
                }
                if name == "list_range" {
                    return self.compile_list_range(args, vars, function);
                }
                let compiled = args
                    .iter()
                    .map(|arg| self.compile_ast(arg, vars, function))
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
                    "print" => self.build_internal_call(
                        self.require_func("__rt_print"),
                        &compiled,
                        "print",
                    ),
                    "list_print" => self.build_internal_call(
                        self.require_func("__rt_list_print"),
                        &compiled,
                        "list_print",
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
                        let callee = self.require_func(other);
                        self.build_internal_call(callee, &compiled, other)
                    }
                }
            }
            Ast::Block(block) => {
                let mut last = None;
                for line in &block.lines {
                    last = Some(self.compile_ast(line, vars, function));
                }
                last.expect("empty block")
            }
            Ast::Variable(name) => {
                if let Some(ptr) = vars.get(name) {
                    self.load_compiled_value(*ptr, name)
                } else if let Some(&ordinal) = self.function_ordinals.get(name) {
                    CompiledValue {
                        tag: self.i64_type.const_int(TAG_FUNCTION as u64, false),
                        payload: self.i64_type.const_int(ordinal as u64, true),
                    }
                } else {
                    panic!("undefined variable: {name}");
                }
            }
            Ast::Assign { name, value } => {
                let value = self.compile_ast(value, vars, function);
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
                let cond_value = self.compile_ast(condition, vars, function);
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
                    then_value = self.compile_ast(line, vars, function);
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
                        else_value = self.compile_ast(line, vars, function);
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

    fn define_runtime_operation(&mut self, name: &str, symbol: &str, op: BinaryArithOp) {
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

        let lhs_tag = function.get_first_param().unwrap().into_int_value();
        let lhs_payload = function.get_nth_param(1).unwrap().into_int_value();
        let rhs_tag = function.get_nth_param(2).unwrap().into_int_value();
        let rhs_payload = function.get_nth_param(3).unwrap().into_int_value();
        let lhs_raw = self.build_internal_scalar_call(
            self.require_func("__value_to_i64"),
            &[CompiledValue {
                tag: lhs_tag,
                payload: lhs_payload,
            }],
            "lhs_raw",
        );
        let rhs_raw = self.build_internal_scalar_call(
            self.require_func("__value_to_i64"),
            &[CompiledValue {
                tag: rhs_tag,
                payload: rhs_payload,
            }],
            "rhs_raw",
        );
        let raw = match op {
            BinaryArithOp::Add => {
                let (value, overflow) = self.build_overflow_intrinsic_call(
                    "llvm.sadd.with.overflow.i64",
                    lhs_raw,
                    rhs_raw,
                    "add",
                );
                self.builder
                    .build_conditional_branch(overflow, trap_block, ok_block)
                    .expect("failed to branch on add overflow");
                self.builder.position_at_end(ok_block);
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
                    .build_conditional_branch(overflow, trap_block, ok_block)
                    .expect("failed to branch on subtract overflow");
                self.builder.position_at_end(ok_block);
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
                    .build_conditional_branch(overflow, trap_block, ok_block)
                    .expect("failed to branch on multiply overflow");
                self.builder.position_at_end(ok_block);
                value
            }
            BinaryArithOp::Divide => {
                let div_ok = self.build_division_safe_check(lhs_raw, rhs_raw, "div");
                self.builder
                    .build_conditional_branch(div_ok, ok_block, trap_block)
                    .expect("failed to build div branch");
                self.builder.position_at_end(ok_block);
                self.builder
                    .build_int_signed_div(lhs_raw, rhs_raw, "quot")
                    .expect("failed to divide")
            }
            BinaryArithOp::Modulo => {
                let rem_ok = self.build_division_safe_check(lhs_raw, rhs_raw, "rem");
                self.builder
                    .build_conditional_branch(rem_ok, ok_block, trap_block)
                    .expect("failed to build rem branch");
                self.builder.position_at_end(ok_block);
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

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_runtime_compare(&mut self, name: &str, symbol: &str, pred: IntPredicate) {
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
        self.builder.position_at_end(entry);

        let lhs_raw = self.build_internal_scalar_call(
            self.require_func("__value_to_i64"),
            &[CompiledValue {
                tag: function.get_first_param().unwrap().into_int_value(),
                payload: function.get_nth_param(1).unwrap().into_int_value(),
            }],
            "lhs_raw",
        );
        let rhs_raw = self.build_internal_scalar_call(
            self.require_func("__value_to_i64"),
            &[CompiledValue {
                tag: function.get_nth_param(2).unwrap().into_int_value(),
                payload: function.get_nth_param(3).unwrap().into_int_value(),
            }],
            "rhs_raw",
        );
        let cmp = self
            .builder
            .build_int_compare(pred, lhs_raw, rhs_raw, "cmp")
            .expect("failed to build compare");
        let raw = self
            .builder
            .build_int_z_extend(cmp, self.i64_type, "cmp_i64")
            .expect("failed to extend compare");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                raw,
                "cmp_result",
            )))
            .expect("failed to return compare");
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
