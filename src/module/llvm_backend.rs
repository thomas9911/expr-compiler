use super::{Module, is_builtin_name, local_var_names};
use crate::parser::{Ast, ExpressionAst, FunctionDefAst, LiteralAst};
use crate::value::{TAG_INT, TAG_LIST};
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
}

impl LlvmJitModule {
    pub fn get_fn_ptr(&self, name: &str) -> *const u8 {
        let addr = self
            .execution_engine
            .get_function_address(name)
            .unwrap_or_else(|e| panic!("unable to find JIT function '{name}': {e}"));
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

    let (context, module, _machine) = create_codegen_context("expr");

    let int_result_function_names = expr_module
        .functions
        .iter()
        .filter(|func| func.inputs.is_empty())
        .map(|func| func.name.clone())
        .collect::<HashSet<_>>();

    let functions = {
        let mut compiler = LlvmCompiler::new(context, module);
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

    let (context, module, machine) = create_codegen_context(name);
    {
        let mut compiler = LlvmCompiler::new(context, module);
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

fn create_codegen_context(
    module_name: &str,
) -> (
    &'static Context,
    &'static LlvmModule<'static>,
    TargetMachine,
) {
    let context = Box::leak(Box::new(Context::create()));
    let module = Box::leak(Box::new(context.create_module(module_name)));

    let triple = TargetMachine::get_default_triple();
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
    functions: HashMap<String, FunctionValue<'ctx>>,
    public_functions: HashMap<String, FunctionValue<'ctx>>,
}

#[derive(Clone, Copy)]
struct CompiledValue<'ctx> {
    tag: IntValue<'ctx>,
    payload: IntValue<'ctx>,
}

impl<'ctx> LlvmCompiler<'ctx> {
    fn new(context: &'ctx Context, module: &'ctx LlvmModule<'ctx>) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            i64_type: context.i64_type(),
            functions: HashMap::new(),
            public_functions: HashMap::new(),
        }
    }

    fn into_functions(self) -> HashMap<String, FunctionValue<'ctx>> {
        self.functions
    }

    fn declare_runtime_functions(&mut self) {
        let i64_type = self.i64_type;
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
            let function =
                self.module
                    .add_function(symbol, self.i64_type.fn_type(&params, false), None);
            self.functions.insert(name.to_string(), function);
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
        self.define_boxed_runtime_pair_wrapper("__rt_print", "llvm_rt_print", "print", 1);
        self.define_boxed_runtime_pair_wrapper(
            "__rt_list_print",
            "llvm_rt_list_print",
            "list_print",
            1,
        );
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
        for func in functions {
            let internal_params = vec![self.i64_type.into(); func.inputs.len() * 2];
            let internal_symbol = internal_symbol_name(&func.name);
            let internal = self.module.add_function(
                &internal_symbol,
                self.pair_type().fn_type(&internal_params, false),
                Some(Linkage::Private),
            );
            self.functions.insert(func.name.clone(), internal);

            let public_params = vec![self.i64_type.into(); func.inputs.len()];
            let public_symbol = function_symbol_name(func, mode);
            let public = self.module.add_function(
                &public_symbol,
                self.i64_type.fn_type(&public_params, false),
                None,
            );
            self.public_functions.insert(func.name.clone(), public);
        }
    }

    fn define_user_functions(&self, functions: &[FunctionDefAst]) {
        for func in functions {
            self.define_user_function(func);
            self.define_public_wrapper(func);
        }
    }

    fn define_int_result_wrappers(&self, functions: &[FunctionDefAst], mode: LlvmOutputMode) {
        for func in functions {
            if func.inputs.is_empty() {
                self.define_int_result_wrapper(func, mode);
            }
        }
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

    fn define_public_wrapper(&self, func_def: &FunctionDefAst) {
        let function = self.require_public_func(&func_def.name);
        let internal = self.require_func(&func_def.name);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let args = function
            .get_param_iter()
            .map(|param| self.unbox_handle(param.into_int_value(), "public_arg"))
            .collect::<Vec<_>>();
        let result = self.build_internal_call(internal, &args, "public_call");
        let handle = self.box_compiled_value(result, "public_result");
        self.builder
            .build_return(Some(&handle))
            .expect("failed to build public wrapper return");
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
                let ptr = vars
                    .get(name)
                    .unwrap_or_else(|| panic!("undefined variable: {name}"));
                self.load_compiled_value(*ptr, name)
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

    fn call_func(&self, name: &str, args: &[IntValue<'ctx>], label: &str) -> IntValue<'ctx> {
        let function = self.require_func(name);
        self.build_boxed_call(function, args, label)
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

    fn require_public_func(&self, name: &str) -> FunctionValue<'ctx> {
        *self
            .public_functions
            .get(name)
            .unwrap_or_else(|| panic!("missing public function declaration: {name}"))
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
        self.builder
            .build_load(
                self.context.ptr_type(Default::default()),
                data_ptr_ptr,
                &format!("{label}_data_ptr"),
            )
            .expect("failed to load list data ptr")
            .into_pointer_value()
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
        self.builder
            .build_store(data_ptr_ptr, data_ptr)
            .expect("failed to store list data ptr");
    }

    fn build_list_handle_ptr(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        let data_ptr = self.build_list_data_ptr_load(payload, label);
        unsafe {
            self.builder
                .build_gep(
                    self.i64_type,
                    data_ptr,
                    &[index],
                    &format!("{label}_handle_ptr"),
                )
                .expect("failed to build list handle gep")
        }
    }

    fn build_list_handle_load(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let handle_ptr = self.build_list_handle_ptr(payload, index, label);
        self.builder
            .build_load(self.i64_type, handle_ptr, &format!("{label}_handle"))
            .expect("failed to load list handle")
            .into_int_value()
    }

    fn build_list_handle_store(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        handle: IntValue<'ctx>,
        label: &str,
    ) {
        let handle_ptr = self.build_list_handle_ptr(payload, index, label);
        self.builder
            .build_store(handle_ptr, handle)
            .expect("failed to store list handle");
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
        self.context.struct_type(
            &[
                self.context.ptr_type(Default::default()).into(),
                self.i64_type.into(),
                self.i64_type.into(),
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
        let bytes = self.i64_type.const_int(1024 * 8, false);
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
        let new_cap = self
            .builder
            .build_int_mul(cap, two, "list_push_new_cap")
            .expect("failed to multiply list cap");
        let bytes = self
            .builder
            .build_int_mul(
                new_cap,
                self.i64_type.const_int(8, false),
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
        let old_data_ptr = self.build_list_data_ptr_load(list_payload, "list_push_old_data");
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
        let old_handle_ptr = unsafe {
            self.builder
                .build_gep(
                    self.i64_type,
                    old_data_ptr,
                    &[copy_idx],
                    "list_push_old_handle_ptr",
                )
                .expect("failed to build old handle gep")
        };
        let old_handle = self
            .builder
            .build_load(self.i64_type, old_handle_ptr, "list_push_old_handle")
            .expect("failed to load old handle")
            .into_int_value();
        let new_handle_ptr = unsafe {
            self.builder
                .build_gep(
                    self.i64_type,
                    new_data_ptr,
                    &[copy_idx],
                    "list_push_new_handle_ptr",
                )
                .expect("failed to build new handle gep")
        };
        self.builder
            .build_store(new_handle_ptr, old_handle)
            .expect("failed to store copied handle");
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
        let boxed = self.box_compiled_value(value, "list_push_boxed");
        self.build_list_handle_store(list_payload, len, boxed, "list_push_store");
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
        let handle = self.build_list_handle_load(list_payload, idx, "list_get");
        let result = self.unbox_handle(handle, "list_get_result");
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
        let handle = self.box_compiled_value(value, "list_set_boxed");
        self.build_list_handle_store(list_payload, idx, handle, "list_set");
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
                self.i64_type.const_int(8, false),
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
        let old_data_ptr = self.build_list_data_ptr_load(list_payload, "list_insert_old_data");
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
        let old_handle_ptr = unsafe {
            self.builder
                .build_gep(
                    self.i64_type,
                    old_data_ptr,
                    &[copy_idx],
                    "list_insert_old_handle_ptr",
                )
                .expect("failed to build insert old handle gep")
        };
        let old_handle = self
            .builder
            .build_load(self.i64_type, old_handle_ptr, "list_insert_old_handle")
            .expect("failed to load insert old handle")
            .into_int_value();
        let new_handle_ptr = unsafe {
            self.builder
                .build_gep(
                    self.i64_type,
                    new_data_ptr,
                    &[copy_idx],
                    "list_insert_new_handle_ptr",
                )
                .expect("failed to build insert new handle gep")
        };
        self.builder
            .build_store(new_handle_ptr, old_handle)
            .expect("failed to store insert copied handle");
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
        let moved = self.build_list_handle_load(list_payload, src_idx, "list_insert_src");
        self.build_list_handle_store(list_payload, shift_idx, moved, "list_insert_dst");
        self.builder
            .build_unconditional_branch(shift_loop_block)
            .expect("failed to loop insert shift");
        shift_idx_phi.add_incoming(&[(&src_idx, shift_body_block)]);

        self.builder.position_at_end(store_block);
        let boxed = self.box_compiled_value(value, "list_insert_boxed");
        self.build_list_handle_store(list_payload, idx, boxed, "list_insert_store");
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
        let handle_a = self.build_list_handle_load(list_payload, idx_a, "list_swap_a");
        let handle_b = self.build_list_handle_load(list_payload, idx_b, "list_swap_b");
        self.build_list_handle_store(list_payload, idx_a, handle_b, "list_swap_store_a");
        self.build_list_handle_store(list_payload, idx_b, handle_a, "list_swap_store_b");
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
        let handle = self.build_list_handle_load(list_payload, new_len, "list_pop");
        let result = self.unbox_handle(handle, "list_pop_result");
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
            .build_int_mul(cap, self.i64_type.const_int(8, false), "list_copy_bytes")
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
        let handle = self.build_list_handle_load(list_payload, idx, "list_copy_src");
        self.build_list_handle_store(new_header_raw, idx, handle, "list_copy_dst");
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

fn function_symbol_name(func: &FunctionDefAst, mode: LlvmOutputMode) -> String {
    if func.name == "main" && matches!(mode, LlvmOutputMode::Executable) {
        #[cfg(windows)]
        {
            return "expr_main_entry".to_string();
        }
        #[cfg(not(windows))]
        {
            return "__expr_main".to_string();
        }
    }

    func.name.clone()
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
