use super::{Module, is_builtin_name, local_var_names};
use crate::parser::{Ast, ExpressionAst, FunctionDefAst, LiteralAst};
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
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
}

pub(super) fn compile_to_jit(expr_module: Module) -> LlvmJitModule {
    Target::initialize_native(&InitializationConfig::default())
        .unwrap_or_else(|e| panic!("failed to initialize LLVM native target: {e}"));

    let (context, module, _machine) = create_codegen_context("expr");

    let functions = {
        let mut compiler = LlvmCompiler::new(context, module);
        compiler.declare_runtime_functions();
        compiler.declare_user_functions(&expr_module.functions, LlvmOutputMode::Jit);
        compiler.define_user_functions(&expr_module.functions);
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
}

impl<'ctx> LlvmCompiler<'ctx> {
    fn new(context: &'ctx Context, module: &'ctx LlvmModule<'ctx>) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            i64_type: context.i64_type(),
            functions: HashMap::new(),
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
                "__value_int",
                "__expr_value_int_host",
                vec![i64_type.into()],
            ),
            (
                "__value_to_i64",
                "__expr_value_to_i64_host",
                vec![i64_type.into()],
            ),
            (
                "__value_is_truthy",
                "__expr_value_is_truthy_host",
                vec![i64_type.into()],
            ),
            (
                "__op_add",
                "__expr_add_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            (
                "__op_subtract",
                "__expr_subtract_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            (
                "__op_multiply",
                "__expr_multiply_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            (
                "__op_divide",
                "__expr_divide_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            (
                "__op_modulo",
                "__expr_modulo_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            (
                "__op_gt",
                "__expr_gt_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            (
                "__op_lt",
                "__expr_lt_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            (
                "__op_gte",
                "__expr_gte_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            (
                "__op_lte",
                "__expr_lte_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            (
                "__op_eq",
                "__expr_eq_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            (
                "__op_ne",
                "__expr_ne_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            ("list_new", "__expr_list_new_host", vec![]),
            (
                "list_push",
                "__expr_list_push_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            ("list_len", "__expr_list_len_host", vec![i64_type.into()]),
            (
                "list_get",
                "__expr_list_get_host",
                vec![i64_type.into(), i64_type.into()],
            ),
            ("list_pop", "__expr_list_pop_host", vec![i64_type.into()]),
            ("list_copy", "__expr_list_copy_host", vec![i64_type.into()]),
        ];

        for (name, symbol, params) in runtime {
            let function =
                self.module
                    .add_function(symbol, self.i64_type.fn_type(&params, false), None);
            self.functions.insert(name.to_string(), function);
        }
    }

    fn declare_user_functions(&mut self, functions: &[FunctionDefAst], mode: LlvmOutputMode) {
        for func in functions {
            let params = vec![self.i64_type.into(); func.inputs.len()];
            let symbol = function_symbol_name(func, mode);
            let function =
                self.module
                    .add_function(&symbol, self.i64_type.fn_type(&params, false), None);
            self.functions.insert(func.name.clone(), function);
        }
    }

    fn define_user_functions(&self, functions: &[FunctionDefAst]) {
        for func in functions {
            self.define_user_function(func);
        }
    }

    fn define_user_function(&self, func_def: &FunctionDefAst) {
        let function = self.require_func(&func_def.name);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let mut vars = HashMap::new();
        for (index, name) in func_def.inputs.iter().enumerate() {
            let ptr = self
                .builder
                .build_alloca(self.i64_type, name)
                .expect("failed to allocate function param");
            let value = function
                .get_nth_param(index as u32)
                .unwrap_or_else(|| panic!("missing param {index} for {}", func_def.name))
                .into_int_value();
            self.builder
                .build_store(ptr, value)
                .expect("failed to store function param");
            vars.insert(name.clone(), ptr);
        }

        for name in local_var_names(&func_def.block) {
            vars.entry(name.clone()).or_insert_with(|| {
                self.builder
                    .build_alloca(self.i64_type, &name)
                    .expect("failed to allocate local")
            });
        }

        let mut last_val = None;
        for line in &func_def.block.lines {
            last_val = Some(self.compile_ast(line, &vars, function));
        }

        if let Some(value) = last_val {
            self.builder
                .build_return(Some(&value))
                .expect("failed to build return");
        } else {
            let zero = self.call_func("__value_int", &[self.i64_type.const_zero()], "empty_block");
            self.builder
                .build_return(Some(&zero))
                .expect("failed to build empty return");
        }
    }

    fn compile_ast(
        &self,
        ast: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        function: FunctionValue<'ctx>,
    ) -> IntValue<'ctx> {
        match ast {
            Ast::Literal(LiteralAst::Integer(n)) => self.call_func(
                "__value_int",
                &[self.i64_type.const_int(*n as u64, true)],
                "int",
            ),
            Ast::ListLiteral(items) => {
                let list = self.call_func("list_new", &[], "list_new");
                for item in items {
                    let value = self.compile_ast(item, vars, function);
                    let _ = self.call_func("list_push", &[list, value], "list_push");
                }
                list
            }
            Ast::Index { collection, index } => {
                let collection = self.compile_ast(collection, vars, function);
                let index = self.compile_ast(index, vars, function);
                self.call_func("list_get", &[collection, index], "list_get")
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
                    "add" => self.call_func("__op_add", &[compiled[0], compiled[1]], "add"),
                    "subtract" => {
                        self.call_func("__op_subtract", &[compiled[0], compiled[1]], "subtract")
                    }
                    "multiply" => {
                        self.call_func("__op_multiply", &[compiled[0], compiled[1]], "multiply")
                    }
                    "divide" => {
                        self.call_func("__op_divide", &[compiled[0], compiled[1]], "divide")
                    }
                    "modulo" => {
                        self.call_func("__op_modulo", &[compiled[0], compiled[1]], "modulo")
                    }
                    "gt" => self.call_func("__op_gt", &[compiled[0], compiled[1]], "gt"),
                    "lt" => self.call_func("__op_lt", &[compiled[0], compiled[1]], "lt"),
                    "gte" => self.call_func("__op_gte", &[compiled[0], compiled[1]], "gte"),
                    "lte" => self.call_func("__op_lte", &[compiled[0], compiled[1]], "lte"),
                    "eq" => self.call_func("__op_eq", &[compiled[0], compiled[1]], "eq"),
                    "ne" => self.call_func("__op_ne", &[compiled[0], compiled[1]], "ne"),
                    other => {
                        let callee = self.require_func(other);
                        self.build_call(callee, &compiled, other)
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
                self.builder
                    .build_load(self.i64_type, *ptr, name)
                    .expect("failed to load variable")
                    .into_int_value()
            }
            Ast::Assign { name, value } => {
                let value = self.compile_ast(value, vars, function);
                let ptr = vars
                    .get(name)
                    .unwrap_or_else(|| panic!("undeclared variable: {name}"));
                self.builder
                    .build_store(*ptr, value)
                    .expect("failed to assign variable");
                value
            }
            Ast::If {
                condition,
                then,
                else_,
            } => {
                let cond_value = self.compile_ast(condition, vars, function);
                let truth = self.call_func("__value_is_truthy", &[cond_value], "truthy");
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
                let mut then_value =
                    self.call_func("__value_int", &[self.i64_type.const_zero()], "then_zero");
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
                let mut else_value =
                    self.call_func("__value_int", &[self.i64_type.const_zero()], "else_zero");
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
                let phi = self
                    .builder
                    .build_phi(self.i64_type, "if_result")
                    .expect("failed to build phi");
                phi.add_incoming(&[(&then_value, then_end), (&else_value, else_end)]);
                phi.as_basic_value().into_int_value()
            }
            Ast::FunctionDef(_) => panic!("nested function definitions are not supported"),
        }
    }

    fn call_func(&self, name: &str, args: &[IntValue<'ctx>], label: &str) -> IntValue<'ctx> {
        let function = self.require_func(name);
        self.build_call(function, args, label)
    }

    fn build_call(
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

    fn require_func(&self, name: &str) -> FunctionValue<'ctx> {
        *self
            .functions
            .get(name)
            .unwrap_or_else(|| panic!("missing function declaration: {name}"))
    }
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
            "__value_int",
            crate::runtime::__expr_value_int_host as usize,
        ),
        (
            "__value_to_i64",
            crate::runtime::__expr_value_to_i64_host as usize,
        ),
        (
            "__value_is_truthy",
            crate::runtime::__expr_value_is_truthy_host as usize,
        ),
        ("__op_add", crate::runtime::__expr_add_host as usize),
        (
            "__op_subtract",
            crate::runtime::__expr_subtract_host as usize,
        ),
        (
            "__op_multiply",
            crate::runtime::__expr_multiply_host as usize,
        ),
        ("__op_divide", crate::runtime::__expr_divide_host as usize),
        ("__op_modulo", crate::runtime::__expr_modulo_host as usize),
        ("__op_gt", crate::runtime::__expr_gt_host as usize),
        ("__op_lt", crate::runtime::__expr_lt_host as usize),
        ("__op_gte", crate::runtime::__expr_gte_host as usize),
        ("__op_lte", crate::runtime::__expr_lte_host as usize),
        ("__op_eq", crate::runtime::__expr_eq_host as usize),
        ("__op_ne", crate::runtime::__expr_ne_host as usize),
        ("list_new", crate::runtime::__expr_list_new_host as usize),
        ("list_push", crate::runtime::__expr_list_push_host as usize),
        ("list_len", crate::runtime::__expr_list_len_host as usize),
        ("list_get", crate::runtime::__expr_list_get_host as usize),
        ("list_pop", crate::runtime::__expr_list_pop_host as usize),
        ("list_copy", crate::runtime::__expr_list_copy_host as usize),
    ];

    for (name, addr) in mappings {
        let function = functions
            .get(name)
            .unwrap_or_else(|| panic!("missing function declaration: {name}"));
        execution_engine.add_global_mapping(function, addr);
    }
}
