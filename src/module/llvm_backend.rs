use super::{Module, is_builtin_name, local_var_names};
use crate::parser::{Ast, ExpressionAst, FunctionDefAst, LiteralAst};
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
    const VALUE_TAG_INT: u64 = 1;
    const VALUE_TAG_LIST: u64 = 2;

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

        self.define_value_to_i64();
        self.define_value_is_truthy();
        self.define_runtime_operation("__op_add", "llvm_rt_add", BinaryArithOp::Add);
        self.define_runtime_operation(
            "__op_subtract",
            "llvm_rt_subtract",
            BinaryArithOp::Subtract,
        );
        self.define_runtime_operation(
            "__op_multiply",
            "llvm_rt_multiply",
            BinaryArithOp::Multiply,
        );
        self.define_runtime_operation("__op_divide", "llvm_rt_divide", BinaryArithOp::Divide);
        self.define_runtime_operation("__op_modulo", "llvm_rt_modulo", BinaryArithOp::Modulo);
        self.define_runtime_compare("__op_gt", "llvm_rt_gt", IntPredicate::SGT);
        self.define_runtime_compare("__op_lt", "llvm_rt_lt", IntPredicate::SLT);
        self.define_runtime_compare("__op_gte", "llvm_rt_gte", IntPredicate::SGE);
        self.define_runtime_compare("__op_lte", "llvm_rt_lte", IntPredicate::SLE);
        self.define_runtime_compare("__op_eq", "llvm_rt_eq", IntPredicate::EQ);
        self.define_runtime_compare("__op_ne", "llvm_rt_ne", IntPredicate::NE);
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

    fn build_value_tag_load(
        &self,
        value_ptr: PointerValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
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
            self.i64_type.fn_type(&[self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert("__value_to_i64".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let handle = function.get_first_param().unwrap().into_int_value();
        let ptr = self
            .builder
            .build_int_to_ptr(
                handle,
                self.context.ptr_type(Default::default()),
                "value_to_i64_ptr",
            )
            .expect("failed to convert handle to pointer");

        let tag = self.build_value_tag_load(ptr, "value_to_i64");
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                self.context.i8_type().const_int(Self::VALUE_TAG_INT, false),
                "value_to_i64_is_int",
            )
            .expect("failed to compare value tag");
        self.builder
            .build_conditional_branch(is_int, ok_block, trap_block)
            .expect("failed to branch on int tag");

        self.builder.position_at_end(ok_block);
        let raw = self.build_value_payload_load(ptr, "value_to_i64");

        self.builder
            .build_return(Some(&raw))
            .expect("failed to return raw int");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_value_is_truthy(&mut self) {
        let function = self.module.add_function(
            "llvm_rt_value_is_truthy",
            self.i64_type.fn_type(&[self.i64_type.into()], false),
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

        let handle = function.get_first_param().unwrap().into_int_value();
        let value_ptr = self
            .builder
            .build_int_to_ptr(
                handle,
                self.context.ptr_type(Default::default()),
                "truthy_value_ptr",
            )
            .expect("failed to convert truthy handle to pointer");
        let tag = self.build_value_tag_load(value_ptr, "truthy");
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                self.context.i8_type().const_int(Self::VALUE_TAG_INT, false),
                "truthy_is_int",
            )
            .expect("failed to compare int tag");
        let is_list = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                self.context.i8_type().const_int(Self::VALUE_TAG_LIST, false),
                "truthy_is_list",
            )
            .expect("failed to compare list tag");
        self.builder
            .build_conditional_branch(is_int, int_block, list_check_block)
            .expect("failed to branch on int truthiness");

        self.builder.position_at_end(int_block);
        let int_payload = self.build_value_payload_load(value_ptr, "truthy_int");
        let int_truthy = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                int_payload,
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
        let list_payload = self.build_value_payload_load(value_ptr, "truthy_list");
        let list_ptr = self
            .builder
            .build_int_to_ptr(
                list_payload,
                self.context.ptr_type(Default::default()),
                "truthy_list_ptr",
            )
            .expect("failed to convert list payload to pointer");
        let len_ptr = self
            .builder
            .build_struct_gep(
                self.list_header_type(),
                list_ptr,
                1,
                "truthy_list_len_ptr",
            )
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
            self.i64_type
                .fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let lhs = function.get_first_param().unwrap().into_int_value();
        let rhs = function.get_nth_param(1).unwrap().into_int_value();
        let lhs_raw = self.call_func("__value_to_i64", &[lhs], "lhs_raw");
        let rhs_raw = self.call_func("__value_to_i64", &[rhs], "rhs_raw");
        let raw = match op {
            BinaryArithOp::Add => {
                let (value, overflow) =
                    self.build_overflow_intrinsic_call("llvm.sadd.with.overflow.i64", lhs_raw, rhs_raw, "add");
                self.builder
                    .build_conditional_branch(overflow, trap_block, ok_block)
                    .expect("failed to branch on add overflow");
                self.builder.position_at_end(ok_block);
                value
            }
            BinaryArithOp::Subtract => {
                let (value, overflow) =
                    self.build_overflow_intrinsic_call("llvm.ssub.with.overflow.i64", lhs_raw, rhs_raw, "sub");
                self.builder
                    .build_conditional_branch(overflow, trap_block, ok_block)
                    .expect("failed to branch on subtract overflow");
                self.builder.position_at_end(ok_block);
                value
            }
            BinaryArithOp::Multiply => {
                let (value, overflow) =
                    self.build_overflow_intrinsic_call("llvm.smul.with.overflow.i64", lhs_raw, rhs_raw, "mul");
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

        let boxed = self.call_func("__value_int", &[raw], "boxed");
        self.builder
            .build_return(Some(&boxed))
            .expect("failed to build runtime return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    fn define_runtime_compare(&mut self, name: &str, symbol: &str, pred: IntPredicate) {
        let function = self.module.add_function(
            symbol,
            self.i64_type
                .fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let lhs = function.get_first_param().unwrap().into_int_value();
        let rhs = function.get_nth_param(1).unwrap().into_int_value();
        let lhs_raw = self.call_func("__value_to_i64", &[lhs], "lhs_raw");
        let rhs_raw = self.call_func("__value_to_i64", &[rhs], "rhs_raw");
        let cmp = self
            .builder
            .build_int_compare(pred, lhs_raw, rhs_raw, "cmp")
            .expect("failed to build compare");
        let raw = self
            .builder
            .build_int_z_extend(cmp, self.i64_type, "cmp_i64")
            .expect("failed to extend compare");
        let boxed = self.call_func("__value_int", &[raw], "boxed");
        self.builder
            .build_return(Some(&boxed))
            .expect("failed to return compare");
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
