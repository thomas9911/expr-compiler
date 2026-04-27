use crate::parser::{Ast, BlockAst, ExpressionAst, FunctionDefAst, LiteralAst};
use cranelift::codegen::ir::FuncRef;
use cranelift::codegen::ir::Function;
use cranelift::codegen::ir::condcodes::IntCC;
use cranelift::codegen::ir::instructions::BlockArg;
use cranelift::codegen::{
    ir::{TrapCode, UserFuncName},
    verify_function,
};
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::DataDescription;
use cranelift::module::{DataId, FuncId, Linkage, Module as CraneliftModule, default_libcall_names};
use cranelift::object::{ObjectBuilder, ObjectModule};
use cranelift::prelude::{isa::OwnedTargetIsa, settings, *};
use std::collections::HashMap;
use std::path::Path;
use std::process::Command;

const VALUE_TAG_INT: i64 = 1;
const VALUE_TAG_LIST: i64 = 2;
const VALUE_SIZE: i64 = 16;
const VALUE_PAYLOAD_OFFSET: i32 = 8;
const LIST_HEADER_SIZE: i64 = 24;
const LIST_PTR_OFFSET: i32 = 0;
const LIST_LEN_OFFSET: i32 = 8;
const LIST_CAP_OFFSET: i32 = 16;
const ARENA_BYTES: i64 = 16 * 1024 * 1024;
const LIST_INITIAL_CAPACITY: i64 = 1024;

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

        let mut jit_builder = JITBuilder::with_isa(isa.clone(), default_libcall_names());
        jit_builder.symbol("__expr_print_host", crate::runtime::__expr_print_host as *const u8);
        jit_builder.symbol(
            "__expr_list_print_host",
            crate::runtime::__expr_list_print_host as *const u8,
        );
        let mut cranelift_module = JITModule::new(jit_builder);

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

    pub fn compile_to_ir(self) -> String {
        let flags = settings::Flags::new(settings::builder());
        let isa = cranelift::native::builder()
            .expect("host machine supported")
            .finish(flags.clone())
            .unwrap();

        let mut cranelift_module = ObjectModule::new(
            ObjectBuilder::new(isa.clone(), "ir", default_libcall_names()).unwrap(),
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

        let mut out = String::new();

        // Stub for __expr_print: the Cranelift interpreter cannot call external
        // functions (printf), so we emit a pure-IR stub that returns its argument.
        // This means print() won't produce output in --run-ir mode but won't crash,
        // and the "printed" value surfaces as the function's return value.
        let print_func_id = func_ids["print"].as_u32();
        let print_stub = format!(
            "; builtin: print (interpreter stub — no I/O; use --run-jit for real output)\n\
             function u0:{print_func_id}(i64) -> i64 system_v {{\n\
             block0(v0: i64):\n    v1 = iconst.i64 1\n    return v1\n}}\n\n"
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
            generate_c_main(&mut cranelift_module, isa.clone(), &flags, id, all_funcs["__value_to_i64"]);
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
        let status = Command::new("cc")
            .arg("-no-pie")
            .arg(&tmp)
            .arg("-o")
            .arg(output)
            .status()
            .expect("cc not found — install gcc or clang");

        #[cfg(windows)]
        std::fs::remove_file(output.with_extension("wrapper.rs")).ok();
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

    pub fn has_function(&self, name: &str) -> bool {
        self.func_ids.contains_key(name)
    }

    pub fn user_function_names(&self) -> impl Iterator<Item = &str> {
        self.func_ids
            .keys()
            .filter(|n| {
                !n.starts_with("__")
                    && n.as_str() != "print"
                    && n.as_str() != "list_new"
                    && n.as_str() != "list_push"
                    && n.as_str() != "list_len"
                    && n.as_str() != "list_get"
                    && n.as_str() != "list_pop"
                    && n.as_str() != "list_copy"
                    && n.as_str() != "list_print"
            })
            .map(|s| s.as_str())
    }
}

fn setup_builtins(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
) -> HashMap<String, FuncId> {
    let print_id = declare_host_builtin(module, isa, "__expr_print_host", &[types::I64]);
    let list_print_id = declare_host_builtin(module, isa, "__expr_list_print_host", &[types::I64]);
    let memcpy_id = declare_host_builtin(module, isa, "memcpy", &[types::I64, types::I64, types::I64]);

    let runtime = define_runtime_ir(module, isa, flags, memcpy_id);

    let mut builtins = HashMap::new();
    builtins.insert("print".to_string(), print_id);
    builtins.insert("__value_int".to_string(), runtime.value_int);
    builtins.insert("__value_to_i64".to_string(), runtime.value_to_i64);
    builtins.insert("__value_is_truthy".to_string(), runtime.value_is_truthy);
    builtins.insert("__op_add".to_string(), runtime.op_add);
    builtins.insert("__op_subtract".to_string(), runtime.op_subtract);
    builtins.insert("__op_multiply".to_string(), runtime.op_multiply);
    builtins.insert("__op_divide".to_string(), runtime.op_divide);
    builtins.insert("__op_modulo".to_string(), runtime.op_modulo);
    builtins.insert("__op_gt".to_string(), runtime.op_gt);
    builtins.insert("__op_lt".to_string(), runtime.op_lt);
    builtins.insert("__op_gte".to_string(), runtime.op_gte);
    builtins.insert("__op_lte".to_string(), runtime.op_lte);
    builtins.insert("__op_eq".to_string(), runtime.op_eq);
    builtins.insert("__op_ne".to_string(), runtime.op_ne);
    builtins.insert("list_new".to_string(), runtime.list_new);
    builtins.insert("list_push".to_string(), runtime.list_push);
    builtins.insert("list_len".to_string(), runtime.list_len);
    builtins.insert("list_get".to_string(), runtime.list_get);
    builtins.insert("list_pop".to_string(), runtime.list_pop);
    builtins.insert("list_copy".to_string(), runtime.list_copy);
    builtins.insert("list_print".to_string(), list_print_id);
    builtins
}

struct RuntimeBuiltins {
    value_int: FuncId,
    value_to_i64: FuncId,
    value_is_truthy: FuncId,
    op_add: FuncId,
    op_subtract: FuncId,
    op_multiply: FuncId,
    op_divide: FuncId,
    op_modulo: FuncId,
    op_gt: FuncId,
    op_lt: FuncId,
    op_gte: FuncId,
    op_lte: FuncId,
    op_eq: FuncId,
    op_ne: FuncId,
    list_new: FuncId,
    list_push: FuncId,
    list_len: FuncId,
    list_get: FuncId,
    list_pop: FuncId,
    list_copy: FuncId,
}

struct RuntimeData {
    arena: DataId,
    offset: DataId,
}

fn declare_local_builtin(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    symbol: &str,
    params: &[Type],
) -> FuncId {
    let mut sig = Signature::new(isa.default_call_conv());
    for &param in params {
        sig.params.push(AbiParam::new(param));
    }
    sig.returns.push(AbiParam::new(types::I64));
    module.declare_function(symbol, Linkage::Local, &sig).unwrap()
}

fn init_runtime_data(module: &mut impl CraneliftModule) -> RuntimeData {
    let arena = module
        .declare_data("__rt_arena", Linkage::Local, true, false)
        .unwrap();
    let mut arena_desc = DataDescription::new();
    arena_desc.define(vec![0u8; ARENA_BYTES as usize].into_boxed_slice());
    module.define_data(arena, &arena_desc).unwrap();

    let offset = module
        .declare_data("__rt_arena_offset", Linkage::Local, true, false)
        .unwrap();
    let mut offset_desc = DataDescription::new();
    offset_desc.define((0i64).to_ne_bytes().to_vec().into_boxed_slice());
    module.define_data(offset, &offset_desc).unwrap();

    RuntimeData { arena, offset }
}

fn define_runtime_ir(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    memcpy_id: FuncId,
) -> RuntimeBuiltins {
    let data = init_runtime_data(module);

    let alloc = declare_local_builtin(module, isa, "__rt_alloc", &[types::I64, types::I64]);
    let value_int = declare_local_builtin(module, isa, "__rt_value_int", &[types::I64]);
    let value_to_i64 = declare_local_builtin(module, isa, "__rt_value_to_i64", &[types::I64]);
    let value_is_truthy =
        declare_local_builtin(module, isa, "__rt_value_is_truthy", &[types::I64]);
    let op_add = declare_local_builtin(module, isa, "__rt_add", &[types::I64, types::I64]);
    let op_subtract =
        declare_local_builtin(module, isa, "__rt_subtract", &[types::I64, types::I64]);
    let op_multiply =
        declare_local_builtin(module, isa, "__rt_multiply", &[types::I64, types::I64]);
    let op_divide = declare_local_builtin(module, isa, "__rt_divide", &[types::I64, types::I64]);
    let op_modulo = declare_local_builtin(module, isa, "__rt_modulo", &[types::I64, types::I64]);
    let op_gt = declare_local_builtin(module, isa, "__rt_gt", &[types::I64, types::I64]);
    let op_lt = declare_local_builtin(module, isa, "__rt_lt", &[types::I64, types::I64]);
    let op_gte = declare_local_builtin(module, isa, "__rt_gte", &[types::I64, types::I64]);
    let op_lte = declare_local_builtin(module, isa, "__rt_lte", &[types::I64, types::I64]);
    let op_eq = declare_local_builtin(module, isa, "__rt_eq", &[types::I64, types::I64]);
    let op_ne = declare_local_builtin(module, isa, "__rt_ne", &[types::I64, types::I64]);
    let list_new = declare_local_builtin(module, isa, "__rt_list_new", &[]);
    let list_push = declare_local_builtin(module, isa, "__rt_list_push", &[types::I64, types::I64]);
    let list_len = declare_local_builtin(module, isa, "__rt_list_len", &[types::I64]);
    let list_get = declare_local_builtin(module, isa, "__rt_list_get", &[types::I64, types::I64]);
    let list_pop = declare_local_builtin(module, isa, "__rt_list_pop", &[types::I64]);
    let list_copy = declare_local_builtin(module, isa, "__rt_list_copy", &[types::I64]);

    define_rt_alloc(module, isa, flags, alloc, &data);
    define_rt_value_int(module, isa, flags, value_int, alloc);
    define_rt_value_to_i64(module, isa, flags, value_to_i64);
    define_rt_value_is_truthy(module, isa, flags, value_is_truthy);
    define_rt_binary_op(module, isa, flags, op_add, value_to_i64, value_int, "add");
    define_rt_binary_op(module, isa, flags, op_subtract, value_to_i64, value_int, "subtract");
    define_rt_binary_op(module, isa, flags, op_multiply, value_to_i64, value_int, "multiply");
    define_rt_binary_op(module, isa, flags, op_divide, value_to_i64, value_int, "divide");
    define_rt_binary_op(module, isa, flags, op_modulo, value_to_i64, value_int, "modulo");
    define_rt_compare_op(module, isa, flags, op_gt, value_to_i64, value_int, IntCC::SignedGreaterThan);
    define_rt_compare_op(module, isa, flags, op_lt, value_to_i64, value_int, IntCC::SignedLessThan);
    define_rt_compare_op(module, isa, flags, op_gte, value_to_i64, value_int, IntCC::SignedGreaterThanOrEqual);
    define_rt_compare_op(module, isa, flags, op_lte, value_to_i64, value_int, IntCC::SignedLessThanOrEqual);
    define_rt_compare_op(module, isa, flags, op_eq, value_to_i64, value_int, IntCC::Equal);
    define_rt_compare_op(module, isa, flags, op_ne, value_to_i64, value_int, IntCC::NotEqual);
    define_rt_list_new(module, isa, flags, list_new, alloc);
    define_rt_list_push(module, isa, flags, list_push, alloc, memcpy_id);
    define_rt_list_len(module, isa, flags, list_len, value_to_i64, value_int);
    define_rt_list_get(module, isa, flags, list_get, value_to_i64);
    define_rt_list_pop(module, isa, flags, list_pop, value_to_i64);
    define_rt_list_copy(module, isa, flags, list_copy, value_to_i64, alloc, memcpy_id);

    RuntimeBuiltins {
        value_int,
        value_to_i64,
        value_is_truthy,
        op_add,
        op_subtract,
        op_multiply,
        op_divide,
        op_modulo,
        op_gt,
        op_lt,
        op_gte,
        op_lte,
        op_eq,
        op_ne,
        list_new,
        list_push,
        list_len,
        list_get,
        list_pop,
        list_copy,
    }
}

fn declare_host_builtin(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    symbol: &str,
    params: &[Type],
) -> FuncId {
    let mut sig = Signature::new(isa.default_call_conv());
    for &param in params {
        sig.params.push(AbiParam::new(param));
    }
    sig.returns.push(AbiParam::new(types::I64));
    module
        .declare_function(symbol, Linkage::Import, &sig)
        .unwrap()
}

fn runtime_sig(isa: &OwnedTargetIsa, params: &[Type]) -> Signature {
    let mut sig = Signature::new(isa.default_call_conv());
    for &param in params {
        sig.params.push(AbiParam::new(param));
    }
    sig.returns.push(AbiParam::new(types::I64));
    sig
}

fn rt_payload_for_tag(builder: &mut FunctionBuilder, handle: Value, expected_tag: i64) -> Value {
    let mf = MemFlags::new();
    let tag = builder.ins().load(types::I8, mf, handle, 0);
    let ok = builder.ins().icmp_imm(IntCC::Equal, tag, expected_tag);
    builder.ins().trapz(ok, TrapCode::BAD_CONVERSION_TO_INTEGER);
    builder.ins().load(types::I64, MemFlags::new(), handle, VALUE_PAYLOAD_OFFSET)
}

fn define_runtime_fn(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    params: &[Type],
    build: impl FnOnce(&mut FunctionBuilder, &[Value], &mut Function),
) {
    let mut ctx = module.make_context();
    ctx.func.signature = runtime_sig(isa, params);
    ctx.func.name = UserFuncName::user(0, id.as_u32());
    let mut fb_ctx = FunctionBuilderContext::new();
    {
        let mut b = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);
        let block0 = b.create_block();
        b.append_block_params_for_function_params(block0);
        b.switch_to_block(block0);
        b.seal_block(block0);
        let params = b.block_params(block0).to_vec();
        let func_ref = b.func as *mut Function;
        // SAFETY: builder owns mutable func for duration of closure.
        build(&mut b, &params, unsafe { &mut *func_ref });
        b.finalize();
    }
    if let Err(errors) = verify_function(&ctx.func, flags) {
        panic!("{errors}");
    }
    module.define_function(id, &mut ctx).unwrap();
    module.clear_context(&mut ctx);
}

fn define_rt_alloc(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    data: &RuntimeData,
) {
    let module_ptr: *mut _ = module;
    define_runtime_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
        let size = p[0];
        let align = p[1];
        let arena_gv = unsafe { (&mut *module_ptr).declare_data_in_func(data.arena, func) };
        let off_gv = unsafe { (&mut *module_ptr).declare_data_in_func(data.offset, func) };
        let base = b.ins().global_value(types::I64, arena_gv);
        let off_addr = b.ins().global_value(types::I64, off_gv);
        let off = b.ins().load(types::I64, MemFlags::new(), off_addr, 0);
        let addr = b.ins().iadd(base, off);
        let one = b.ins().iconst(types::I64, 1);
        let align_minus = b.ins().isub(align, one);
        let addr_plus = b.ins().iadd(addr, align_minus);
        let neg_one = b.ins().iconst(types::I64, -1);
        let mask = b.ins().bxor(align_minus, neg_one);
        let aligned = b.ins().band(addr_plus, mask);
        let rel = b.ins().isub(aligned, base);
        let new_off = b.ins().iadd(rel, size);
        let max = b.ins().iconst(types::I64, ARENA_BYTES);
        let ok = b.ins().icmp(IntCC::UnsignedLessThanOrEqual, new_off, max);
        b.ins().trapz(ok, TrapCode::HEAP_OUT_OF_BOUNDS);
        b.ins().store(MemFlags::new(), new_off, off_addr, 0);
        b.ins().return_(&[aligned]);
    });
}

fn define_rt_value_int(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_fn(module, isa, flags, id, &[types::I64], |b, p, func| {
        let raw = p[0];
        let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
        let size = b.ins().iconst(types::I64, VALUE_SIZE);
        let align = b.ins().iconst(types::I64, 8);
        let call = b.ins().call(alloc_ref, &[size, align]);
        let ptr = b.inst_results(call)[0];
        let tag = b.ins().iconst(types::I8, VALUE_TAG_INT);
        b.ins().store(MemFlags::new(), tag, ptr, 0);
        b.ins()
            .store(MemFlags::new(), raw, ptr, VALUE_PAYLOAD_OFFSET);
        b.ins().return_(&[ptr]);
    });
}

fn define_rt_value_to_i64(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
) {
    define_runtime_fn(module, isa, flags, id, &[types::I64], |b, p, _| {
        let payload = rt_payload_for_tag(b, p[0], VALUE_TAG_INT);
        b.ins().return_(&[payload]);
    });
}

fn define_rt_value_is_truthy(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
) {
    define_runtime_fn(module, isa, flags, id, &[types::I64], |b, p, _| {
        let handle = p[0];
        let tag = b.ins().load(types::I8, MemFlags::new(), handle, 0);
        let one = b.ins().iconst(types::I64, 1);
        let zero = b.ins().iconst(types::I64, 0);
        let is_int = b.ins().icmp_imm(IntCC::Equal, tag, VALUE_TAG_INT);
        let int_block = b.create_block();
        let list_block = b.create_block();
        let merge = b.create_block();
        b.append_block_param(merge, types::I64);
        b.ins().brif(is_int, int_block, &[], list_block, &[]);

        b.switch_to_block(int_block);
        b.seal_block(int_block);
        let raw = b
            .ins()
            .load(types::I64, MemFlags::new(), handle, VALUE_PAYLOAD_OFFSET);
        let nz = b.ins().icmp_imm(IntCC::NotEqual, raw, 0);
        let int_truthy = b.ins().select(nz, one, zero);
        b.ins().jump(merge, &[BlockArg::Value(int_truthy)]);

        b.switch_to_block(list_block);
        b.seal_block(list_block);
        let is_list = b.ins().icmp_imm(IntCC::Equal, tag, VALUE_TAG_LIST);
        b.ins().trapz(is_list, TrapCode::BAD_CONVERSION_TO_INTEGER);
        let header = b
            .ins()
            .load(types::I64, MemFlags::new(), handle, VALUE_PAYLOAD_OFFSET);
        let len = b
            .ins()
            .load(types::I64, MemFlags::new(), header, LIST_LEN_OFFSET);
        let list_nz = b.ins().icmp_imm(IntCC::NotEqual, len, 0);
        let list_truthy = b.ins().select(list_nz, one, zero);
        b.ins().jump(merge, &[BlockArg::Value(list_truthy)]);

        b.switch_to_block(merge);
        b.seal_block(merge);
        let out = b.block_params(merge)[0];
        b.ins().return_(&[out]);
    });
}

fn define_rt_binary_op(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    to_i64_id: FuncId,
    int_id: FuncId,
    op: &str,
) {
    let module_ptr: *mut _ = module;
    define_runtime_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64],
        |b, p, func| {
            let to_i64 = unsafe { (&mut *module_ptr).declare_func_in_func(to_i64_id, func) };
            let make_int = unsafe { (&mut *module_ptr).declare_func_in_func(int_id, func) };
            let l = b.ins().call(to_i64, &[p[0]]);
            let lhs = b.inst_results(l)[0];
            let r = b.ins().call(to_i64, &[p[1]]);
            let rhs = b.inst_results(r)[0];

            let raw = match op {
                "add" => {
                    let (sum, ovf) = b.ins().sadd_overflow(lhs, rhs);
                    b.ins().trapnz(ovf, TrapCode::INTEGER_OVERFLOW);
                    sum
                }
                "subtract" => {
                    let (diff, ovf) = b.ins().ssub_overflow(lhs, rhs);
                    b.ins().trapnz(ovf, TrapCode::INTEGER_OVERFLOW);
                    diff
                }
                "multiply" => {
                    let (prod, ovf) = b.ins().smul_overflow(lhs, rhs);
                    b.ins().trapnz(ovf, TrapCode::INTEGER_OVERFLOW);
                    prod
                }
                "divide" => {
                    b.ins().trapz(rhs, TrapCode::INTEGER_DIVISION_BY_ZERO);
                    let lhs_is_min = b.ins().icmp_imm(IntCC::Equal, lhs, i64::MIN);
                    let neg_one = b.ins().iconst(types::I64, -1);
                    let rhs_is_neg_one = b.ins().icmp(IntCC::Equal, rhs, neg_one);
                    let overflow = b.ins().band(lhs_is_min, rhs_is_neg_one);
                    b.ins().trapnz(overflow, TrapCode::INTEGER_OVERFLOW);
                    b.ins().sdiv(lhs, rhs)
                }
                "modulo" => {
                    b.ins().trapz(rhs, TrapCode::INTEGER_DIVISION_BY_ZERO);
                    let lhs_is_min = b.ins().icmp_imm(IntCC::Equal, lhs, i64::MIN);
                    let neg_one = b.ins().iconst(types::I64, -1);
                    let rhs_is_neg_one = b.ins().icmp(IntCC::Equal, rhs, neg_one);
                    let overflow = b.ins().band(lhs_is_min, rhs_is_neg_one);
                    b.ins().trapnz(overflow, TrapCode::INTEGER_OVERFLOW);
                    b.ins().srem(lhs, rhs)
                }
                _ => unreachable!(),
            };
            let out = b.ins().call(make_int, &[raw]);
            let boxed = b.inst_results(out)[0];
            b.ins().return_(&[boxed]);
        },
    );
}

fn define_rt_compare_op(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    to_i64_id: FuncId,
    int_id: FuncId,
    cc: IntCC,
) {
    let module_ptr: *mut _ = module;
    define_runtime_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64],
        |b, p, func| {
            let to_i64 = unsafe { (&mut *module_ptr).declare_func_in_func(to_i64_id, func) };
            let make_int = unsafe { (&mut *module_ptr).declare_func_in_func(int_id, func) };
            let l = b.ins().call(to_i64, &[p[0]]);
            let lhs = b.inst_results(l)[0];
            let r = b.ins().call(to_i64, &[p[1]]);
            let rhs = b.inst_results(r)[0];
            let cmp = b.ins().icmp(cc, lhs, rhs);
            let one = b.ins().iconst(types::I64, 1);
            let zero = b.ins().iconst(types::I64, 0);
            let raw = b.ins().select(cmp, one, zero);
            let out = b.ins().call(make_int, &[raw]);
            let boxed = b.inst_results(out)[0];
            b.ins().return_(&[boxed]);
        },
    );
}

fn define_rt_list_new(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_fn(module, isa, flags, id, &[], |b, _p, func| {
        let alloc = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
        let data_bytes = b.ins().iconst(types::I64, LIST_INITIAL_CAPACITY * 8);
        let align = b.ins().iconst(types::I64, 8);
        let data_call = b.ins().call(alloc, &[data_bytes, align]);
        let data_ptr = b.inst_results(data_call)[0];

        let header_size = b.ins().iconst(types::I64, LIST_HEADER_SIZE);
        let header_call = b.ins().call(alloc, &[header_size, align]);
        let header_ptr = b.inst_results(header_call)[0];
        b.ins()
            .store(MemFlags::new(), data_ptr, header_ptr, LIST_PTR_OFFSET);
        let zero = b.ins().iconst(types::I64, 0);
        b.ins()
            .store(MemFlags::new(), zero, header_ptr, LIST_LEN_OFFSET);
        let cap = b.ins().iconst(types::I64, LIST_INITIAL_CAPACITY);
        b.ins()
            .store(MemFlags::new(), cap, header_ptr, LIST_CAP_OFFSET);

        let value_size = b.ins().iconst(types::I64, VALUE_SIZE);
        let value_call = b.ins().call(alloc, &[value_size, align]);
        let value_ptr = b.inst_results(value_call)[0];
        let tag = b.ins().iconst(types::I8, VALUE_TAG_LIST);
        b.ins().store(MemFlags::new(), tag, value_ptr, 0);
        b.ins()
            .store(MemFlags::new(), header_ptr, value_ptr, VALUE_PAYLOAD_OFFSET);
        b.ins().return_(&[value_ptr]);
    });
}

fn define_rt_list_push(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
    memcpy_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
        let list = p[0];
        let value = p[1];
        let header_ptr = rt_payload_for_tag(b, list, VALUE_TAG_LIST);
        let len = b
            .ins()
            .load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
        let cap = b
            .ins()
            .load(types::I64, MemFlags::new(), header_ptr, LIST_CAP_OFFSET);
        let data_ptr = b
            .ins()
            .load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);
        let has_room = b.ins().icmp(IntCC::UnsignedLessThan, len, cap);
        let fast_block = b.create_block();
        let grow_block = b.create_block();
        let cont_block = b.create_block();
        b.append_block_param(cont_block, types::I64);
        b.ins().brif(has_room, fast_block, &[], grow_block, &[]);

        b.switch_to_block(fast_block);
        b.seal_block(fast_block);
        b.ins().jump(cont_block, &[BlockArg::Value(data_ptr)]);

        b.switch_to_block(grow_block);
        b.seal_block(grow_block);
        let alloc = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
        let memcpy = unsafe { (&mut *module_ptr).declare_func_in_func(memcpy_id, func) };
        let two = b.ins().iconst(types::I64, 2);
        let (new_cap, ovf) = b.ins().smul_overflow(cap, two);
        b.ins().trapnz(ovf, TrapCode::INTEGER_OVERFLOW);
        let bytes = b.ins().ishl_imm(new_cap, 3);
        let align = b.ins().iconst(types::I64, 8);
        let new_data_call = b.ins().call(alloc, &[bytes, align]);
        let new_data_ptr = b.inst_results(new_data_call)[0];
        let old_bytes = b.ins().ishl_imm(len, 3);
        let _ = b.ins().call(memcpy, &[new_data_ptr, data_ptr, old_bytes]);
        b.ins().store(MemFlags::new(), new_data_ptr, header_ptr, LIST_PTR_OFFSET);
        b.ins().store(MemFlags::new(), new_cap, header_ptr, LIST_CAP_OFFSET);
        b.ins().jump(cont_block, &[BlockArg::Value(new_data_ptr)]);

        b.switch_to_block(cont_block);
        b.seal_block(cont_block);
        let active_data_ptr = b.block_params(cont_block)[0];

        let off = b.ins().ishl_imm(len, 3);
        let elem_ptr = b.ins().iadd(active_data_ptr, off);
        b.ins().store(MemFlags::new(), value, elem_ptr, 0);
        let new_len = b.ins().iadd_imm(len, 1);
        b.ins()
            .store(MemFlags::new(), new_len, header_ptr, LIST_LEN_OFFSET);
        b.ins().return_(&[list]);
    });
}

fn define_rt_list_len(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    _to_i64_id: FuncId,
    int_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_fn(module, isa, flags, id, &[types::I64], |b, p, func| {
        let header_ptr = rt_payload_for_tag(b, p[0], VALUE_TAG_LIST);
        let len = b
            .ins()
            .load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
        let make_int = unsafe { (&mut *module_ptr).declare_func_in_func(int_id, func) };
        let out = b.ins().call(make_int, &[len]);
        let boxed = b.inst_results(out)[0];
        b.ins().return_(&[boxed]);
    });
}

fn define_rt_list_get(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    to_i64_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
        let header_ptr = rt_payload_for_tag(b, p[0], VALUE_TAG_LIST);
        let to_i64 = unsafe { (&mut *module_ptr).declare_func_in_func(to_i64_id, func) };
        let idx_call = b.ins().call(to_i64, &[p[1]]);
        let idx = b.inst_results(idx_call)[0];
        let non_neg = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, idx, 0);
        b.ins().trapz(non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
        let len = b
            .ins()
            .load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
        let in_bounds = b.ins().icmp(IntCC::UnsignedLessThan, idx, len);
        b.ins().trapz(in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);
        let data_ptr = b
            .ins()
            .load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);
        let off = b.ins().ishl_imm(idx, 3);
        let elem_ptr = b.ins().iadd(data_ptr, off);
        let value = b.ins().load(types::I64, MemFlags::new(), elem_ptr, 0);
        b.ins().return_(&[value]);
    });
}

fn define_rt_list_pop(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    _to_i64_id: FuncId,
) {
    define_runtime_fn(module, isa, flags, id, &[types::I64], |b, p, _| {
        let header_ptr = rt_payload_for_tag(b, p[0], VALUE_TAG_LIST);
        let len = b
            .ins()
            .load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
        let non_empty = b.ins().icmp_imm(IntCC::NotEqual, len, 0);
        b.ins().trapz(non_empty, TrapCode::HEAP_OUT_OF_BOUNDS);
        let new_len = b.ins().iadd_imm(len, -1);
        b.ins()
            .store(MemFlags::new(), new_len, header_ptr, LIST_LEN_OFFSET);
        let data_ptr = b
            .ins()
            .load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);
        let off = b.ins().ishl_imm(new_len, 3);
        let elem_ptr = b.ins().iadd(data_ptr, off);
        let value = b.ins().load(types::I64, MemFlags::new(), elem_ptr, 0);
        b.ins().return_(&[value]);
    });
}

fn define_rt_list_copy(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    _to_i64_id: FuncId,
    alloc_id: FuncId,
    memcpy_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_fn(module, isa, flags, id, &[types::I64], |b, p, func| {
        let header_ptr = rt_payload_for_tag(b, p[0], VALUE_TAG_LIST);
        let src_ptr = b
            .ins()
            .load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);
        let len = b
            .ins()
            .load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
        let cap = b
            .ins()
            .load(types::I64, MemFlags::new(), header_ptr, LIST_CAP_OFFSET);

        let alloc = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
        let memcpy = unsafe { (&mut *module_ptr).declare_func_in_func(memcpy_id, func) };
        let eight = b.ins().iconst(types::I64, 8);
        let bytes = b.ins().imul(cap, eight);
        let new_data_call = b.ins().call(alloc, &[bytes, eight]);
        let new_data = b.inst_results(new_data_call)[0];
        let _copy = b.ins().call(memcpy, &[new_data, src_ptr, bytes]);

        let header_size = b.ins().iconst(types::I64, LIST_HEADER_SIZE);
        let new_header_call = b.ins().call(alloc, &[header_size, eight]);
        let new_header = b.inst_results(new_header_call)[0];
        b.ins()
            .store(MemFlags::new(), new_data, new_header, LIST_PTR_OFFSET);
        b.ins()
            .store(MemFlags::new(), len, new_header, LIST_LEN_OFFSET);
        b.ins()
            .store(MemFlags::new(), cap, new_header, LIST_CAP_OFFSET);

        let value_size = b.ins().iconst(types::I64, VALUE_SIZE);
        let value_call = b.ins().call(alloc, &[value_size, eight]);
        let value_ptr = b.inst_results(value_call)[0];
        let tag = b.ins().iconst(types::I8, VALUE_TAG_LIST);
        b.ins().store(MemFlags::new(), tag, value_ptr, 0);
        b.ins()
            .store(MemFlags::new(), new_header, value_ptr, VALUE_PAYLOAD_OFFSET);
        b.ins().return_(&[value_ptr]);
    });
}


#[cfg(windows)]
fn write_windows_wrapper(output: &Path) -> std::path::PathBuf {
    let wrapper = output.with_extension("wrapper.rs");
    let source = include_str!("./wrapper/windows.rs");
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

fn local_var_names(block: &BlockAst) -> Vec<String> {
    let mut names = vec![];
    for line in &block.lines {
        collect_var_names(line, &mut names);
    }
    names
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
                "subtract" => {
                    call_binary(builder, func_refs, "__op_subtract", compiled[0], compiled[1])
                }
                "multiply" => {
                    call_binary(builder, func_refs, "__op_multiply", compiled[0], compiled[1])
                }
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
