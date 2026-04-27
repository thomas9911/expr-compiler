use cranelift::codegen::ir::Function;
use cranelift::codegen::ir::condcodes::IntCC;
use cranelift::codegen::ir::instructions::BlockArg;
use cranelift::codegen::{
    ir::{TrapCode, UserFuncName},
    verify_function,
};
use cranelift::module::DataDescription;
use cranelift::module::{DataId, FuncId, Linkage, Module as CraneliftModule};
use cranelift::prelude::{isa::OwnedTargetIsa, settings, *};
use std::collections::HashMap;

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

pub(super) fn setup_builtins(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
) -> HashMap<String, FuncId> {
    let print_id = declare_host_builtin(module, isa, "__expr_print_host", &[types::I64]);
    let list_print_id = declare_host_builtin(module, isa, "__expr_list_print_host", &[types::I64]);
    let runtime = define_runtime_ir(module, isa, flags);
    build_builtin_map(print_id, list_print_id, runtime)
}

pub(super) fn setup_builtins_jit(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    print_host_addr: i64,
    list_print_host_addr: i64,
    arena_base_addr: i64,
    arena_offset_addr: i64,
) -> HashMap<String, FuncId> {
    let print_id = declare_local_builtin(module, isa, "__rt_print", &[types::I64]);
    let list_print_id = declare_local_builtin(module, isa, "__rt_list_print", &[types::I64]);
    define_rt_host_print_shim(module, isa, flags, print_id, print_host_addr);
    define_rt_host_print_shim(module, isa, flags, list_print_id, list_print_host_addr);

    let runtime = define_runtime_ir_jit(module, isa, flags, arena_base_addr, arena_offset_addr);
    build_builtin_map(print_id, list_print_id, runtime)
}

fn build_builtin_map(
    print_id: FuncId,
    list_print_id: FuncId,
    runtime: RuntimeBuiltins,
) -> HashMap<String, FuncId> {
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

struct RuntimeFunctionIds {
    alloc: FuncId,
    memcpy: FuncId,
    builtins: RuntimeBuiltins,
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
    module
        .declare_function(symbol, Linkage::Local, &sig)
        .unwrap()
}

fn init_runtime_data(module: &mut impl CraneliftModule) -> RuntimeData {
    let arena = module
        .declare_data("__rt_arena", Linkage::Local, true, false)
        .unwrap();
    let mut arena_desc = DataDescription::new();
    arena_desc.define_zeroinit(ARENA_BYTES as usize);
    module.define_data(arena, &arena_desc).unwrap();

    let offset = module
        .declare_data("__rt_arena_offset", Linkage::Local, true, false)
        .unwrap();
    let mut offset_desc = DataDescription::new();
    offset_desc.define((0i64).to_ne_bytes().to_vec().into_boxed_slice());
    module.define_data(offset, &offset_desc).unwrap();

    RuntimeData { arena, offset }
}

fn declare_runtime_function_ids(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
) -> RuntimeFunctionIds {
    let alloc = declare_local_builtin(module, isa, "__rt_alloc", &[types::I64, types::I64]);
    let memcpy = declare_local_builtin(
        module,
        isa,
        "__rt_memcpy",
        &[types::I64, types::I64, types::I64],
    );
    let value_int = declare_local_builtin(module, isa, "__rt_value_int", &[types::I64]);
    let value_to_i64 = declare_local_builtin(module, isa, "__rt_value_to_i64", &[types::I64]);
    let value_is_truthy = declare_local_builtin(module, isa, "__rt_value_is_truthy", &[types::I64]);
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

    RuntimeFunctionIds {
        alloc,
        memcpy,
        builtins: RuntimeBuiltins {
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
        },
    }
}

fn define_runtime_operations(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    ids: &RuntimeFunctionIds,
) {
    define_rt_memcpy(module, isa, flags, ids.memcpy);
    define_rt_value_int(module, isa, flags, ids.builtins.value_int, ids.alloc);
    define_rt_value_to_i64(module, isa, flags, ids.builtins.value_to_i64);
    define_rt_value_is_truthy(module, isa, flags, ids.builtins.value_is_truthy);
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_add,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        "add",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_subtract,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        "subtract",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_multiply,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        "multiply",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_divide,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        "divide",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_modulo,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        "modulo",
    );
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_gt,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        IntCC::SignedGreaterThan,
    );
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_lt,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        IntCC::SignedLessThan,
    );
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_gte,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        IntCC::SignedGreaterThanOrEqual,
    );
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_lte,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        IntCC::SignedLessThanOrEqual,
    );
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_eq,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        IntCC::Equal,
    );
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_ne,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        IntCC::NotEqual,
    );
    define_rt_list_new(module, isa, flags, ids.builtins.list_new, ids.alloc);
    define_rt_list_push(
        module,
        isa,
        flags,
        ids.builtins.list_push,
        ids.alloc,
        ids.memcpy,
    );
    define_rt_list_len(
        module,
        isa,
        flags,
        ids.builtins.list_len,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
    );
    define_rt_list_get(
        module,
        isa,
        flags,
        ids.builtins.list_get,
        ids.builtins.value_to_i64,
    );
    define_rt_list_pop(
        module,
        isa,
        flags,
        ids.builtins.list_pop,
        ids.builtins.value_to_i64,
    );
    define_rt_list_copy(
        module,
        isa,
        flags,
        ids.builtins.list_copy,
        ids.builtins.value_to_i64,
        ids.alloc,
        ids.memcpy,
    );
}

fn define_runtime_ir(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
) -> RuntimeBuiltins {
    let data = init_runtime_data(module);
    let ids = declare_runtime_function_ids(module, isa);
    define_rt_alloc(module, isa, flags, ids.alloc, &data);
    define_runtime_operations(module, isa, flags, &ids);
    ids.builtins
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
    builder
        .ins()
        .load(types::I64, MemFlags::new(), handle, VALUE_PAYLOAD_OFFSET)
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

fn define_runtime_ir_jit(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    arena_base_addr: i64,
    arena_offset_addr: i64,
) -> RuntimeBuiltins {
    let ids = declare_runtime_function_ids(module, isa);
    define_rt_alloc_from_addrs(
        module,
        isa,
        flags,
        ids.alloc,
        arena_base_addr,
        arena_offset_addr,
    );
    define_runtime_operations(module, isa, flags, &ids);
    ids.builtins
}

fn define_rt_host_print_shim(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    host_addr: i64,
) {
    define_runtime_fn(module, isa, flags, id, &[types::I64], |b, p, func| {
        let sig_ref = func.import_signature(runtime_sig(isa, &[types::I64]));
        let callee = b.ins().iconst(types::I64, host_addr);
        let call = b.ins().call_indirect(sig_ref, callee, &[p[0]]);
        let out = b.inst_results(call)[0];
        b.ins().return_(&[out]);
    });
}

fn define_rt_memcpy(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
) {
    define_runtime_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64],
        |b, p, _| {
            let dst = p[0];
            let src = p[1];
            let len = p[2];
            let idx_block = b.create_block();
            let body_block = b.create_block();
            let done_block = b.create_block();
            b.append_block_param(idx_block, types::I64);

            let zero = b.ins().iconst(types::I64, 0);
            b.ins().jump(idx_block, &[BlockArg::Value(zero)]);

            b.switch_to_block(idx_block);
            let i = b.block_params(idx_block)[0];
            let more = b.ins().icmp(IntCC::UnsignedLessThan, i, len);
            b.ins().brif(more, body_block, &[], done_block, &[]);

            b.switch_to_block(body_block);
            b.seal_block(body_block);
            let src_i = b.ins().iadd(src, i);
            let dst_i = b.ins().iadd(dst, i);
            let byte = b.ins().load(types::I8, MemFlags::new(), src_i, 0);
            b.ins().store(MemFlags::new(), byte, dst_i, 0);
            let next = b.ins().iadd_imm(i, 1);
            b.ins().jump(idx_block, &[BlockArg::Value(next)]);

            b.switch_to_block(done_block);
            b.seal_block(done_block);
            b.seal_block(idx_block);
            b.ins().return_(&[dst]);
        },
    );
}

fn define_rt_alloc(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    data: &RuntimeData,
) {
    let module_ptr: *mut _ = module;
    define_runtime_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64],
        |b, p, func| {
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
        },
    );
}

fn define_rt_alloc_from_addrs(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    arena_base_addr: i64,
    arena_offset_addr: i64,
) {
    define_runtime_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64],
        |b, p, _| {
            let size = p[0];
            let align = p[1];
            let base = b.ins().iconst(types::I64, arena_base_addr);
            let off_addr = b.ins().iconst(types::I64, arena_offset_addr);
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
        },
    );
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
    define_runtime_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64],
        |b, p, func| {
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
            b.ins()
                .store(MemFlags::new(), new_data_ptr, header_ptr, LIST_PTR_OFFSET);
            b.ins()
                .store(MemFlags::new(), new_cap, header_ptr, LIST_CAP_OFFSET);
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
        },
    );
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
    define_runtime_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64],
        |b, p, func| {
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
        },
    );
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
