use cranelift::codegen::ir::FuncRef;
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

use crate::value::{
    BIGINT_CAP_OFFSET, BIGINT_HEADER_SIZE, BIGINT_LEN_OFFSET, BIGINT_LIMB_SIZE, BIGINT_PTR_OFFSET,
    BIGINT_SIGN_OFFSET, LIST_CAP_OFFSET, LIST_HEADER_SIZE, LIST_LEN_OFFSET, LIST_PTR_OFFSET,
    MAP_CAP_OFFSET, MAP_ENTRY_HASH_OFFSET, MAP_ENTRY_KEY_OFFSET, MAP_ENTRY_OCCUPIED,
    MAP_ENTRY_SIZE, MAP_ENTRY_STATE_OFFSET, MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
    MAP_ENTRY_VALUE_TAG_OFFSET, MAP_HEADER_SIZE, MAP_LEN_OFFSET, MAP_PTR_OFFSET, STRING_LEN_OFFSET,
    STRING_PTR_OFFSET, TAG_BIGINT, TAG_INT, TAG_LIST, TAG_MAP, TAG_STRING, VALUE_PAYLOAD_OFFSET,
    VALUE_SIZE,
};

const ARENA_BYTES: i64 = 16 * 1024 * 1024;
const LIST_INITIAL_CAPACITY: i64 = 1024;

pub(super) fn setup_builtins(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    bigint_enabled: bool,
    list_enabled: bool,
    list_mutation_enabled: bool,
    map_enabled: bool,
) -> HashMap<String, FuncId> {
    let oom_host_id = declare_host_builtin(module, isa, "__expr_runtime_oom_host", &[]);
    let runtime = define_runtime_ir(
        module,
        isa,
        flags,
        oom_host_id,
        bigint_enabled,
        list_enabled,
        list_mutation_enabled,
        map_enabled,
    );
    let print_host_id = declare_host_builtin(module, isa, "__expr_print_host", &[types::I64]);
    let list_print_host_id =
        declare_host_builtin(module, isa, "__expr_list_print_host", &[types::I64]);
    let print_id =
        declare_local_pair_builtin(module, isa, "__rt_print_pair", &[types::I64, types::I64]);
    let list_print_id =
        declare_local_pair_builtin(module, isa, "__rt_list_print_pair", &[types::I64, types::I64]);
    define_rt_pair_print_wrapper(module, isa, flags, print_id, print_host_id, runtime.box_value);
    define_rt_pair_print_wrapper(
        module,
        isa,
        flags,
        list_print_id,
        list_print_host_id,
        runtime.box_value,
    );
    build_builtin_map(
        print_id,
        list_print_id,
        runtime,
        bigint_enabled,
        list_enabled,
        list_mutation_enabled,
        map_enabled,
    )
}

pub(super) fn setup_builtins_jit(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    bigint_enabled: bool,
    list_enabled: bool,
    list_mutation_enabled: bool,
    map_enabled: bool,
    print_host_addr: i64,
    list_print_host_addr: i64,
    arena_base_addr: i64,
    arena_offset_addr: i64,
) -> HashMap<String, FuncId> {
    let runtime = define_runtime_ir_jit(
        module,
        isa,
        flags,
        arena_base_addr,
        arena_offset_addr,
        crate::runtime::__expr_runtime_oom_host as usize as i64,
        bigint_enabled,
        list_enabled,
        list_mutation_enabled,
        map_enabled,
    );
    let print_host_id = declare_local_builtin(module, isa, "__rt_print_host_scalar", &[types::I64]);
    let list_print_host_id =
        declare_local_builtin(module, isa, "__rt_list_print_host_scalar", &[types::I64]);
    define_rt_host_print_shim(module, isa, flags, print_host_id, print_host_addr);
    define_rt_host_print_shim(module, isa, flags, list_print_host_id, list_print_host_addr);
    let print_id =
        declare_local_pair_builtin(module, isa, "__rt_print_pair", &[types::I64, types::I64]);
    let list_print_id =
        declare_local_pair_builtin(module, isa, "__rt_list_print_pair", &[types::I64, types::I64]);
    define_rt_pair_print_wrapper(module, isa, flags, print_id, print_host_id, runtime.box_value);
    define_rt_pair_print_wrapper(
        module,
        isa,
        flags,
        list_print_id,
        list_print_host_id,
        runtime.box_value,
    );
    build_builtin_map(
        print_id,
        list_print_id,
        runtime,
        bigint_enabled,
        list_enabled,
        list_mutation_enabled,
        map_enabled,
    )
}

fn build_builtin_map(
    print_id: FuncId,
    _list_print_id: FuncId,
    runtime: RuntimeBuiltins,
    bigint_enabled: bool,
    list_enabled: bool,
    list_mutation_enabled: bool,
    map_enabled: bool,
) -> HashMap<String, FuncId> {
    let mut builtins = HashMap::new();
    builtins.insert("print".to_string(), print_id);
    builtins.insert("__alloc".to_string(), runtime.alloc);
    builtins.insert("__box_value".to_string(), runtime.box_value);
    builtins.insert("__value_int".to_string(), runtime.value_int);
    builtins.insert("__value_to_i64".to_string(), runtime.value_to_i64);
    builtins.insert("__value_is_truthy".to_string(), runtime.value_is_truthy);
    builtins.insert("__op_add".to_string(), runtime.op_add);
    builtins.insert("__op_subtract".to_string(), runtime.op_subtract);
    builtins.insert("__op_multiply".to_string(), runtime.op_multiply);
    builtins.insert("__op_divide".to_string(), runtime.op_divide);
    builtins.insert("__op_modulo".to_string(), runtime.op_modulo);
    builtins.insert("__op_bitand".to_string(), runtime.op_bitand);
    builtins.insert("__op_bitor".to_string(), runtime.op_bitor);
    builtins.insert("__op_bitxor".to_string(), runtime.op_bitxor);
    builtins.insert("__op_shl".to_string(), runtime.op_shl);
    builtins.insert("__op_shr".to_string(), runtime.op_shr);
    builtins.insert("__op_gt".to_string(), runtime.op_gt);
    builtins.insert("__op_lt".to_string(), runtime.op_lt);
    builtins.insert("__op_gte".to_string(), runtime.op_gte);
    builtins.insert("__op_lte".to_string(), runtime.op_lte);
    builtins.insert("__op_eq".to_string(), runtime.op_eq);
    builtins.insert("__op_ne".to_string(), runtime.op_ne);
    if bigint_enabled {
        builtins.insert("bigint_compare".to_string(), runtime.bigint_compare.unwrap());
        builtins.insert("bigint_from_int".to_string(), runtime.bigint_from_int.unwrap());
        builtins.insert("bigint_add".to_string(), runtime.bigint_add.unwrap());
        builtins.insert("bigint_subtract".to_string(), runtime.bigint_subtract.unwrap());
        builtins.insert("bigint_multiply".to_string(), runtime.bigint_multiply.unwrap());
        builtins.insert("bigint_divide".to_string(), runtime.bigint_divide.unwrap());
        builtins.insert("bigint_modulo".to_string(), runtime.bigint_modulo.unwrap());
        builtins.insert("bigint_bitand".to_string(), runtime.bigint_bitand.unwrap());
        builtins.insert("bigint_bitor".to_string(), runtime.bigint_bitor.unwrap());
        builtins.insert("bigint_bitxor".to_string(), runtime.bigint_bitxor.unwrap());
        builtins.insert("bigint_shl".to_string(), runtime.bigint_shl.unwrap());
        builtins.insert("bigint_shr".to_string(), runtime.bigint_shr.unwrap());
    }
    if list_enabled {
        builtins.insert("list_new".to_string(), runtime.list_new.unwrap());
        builtins.insert("list_push".to_string(), runtime.list_push.unwrap());
        builtins.insert("list_len".to_string(), runtime.list_len.unwrap());
        builtins.insert("list_get".to_string(), runtime.list_get.unwrap());
    }
    if list_mutation_enabled {
        builtins.insert("list_insert".to_string(), runtime.list_insert.unwrap());
        builtins.insert("list_set".to_string(), runtime.list_set.unwrap());
        builtins.insert("list_swap".to_string(), runtime.list_swap.unwrap());
        builtins.insert("list_pop".to_string(), runtime.list_pop.unwrap());
        builtins.insert("list_delete".to_string(), runtime.list_delete.unwrap());
        builtins.insert("list_copy".to_string(), runtime.list_copy.unwrap());
    }
    if map_enabled {
        builtins.insert("map_new".to_string(), runtime.map_new.unwrap());
        builtins.insert("map_set".to_string(), runtime.map_set.unwrap());
        builtins.insert("map_len".to_string(), runtime.map_len.unwrap());
        builtins.insert("map_get".to_string(), runtime.map_get.unwrap());
        builtins.insert("map_has".to_string(), runtime.map_has.unwrap());
        builtins.insert("map_delete".to_string(), runtime.map_delete.unwrap());
        builtins.insert("map_keys".to_string(), runtime.map_keys.unwrap());
    }
    builtins
}

struct RuntimeBuiltins {
    alloc: FuncId,
    box_value: FuncId,
    value_int: FuncId,
    value_to_i64: FuncId,
    value_is_truthy: FuncId,
    op_add: FuncId,
    op_subtract: FuncId,
    op_multiply: FuncId,
    op_divide: FuncId,
    op_modulo: FuncId,
    op_bitand: FuncId,
    op_bitor: FuncId,
    op_bitxor: FuncId,
    op_shl: FuncId,
    op_shr: FuncId,
    op_gt: FuncId,
    op_lt: FuncId,
    op_gte: FuncId,
    op_lte: FuncId,
    op_eq: FuncId,
    op_ne: FuncId,
    bigint_compare: Option<FuncId>,
    bigint_from_int: Option<FuncId>,
    bigint_add: Option<FuncId>,
    bigint_subtract: Option<FuncId>,
    bigint_multiply: Option<FuncId>,
    bigint_divide: Option<FuncId>,
    bigint_modulo: Option<FuncId>,
    bigint_bitand: Option<FuncId>,
    bigint_bitor: Option<FuncId>,
    bigint_bitxor: Option<FuncId>,
    bigint_shl: Option<FuncId>,
    bigint_shr: Option<FuncId>,
    list_new: Option<FuncId>,
    list_push: Option<FuncId>,
    list_insert: Option<FuncId>,
    list_len: Option<FuncId>,
    list_get: Option<FuncId>,
    list_set: Option<FuncId>,
    list_swap: Option<FuncId>,
    list_pop: Option<FuncId>,
    list_delete: Option<FuncId>,
    list_copy: Option<FuncId>,
    map_new: Option<FuncId>,
    map_set: Option<FuncId>,
    map_len: Option<FuncId>,
    map_get: Option<FuncId>,
    map_has: Option<FuncId>,
    map_delete: Option<FuncId>,
    map_keys: Option<FuncId>,
}

struct RuntimeData {
    arena: DataId,
    offset: DataId,
}

struct RuntimeFunctionIds {
    alloc: FuncId,
    oom: FuncId,
    memcpy: FuncId,
    builtins: RuntimeBuiltins,
}

fn declare_local_builtin(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    symbol: &str,
    params: &[Type],
) -> FuncId {
    declare_local_builtin_with_returns(module, isa, symbol, params, &[types::I64])
}

fn declare_local_pair_builtin(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    symbol: &str,
    params: &[Type],
) -> FuncId {
    declare_local_builtin_with_returns(module, isa, symbol, params, &[types::I64, types::I64])
}

fn declare_local_builtin_with_returns(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    symbol: &str,
    params: &[Type],
    returns: &[Type],
) -> FuncId {
    let mut sig = Signature::new(isa.default_call_conv());
    for &param in params {
        sig.params.push(AbiParam::new(param));
    }
    for &ret in returns {
        sig.returns.push(AbiParam::new(ret));
    }
    module.declare_function(symbol, Linkage::Local, &sig).unwrap()
}

fn init_runtime_data(module: &mut impl CraneliftModule) -> RuntimeData {
    let arena = module.declare_data("__rt_arena", Linkage::Local, true, false).unwrap();
    let mut arena_desc = DataDescription::new();
    arena_desc.define_zeroinit(ARENA_BYTES as usize);
    module.define_data(arena, &arena_desc).unwrap();

    let offset = module.declare_data("__rt_arena_offset", Linkage::Local, true, false).unwrap();
    let mut offset_desc = DataDescription::new();
    offset_desc.define((0i64).to_ne_bytes().to_vec().into_boxed_slice());
    module.define_data(offset, &offset_desc).unwrap();

    RuntimeData { arena, offset }
}

fn declare_runtime_function_ids(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    bigint_enabled: bool,
    list_enabled: bool,
    list_mutation_enabled: bool,
    map_enabled: bool,
) -> RuntimeFunctionIds {
    let alloc = declare_local_builtin(module, isa, "__rt_alloc", &[types::I64, types::I64]);
    let oom = declare_local_builtin(module, isa, "__rt_runtime_oom_host", &[]);
    let memcpy =
        declare_local_builtin(module, isa, "__rt_memcpy", &[types::I64, types::I64, types::I64]);
    let box_value = declare_local_builtin(module, isa, "__rt_box_value", &[types::I64, types::I64]);
    let value_int = declare_local_pair_builtin(module, isa, "__rt_value_int", &[types::I64]);
    let value_to_i64 = declare_local_builtin(module, isa, "__rt_value_to_i64", &[types::I64]);
    let value_is_truthy =
        declare_local_builtin(module, isa, "__rt_value_is_truthy", &[types::I64, types::I64]);
    let op_add = declare_local_pair_builtin(
        module,
        isa,
        "__rt_add",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_subtract = declare_local_pair_builtin(
        module,
        isa,
        "__rt_subtract",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_multiply = declare_local_pair_builtin(
        module,
        isa,
        "__rt_multiply",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_divide = declare_local_pair_builtin(
        module,
        isa,
        "__rt_divide",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_modulo = declare_local_pair_builtin(
        module,
        isa,
        "__rt_modulo",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_bitand = declare_local_pair_builtin(
        module,
        isa,
        "__rt_bitand",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_bitor = declare_local_pair_builtin(
        module,
        isa,
        "__rt_bitor",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_bitxor = declare_local_pair_builtin(
        module,
        isa,
        "__rt_bitxor",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_shl = declare_local_pair_builtin(
        module,
        isa,
        "__rt_shl",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_shr = declare_local_pair_builtin(
        module,
        isa,
        "__rt_shr",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_gt = declare_local_pair_builtin(
        module,
        isa,
        "__rt_gt",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_lt = declare_local_pair_builtin(
        module,
        isa,
        "__rt_lt",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_gte = declare_local_pair_builtin(
        module,
        isa,
        "__rt_gte",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_lte = declare_local_pair_builtin(
        module,
        isa,
        "__rt_lte",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_eq = declare_local_pair_builtin(
        module,
        isa,
        "__rt_eq",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let op_ne = declare_local_pair_builtin(
        module,
        isa,
        "__rt_ne",
        &[types::I64, types::I64, types::I64, types::I64],
    );
    let bigint_from_int = bigint_enabled.then(|| {
        declare_local_pair_builtin(module, isa, "__rt_bigint_from_int", &[types::I64, types::I64])
    });
    let bigint_compare = bigint_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_bigint_compare",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let bigint_add = bigint_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_bigint_add",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let bigint_subtract = bigint_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_bigint_subtract",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let bigint_multiply = bigint_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_bigint_multiply",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let bigint_divide = bigint_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_bigint_divide",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let bigint_modulo = bigint_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_bigint_modulo",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let bigint_bitand = bigint_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_bigint_bitand",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let bigint_bitor = bigint_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_bigint_bitor",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let bigint_bitxor = bigint_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_bigint_bitxor",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let bigint_shl = bigint_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_bigint_shl",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let bigint_shr = bigint_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_bigint_shr",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let list_new =
        list_enabled.then(|| declare_local_pair_builtin(module, isa, "__rt_list_new", &[]));
    let list_push = list_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_list_push",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let list_insert = list_mutation_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_list_insert",
            &[types::I64, types::I64, types::I64, types::I64, types::I64, types::I64],
        )
    });
    let list_len = list_enabled.then(|| {
        declare_local_pair_builtin(module, isa, "__rt_list_len", &[types::I64, types::I64])
    });
    let list_get = list_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_list_get",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let list_set = list_mutation_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_list_set",
            &[types::I64, types::I64, types::I64, types::I64, types::I64, types::I64],
        )
    });
    let list_swap = list_mutation_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_list_swap",
            &[types::I64, types::I64, types::I64, types::I64, types::I64, types::I64],
        )
    });
    let list_pop = list_mutation_enabled.then(|| {
        declare_local_pair_builtin(module, isa, "__rt_list_pop", &[types::I64, types::I64])
    });
    let list_delete = list_mutation_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_list_delete",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let list_copy = list_mutation_enabled.then(|| {
        declare_local_pair_builtin(module, isa, "__rt_list_copy", &[types::I64, types::I64])
    });
    let map_new = map_enabled.then(|| declare_local_pair_builtin(module, isa, "__rt_map_new", &[]));
    let map_set = map_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_map_set",
            &[types::I64, types::I64, types::I64, types::I64, types::I64, types::I64],
        )
    });
    let map_len = map_enabled.then(|| {
        declare_local_pair_builtin(module, isa, "__rt_map_len", &[types::I64, types::I64])
    });
    let map_get = map_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_map_get",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let map_has = map_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_map_has",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let map_delete = map_enabled.then(|| {
        declare_local_pair_builtin(
            module,
            isa,
            "__rt_map_delete",
            &[types::I64, types::I64, types::I64, types::I64],
        )
    });
    let map_keys = map_enabled.then(|| {
        declare_local_pair_builtin(module, isa, "__rt_map_keys", &[types::I64, types::I64])
    });

    RuntimeFunctionIds {
        alloc,
        oom,
        memcpy,
        builtins: RuntimeBuiltins {
            alloc,
            box_value,
            value_int,
            value_to_i64,
            value_is_truthy,
            op_add,
            op_subtract,
            op_multiply,
            op_divide,
            op_modulo,
            op_bitand,
            op_bitor,
            op_bitxor,
            op_shl,
            op_shr,
            op_gt,
            op_lt,
            op_gte,
            op_lte,
            op_eq,
            op_ne,
            bigint_compare,
            bigint_from_int,
            bigint_add,
            bigint_subtract,
            bigint_multiply,
            bigint_divide,
            bigint_modulo,
            bigint_bitand,
            bigint_bitor,
            bigint_bitxor,
            bigint_shl,
            bigint_shr,
            list_new,
            list_push,
            list_insert,
            list_len,
            list_get,
            list_set,
            list_swap,
            list_pop,
            list_delete,
            list_copy,
            map_new,
            map_set,
            map_len,
            map_get,
            map_has,
            map_delete,
            map_keys,
        },
    }
}

fn define_runtime_operations(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    ids: &RuntimeFunctionIds,
    bigint_enabled: bool,
    list_enabled: bool,
    list_mutation_enabled: bool,
    map_enabled: bool,
) {
    define_rt_memcpy(module, isa, flags, ids.memcpy);
    define_rt_box_value(module, isa, flags, ids.builtins.box_value, ids.alloc);
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
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_add,
        "add",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_subtract,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_subtract,
        "subtract",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_multiply,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_multiply,
        "multiply",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_divide,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_divide,
        "divide",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_modulo,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_modulo,
        "modulo",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_bitand,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_bitand,
        "bitand",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_bitor,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_bitor,
        "bitor",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_bitxor,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_bitxor,
        "bitxor",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_shl,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_shl,
        "shl",
    );
    define_rt_binary_op(
        module,
        isa,
        flags,
        ids.builtins.op_shr,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_shr,
        "shr",
    );
    if bigint_enabled {
        define_rt_bigint_from_int(
            module,
            isa,
            flags,
            ids.builtins.bigint_from_int.unwrap(),
            ids.alloc,
        );
        define_rt_bigint_compare(
            module,
            isa,
            flags,
            ids.builtins.bigint_compare.unwrap(),
            ids.builtins.value_int,
        );
        define_rt_bigint_add(module, isa, flags, ids.builtins.bigint_add.unwrap(), ids.alloc);
        define_rt_bigint_subtract(
            module,
            isa,
            flags,
            ids.builtins.bigint_subtract.unwrap(),
            ids.alloc,
        );
        define_rt_bigint_multiply(
            module,
            isa,
            flags,
            ids.builtins.bigint_multiply.unwrap(),
            ids.alloc,
        );
        define_rt_bigint_divide(module, isa, flags, ids.builtins.bigint_divide.unwrap(), ids.alloc);
        define_rt_bigint_modulo(module, isa, flags, ids.builtins.bigint_modulo.unwrap(), ids.alloc);
        define_rt_bigint_bitand(module, isa, flags, ids.builtins.bigint_bitand.unwrap(), ids.alloc);
        define_rt_bigint_bitor(module, isa, flags, ids.builtins.bigint_bitor.unwrap(), ids.alloc);
        define_rt_bigint_bitxor(module, isa, flags, ids.builtins.bigint_bitxor.unwrap(), ids.alloc);
        define_rt_bigint_shl(module, isa, flags, ids.builtins.bigint_shl.unwrap(), ids.alloc);
        define_rt_bigint_shr(module, isa, flags, ids.builtins.bigint_shr.unwrap(), ids.alloc);
    }
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_gt,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_compare,
        IntCC::SignedGreaterThan,
    );
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_lt,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_compare,
        IntCC::SignedLessThan,
    );
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_gte,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_compare,
        IntCC::SignedGreaterThanOrEqual,
    );
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_lte,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_compare,
        IntCC::SignedLessThanOrEqual,
    );
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_eq,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_compare,
        IntCC::Equal,
    );
    define_rt_compare_op(
        module,
        isa,
        flags,
        ids.builtins.op_ne,
        ids.builtins.value_to_i64,
        ids.builtins.value_int,
        ids.builtins.bigint_from_int,
        ids.builtins.bigint_compare,
        IntCC::NotEqual,
    );
    if list_enabled {
        define_rt_list_new(module, isa, flags, ids.builtins.list_new.unwrap(), ids.alloc);
        define_rt_list_push(
            module,
            isa,
            flags,
            ids.builtins.list_push.unwrap(),
            ids.alloc,
            ids.memcpy,
            ids.builtins.box_value,
        );
        define_rt_list_len(
            module,
            isa,
            flags,
            ids.builtins.list_len.unwrap(),
            ids.builtins.value_int,
        );
        define_rt_list_get(
            module,
            isa,
            flags,
            ids.builtins.list_get.unwrap(),
            ids.builtins.value_to_i64,
        );
    }
    if list_mutation_enabled {
        define_rt_list_insert(
            module,
            isa,
            flags,
            ids.builtins.list_insert.unwrap(),
            ids.builtins.value_to_i64,
            ids.alloc,
            ids.memcpy,
            ids.builtins.box_value,
        );
        define_rt_list_set(
            module,
            isa,
            flags,
            ids.builtins.list_set.unwrap(),
            ids.builtins.value_to_i64,
            ids.builtins.box_value,
        );
        define_rt_list_swap(
            module,
            isa,
            flags,
            ids.builtins.list_swap.unwrap(),
            ids.builtins.value_to_i64,
        );
        define_rt_list_pop(
            module,
            isa,
            flags,
            ids.builtins.list_pop.unwrap(),
            ids.builtins.value_to_i64,
        );
        define_rt_list_delete(
            module,
            isa,
            flags,
            ids.builtins.list_delete.unwrap(),
            ids.builtins.value_to_i64,
        );
        define_rt_list_copy(
            module,
            isa,
            flags,
            ids.builtins.list_copy.unwrap(),
            ids.builtins.value_to_i64,
            ids.alloc,
            ids.memcpy,
        );
    }
    if map_enabled {
        define_rt_map_new(module, isa, flags, ids.builtins.map_new.unwrap(), ids.alloc);
        define_rt_map_set(module, isa, flags, ids.builtins.map_set.unwrap(), ids.alloc, ids.memcpy);
        define_rt_map_len(
            module,
            isa,
            flags,
            ids.builtins.map_len.unwrap(),
            ids.builtins.value_int,
        );
        define_rt_map_get(module, isa, flags, ids.builtins.map_get.unwrap());
        define_rt_map_has(
            module,
            isa,
            flags,
            ids.builtins.map_has.unwrap(),
            ids.builtins.value_int,
        );
        define_rt_map_delete(module, isa, flags, ids.builtins.map_delete.unwrap());
        define_rt_map_keys(
            module,
            isa,
            flags,
            ids.builtins.map_keys.unwrap(),
            ids.builtins.list_new.expect("internal compiler error: map_keys requires list_new"),
            ids.builtins.list_push.expect("internal compiler error: map_keys requires list_push"),
        );
    }
}

fn define_runtime_ir(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    oom_host_id: FuncId,
    bigint_enabled: bool,
    list_enabled: bool,
    list_mutation_enabled: bool,
    map_enabled: bool,
) -> RuntimeBuiltins {
    let data = init_runtime_data(module);
    let ids = declare_runtime_function_ids(
        module,
        isa,
        bigint_enabled,
        list_enabled,
        list_mutation_enabled,
        map_enabled,
    );
    define_rt_host_oom_import_wrapper(module, isa, flags, ids.oom, oom_host_id);
    define_rt_alloc(module, isa, flags, ids.alloc, ids.oom, &data);
    define_runtime_operations(
        module,
        isa,
        flags,
        &ids,
        bigint_enabled,
        list_enabled,
        list_mutation_enabled,
        map_enabled,
    );
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
    module.declare_function(symbol, Linkage::Import, &sig).unwrap()
}

fn runtime_sig(isa: &OwnedTargetIsa, params: &[Type]) -> Signature {
    runtime_sig_with_returns(isa, params, &[types::I64])
}

fn runtime_sig_with_returns(isa: &OwnedTargetIsa, params: &[Type], returns: &[Type]) -> Signature {
    let mut sig = Signature::new(isa.default_call_conv());
    for &param in params {
        sig.params.push(AbiParam::new(param));
    }
    for &ret in returns {
        sig.returns.push(AbiParam::new(ret));
    }
    sig
}

fn rt_payload_for_tag(builder: &mut FunctionBuilder, handle: Value, expected_tag: i64) -> Value {
    let mf = MemFlags::new();
    let tag = builder.ins().load(types::I8, mf, handle, 0);
    let ok = builder.ins().icmp_imm(IntCC::Equal, tag, expected_tag);
    builder.ins().trapz(ok, TrapCode::BAD_CONVERSION_TO_INTEGER);
    builder.ins().load(types::I64, MemFlags::new(), handle, VALUE_PAYLOAD_OFFSET)
}

fn pair_payload_for_tag(
    builder: &mut FunctionBuilder,
    tag: Value,
    payload: Value,
    expected_tag: i64,
) -> Value {
    let ok = builder.ins().icmp_imm(IntCC::Equal, tag, expected_tag);
    builder.ins().trapz(ok, TrapCode::BAD_CONVERSION_TO_INTEGER);
    payload
}

fn string_load_len(builder: &mut FunctionBuilder, header_ptr: Value) -> Value {
    builder.ins().load(types::I64, MemFlags::new(), header_ptr, STRING_LEN_OFFSET)
}

fn string_load_ptr(builder: &mut FunctionBuilder, header_ptr: Value) -> Value {
    builder.ins().load(types::I64, MemFlags::new(), header_ptr, STRING_PTR_OFFSET)
}

fn string_eq_bytes(builder: &mut FunctionBuilder, lhs_ptr: Value, rhs_ptr: Value) -> Value {
    let lhs_len = string_load_len(builder, lhs_ptr);
    let rhs_len = string_load_len(builder, rhs_ptr);
    let len_equal = builder.ins().icmp(IntCC::Equal, lhs_len, rhs_len);
    let len_equal_block = builder.create_block();
    let false_block = builder.create_block();
    let loop_block = builder.create_block();
    let body_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(loop_block, types::I64);
    builder.append_block_param(done_block, types::I64);
    builder.ins().brif(len_equal, len_equal_block, &[], false_block, &[]);

    builder.switch_to_block(false_block);
    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(done_block, &[BlockArg::Value(zero)]);

    builder.switch_to_block(len_equal_block);
    builder.seal_block(len_equal_block);
    let lhs_data = string_load_ptr(builder, lhs_ptr);
    let rhs_data = string_load_ptr(builder, rhs_ptr);
    let start = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(loop_block, &[BlockArg::Value(start)]);

    builder.switch_to_block(loop_block);
    let idx = builder.block_params(loop_block)[0];
    let more = builder.ins().icmp(IntCC::UnsignedLessThan, idx, lhs_len);
    let one = builder.ins().iconst(types::I64, 1);
    builder.ins().brif(more, body_block, &[], done_block, &[BlockArg::Value(one)]);

    builder.switch_to_block(body_block);
    let lhs_byte_ptr = builder.ins().iadd(lhs_data, idx);
    let rhs_byte_ptr = builder.ins().iadd(rhs_data, idx);
    let lhs_byte = builder.ins().load(types::I8, MemFlags::new(), lhs_byte_ptr, 0);
    let rhs_byte = builder.ins().load(types::I8, MemFlags::new(), rhs_byte_ptr, 0);
    let bytes_equal = builder.ins().icmp(IntCC::Equal, lhs_byte, rhs_byte);
    let continue_block = builder.create_block();
    builder.ins().brif(bytes_equal, continue_block, &[], false_block, &[]);

    builder.switch_to_block(continue_block);
    builder.seal_block(continue_block);
    let next = builder.ins().iadd_imm(idx, 1);
    builder.ins().jump(loop_block, &[BlockArg::Value(next)]);

    builder.switch_to_block(done_block);
    builder.seal_block(false_block);
    builder.seal_block(done_block);
    builder.seal_block(loop_block);
    builder.seal_block(body_block);
    builder.block_params(done_block)[0]
}

fn map_load_len(builder: &mut FunctionBuilder, header_ptr: Value) -> Value {
    builder.ins().load(types::I64, MemFlags::new(), header_ptr, MAP_LEN_OFFSET)
}

fn map_load_cap(builder: &mut FunctionBuilder, header_ptr: Value) -> Value {
    builder.ins().load(types::I64, MemFlags::new(), header_ptr, MAP_CAP_OFFSET)
}

fn map_load_ptr(builder: &mut FunctionBuilder, header_ptr: Value) -> Value {
    builder.ins().load(types::I64, MemFlags::new(), header_ptr, MAP_PTR_OFFSET)
}

fn map_entry_ptr(builder: &mut FunctionBuilder, entries_ptr: Value, index: Value) -> Value {
    let entry_size = builder.ins().iconst(types::I64, MAP_ENTRY_SIZE);
    let off = builder.ins().imul(index, entry_size);
    builder.ins().iadd(entries_ptr, off)
}

fn bigint_load_sign(builder: &mut FunctionBuilder, header_ptr: Value) -> Value {
    builder.ins().load(types::I64, MemFlags::new(), header_ptr, BIGINT_SIGN_OFFSET)
}

fn bigint_store_sign(builder: &mut FunctionBuilder, header_ptr: Value, sign: Value) {
    builder.ins().store(MemFlags::new(), sign, header_ptr, BIGINT_SIGN_OFFSET);
}

fn bigint_load_len(builder: &mut FunctionBuilder, header_ptr: Value) -> Value {
    builder.ins().load(types::I64, MemFlags::new(), header_ptr, BIGINT_LEN_OFFSET)
}

fn bigint_store_len(builder: &mut FunctionBuilder, header_ptr: Value, len: Value) {
    builder.ins().store(MemFlags::new(), len, header_ptr, BIGINT_LEN_OFFSET);
}

fn bigint_store_cap(builder: &mut FunctionBuilder, header_ptr: Value, cap: Value) {
    builder.ins().store(MemFlags::new(), cap, header_ptr, BIGINT_CAP_OFFSET);
}

fn bigint_load_ptr(builder: &mut FunctionBuilder, header_ptr: Value) -> Value {
    builder.ins().load(types::I64, MemFlags::new(), header_ptr, BIGINT_PTR_OFFSET)
}

fn bigint_store_ptr(builder: &mut FunctionBuilder, header_ptr: Value, ptr: Value) {
    builder.ins().store(MemFlags::new(), ptr, header_ptr, BIGINT_PTR_OFFSET);
}

fn bigint_limb_ptr(builder: &mut FunctionBuilder, header_ptr: Value, index: Value) -> Value {
    let data_ptr = bigint_load_ptr(builder, header_ptr);
    let byte_off = builder.ins().ishl_imm(index, 2);
    builder.ins().iadd(data_ptr, byte_off)
}

fn bigint_load_limb(builder: &mut FunctionBuilder, header_ptr: Value, index: Value) -> Value {
    let ptr = bigint_limb_ptr(builder, header_ptr, index);
    let limb = builder.ins().load(types::I32, MemFlags::new(), ptr, 0);
    builder.ins().uextend(types::I64, limb)
}

fn bigint_store_limb(builder: &mut FunctionBuilder, header_ptr: Value, index: Value, limb: Value) {
    let ptr = bigint_limb_ptr(builder, header_ptr, index);
    let narrowed = builder.ins().ireduce(types::I32, limb);
    builder.ins().store(MemFlags::new(), narrowed, ptr, 0);
}

fn bigint_alloc(builder: &mut FunctionBuilder, alloc_ref: FuncRef, cap: Value) -> Value {
    let limb_size = builder.ins().iconst(types::I64, BIGINT_LIMB_SIZE);
    let limb_bytes = builder.ins().imul(cap, limb_size);
    let limb_align = builder.ins().iconst(types::I64, 4);
    let limb_call = builder.ins().call(alloc_ref, &[limb_bytes, limb_align]);
    let limb_ptr = builder.inst_results(limb_call)[0];

    let header_size = builder.ins().iconst(types::I64, BIGINT_HEADER_SIZE);
    let header_align = builder.ins().iconst(types::I64, 8);
    let header_call = builder.ins().call(alloc_ref, &[header_size, header_align]);
    let header_ptr = builder.inst_results(header_call)[0];

    let zero = builder.ins().iconst(types::I64, 0);
    bigint_store_sign(builder, header_ptr, zero);
    bigint_store_len(builder, header_ptr, zero);
    bigint_store_cap(builder, header_ptr, cap);
    bigint_store_ptr(builder, header_ptr, limb_ptr);
    header_ptr
}

fn bigint_normalize(builder: &mut FunctionBuilder, header_ptr: Value) {
    let loop_block = builder.create_block();
    let trim_block = builder.create_block();
    let done_block = builder.create_block();
    builder.ins().jump(loop_block, &[]);

    builder.switch_to_block(loop_block);
    let len = bigint_load_len(builder, header_ptr);
    let has_len = builder.ins().icmp_imm(IntCC::NotEqual, len, 0);
    builder.ins().brif(has_len, trim_block, &[], done_block, &[]);

    builder.switch_to_block(trim_block);
    builder.seal_block(trim_block);
    let last_index = builder.ins().iadd_imm(len, -1);
    let last = bigint_load_limb(builder, header_ptr, last_index);
    let is_zero = builder.ins().icmp_imm(IntCC::Equal, last, 0);
    let trim_more = builder.create_block();
    let keep_block = builder.create_block();
    builder.ins().brif(is_zero, trim_more, &[], keep_block, &[]);

    builder.switch_to_block(trim_more);
    builder.seal_block(trim_more);
    bigint_store_len(builder, header_ptr, last_index);
    builder.ins().jump(loop_block, &[]);

    builder.switch_to_block(keep_block);
    builder.seal_block(keep_block);
    builder.ins().jump(done_block, &[]);

    builder.switch_to_block(done_block);
    builder.seal_block(done_block);
    let final_len = bigint_load_len(builder, header_ptr);
    let non_zero = builder.ins().icmp_imm(IntCC::NotEqual, final_len, 0);
    let keep_sign = builder.create_block();
    let zero_sign = builder.create_block();
    builder.ins().brif(non_zero, keep_sign, &[], zero_sign, &[]);

    builder.switch_to_block(keep_sign);
    builder.seal_block(keep_sign);
    builder.ins().jump(zero_sign, &[]);

    builder.switch_to_block(zero_sign);
    builder.seal_block(zero_sign);
    let final_len = bigint_load_len(builder, header_ptr);
    let still_zero = builder.ins().icmp_imm(IntCC::Equal, final_len, 0);
    let end_block = builder.create_block();
    let set_zero_block = builder.create_block();
    builder.ins().brif(still_zero, set_zero_block, &[], end_block, &[]);

    builder.switch_to_block(set_zero_block);
    builder.seal_block(set_zero_block);
    let zero = builder.ins().iconst(types::I64, 0);
    bigint_store_sign(builder, header_ptr, zero);
    builder.ins().jump(end_block, &[]);

    builder.switch_to_block(end_block);
    builder.seal_block(end_block);
    builder.seal_block(loop_block);
}

fn bigint_cmp_abs(builder: &mut FunctionBuilder, lhs: Value, rhs: Value) -> Value {
    let merge = builder.create_block();
    builder.append_block_param(merge, types::I64);
    let one = builder.ins().iconst(types::I64, 1);
    let minus_one = builder.ins().iconst(types::I64, -1);
    let zero = builder.ins().iconst(types::I64, 0);
    let lhs_len = bigint_load_len(builder, lhs);
    let rhs_len = bigint_load_len(builder, rhs);
    let len_eq = builder.ins().icmp(IntCC::Equal, lhs_len, rhs_len);
    let len_cmp_block = builder.create_block();
    let same_len_block = builder.create_block();
    builder.ins().brif(len_eq, same_len_block, &[], len_cmp_block, &[]);

    builder.switch_to_block(len_cmp_block);
    builder.seal_block(len_cmp_block);
    let gt = builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs_len, rhs_len);
    let len_cmp = builder.ins().select(gt, one, minus_one);
    builder.ins().jump(merge, &[BlockArg::Value(len_cmp)]);

    builder.switch_to_block(same_len_block);
    builder.seal_block(same_len_block);
    let loop_block = builder.create_block();
    let body_block = builder.create_block();
    let equal_block = builder.create_block();
    builder.append_block_param(loop_block, types::I64);
    builder.ins().jump(loop_block, &[BlockArg::Value(lhs_len)]);

    builder.switch_to_block(loop_block);
    let remaining = builder.block_params(loop_block)[0];
    let has_more = builder.ins().icmp_imm(IntCC::NotEqual, remaining, 0);
    builder.ins().brif(has_more, body_block, &[], equal_block, &[]);

    builder.switch_to_block(body_block);
    builder.seal_block(body_block);
    let index = builder.ins().iadd_imm(remaining, -1);
    let lhs_limb = bigint_load_limb(builder, lhs, index);
    let rhs_limb = bigint_load_limb(builder, rhs, index);
    let limb_eq = builder.ins().icmp(IntCC::Equal, lhs_limb, rhs_limb);
    let next_block = builder.create_block();
    let diff_block = builder.create_block();
    builder.ins().brif(limb_eq, next_block, &[], diff_block, &[]);

    builder.switch_to_block(next_block);
    builder.seal_block(next_block);
    builder.ins().jump(loop_block, &[BlockArg::Value(index)]);

    builder.switch_to_block(diff_block);
    builder.seal_block(diff_block);
    let gt = builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs_limb, rhs_limb);
    let limb_cmp = builder.ins().select(gt, one, minus_one);
    builder.ins().jump(merge, &[BlockArg::Value(limb_cmp)]);

    builder.switch_to_block(equal_block);
    builder.seal_block(equal_block);
    builder.ins().jump(merge, &[BlockArg::Value(zero)]);

    builder.switch_to_block(merge);
    builder.seal_block(merge);
    builder.seal_block(loop_block);
    builder.block_params(merge)[0]
}

fn bigint_add_abs(
    builder: &mut FunctionBuilder,
    alloc_ref: FuncRef,
    lhs: Value,
    rhs: Value,
) -> Value {
    let lhs_len = bigint_load_len(builder, lhs);
    let rhs_len = bigint_load_len(builder, rhs);
    let lhs_ge = builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lhs_len, rhs_len);
    let max_len = builder.ins().select(lhs_ge, lhs_len, rhs_len);
    let cap = builder.ins().iadd_imm(max_len, 1);
    let header_ptr = bigint_alloc(builder, alloc_ref, cap);
    bigint_store_len(builder, header_ptr, cap);

    let loop_block = builder.create_block();
    let body_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(loop_block, types::I64);
    builder.append_block_param(loop_block, types::I64);
    builder.append_block_param(done_block, types::I64);
    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(loop_block, &[BlockArg::Value(zero), BlockArg::Value(zero)]);

    builder.switch_to_block(loop_block);
    let idx = builder.block_params(loop_block)[0];
    let carry = builder.block_params(loop_block)[1];
    let more = builder.ins().icmp(IntCC::UnsignedLessThan, idx, max_len);
    builder.ins().brif(more, body_block, &[], done_block, &[BlockArg::Value(carry)]);

    builder.switch_to_block(body_block);
    builder.seal_block(body_block);
    let lhs_in = builder.ins().icmp(IntCC::UnsignedLessThan, idx, lhs_len);
    let lhs_zero_block = builder.create_block();
    let lhs_read_block = builder.create_block();
    let lhs_merge = builder.create_block();
    builder.append_block_param(lhs_merge, types::I64);
    builder.ins().brif(lhs_in, lhs_read_block, &[], lhs_zero_block, &[]);

    builder.switch_to_block(lhs_zero_block);
    builder.seal_block(lhs_zero_block);
    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(lhs_merge, &[BlockArg::Value(zero)]);

    builder.switch_to_block(lhs_read_block);
    builder.seal_block(lhs_read_block);
    let limb = bigint_load_limb(builder, lhs, idx);
    builder.ins().jump(lhs_merge, &[BlockArg::Value(limb)]);

    builder.switch_to_block(lhs_merge);
    builder.seal_block(lhs_merge);
    let lhs_limb = builder.block_params(lhs_merge)[0];

    let rhs_in = builder.ins().icmp(IntCC::UnsignedLessThan, idx, rhs_len);
    let rhs_zero_block = builder.create_block();
    let rhs_read_block = builder.create_block();
    let rhs_merge = builder.create_block();
    builder.append_block_param(rhs_merge, types::I64);
    builder.ins().brif(rhs_in, rhs_read_block, &[], rhs_zero_block, &[]);

    builder.switch_to_block(rhs_zero_block);
    builder.seal_block(rhs_zero_block);
    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(rhs_merge, &[BlockArg::Value(zero)]);

    builder.switch_to_block(rhs_read_block);
    builder.seal_block(rhs_read_block);
    let limb = bigint_load_limb(builder, rhs, idx);
    builder.ins().jump(rhs_merge, &[BlockArg::Value(limb)]);

    builder.switch_to_block(rhs_merge);
    builder.seal_block(rhs_merge);
    let rhs_limb = builder.block_params(rhs_merge)[0];

    let tmp = builder.ins().iadd(lhs_limb, rhs_limb);
    let sum = builder.ins().iadd(tmp, carry);
    let mask = builder.ins().iconst(types::I64, 0xffff_ffff);
    let low = builder.ins().band(sum, mask);
    bigint_store_limb(builder, header_ptr, idx, low);
    let next_carry = builder.ins().ushr_imm(sum, 32);
    let next_idx = builder.ins().iadd_imm(idx, 1);
    builder.ins().jump(loop_block, &[BlockArg::Value(next_idx), BlockArg::Value(next_carry)]);

    builder.switch_to_block(done_block);
    builder.seal_block(done_block);
    let final_carry = builder.block_params(done_block)[0];
    bigint_store_limb(builder, header_ptr, max_len, final_carry);
    bigint_normalize(builder, header_ptr);
    builder.seal_block(loop_block);
    header_ptr
}

fn bigint_sub_abs(
    builder: &mut FunctionBuilder,
    alloc_ref: FuncRef,
    lhs: Value,
    rhs: Value,
) -> Value {
    let lhs_len = bigint_load_len(builder, lhs);
    let rhs_len = bigint_load_len(builder, rhs);
    let header_ptr = bigint_alloc(builder, alloc_ref, lhs_len);
    bigint_store_len(builder, header_ptr, lhs_len);
    let loop_block = builder.create_block();
    let body_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(loop_block, types::I64);
    builder.append_block_param(loop_block, types::I64);
    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(loop_block, &[BlockArg::Value(zero), BlockArg::Value(zero)]);

    builder.switch_to_block(loop_block);
    let idx = builder.block_params(loop_block)[0];
    let borrow = builder.block_params(loop_block)[1];
    let more = builder.ins().icmp(IntCC::UnsignedLessThan, idx, lhs_len);
    builder.ins().brif(more, body_block, &[], done_block, &[]);

    builder.switch_to_block(body_block);
    builder.seal_block(body_block);
    let lhs_limb = bigint_load_limb(builder, lhs, idx);
    let rhs_in = builder.ins().icmp(IntCC::UnsignedLessThan, idx, rhs_len);
    let rhs_zero_block = builder.create_block();
    let rhs_read_block = builder.create_block();
    let rhs_merge = builder.create_block();
    builder.append_block_param(rhs_merge, types::I64);
    builder.ins().brif(rhs_in, rhs_read_block, &[], rhs_zero_block, &[]);

    builder.switch_to_block(rhs_zero_block);
    builder.seal_block(rhs_zero_block);
    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(rhs_merge, &[BlockArg::Value(zero)]);

    builder.switch_to_block(rhs_read_block);
    builder.seal_block(rhs_read_block);
    let limb = bigint_load_limb(builder, rhs, idx);
    builder.ins().jump(rhs_merge, &[BlockArg::Value(limb)]);

    builder.switch_to_block(rhs_merge);
    builder.seal_block(rhs_merge);
    let rhs_limb = builder.block_params(rhs_merge)[0];
    let rhs_plus_borrow = builder.ins().iadd(rhs_limb, borrow);
    let enough = builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lhs_limb, rhs_plus_borrow);
    let no_borrow_block = builder.create_block();
    let borrow_block = builder.create_block();
    let merge = builder.create_block();
    builder.append_block_param(merge, types::I64);
    builder.append_block_param(merge, types::I64);
    builder.ins().brif(enough, no_borrow_block, &[], borrow_block, &[]);

    builder.switch_to_block(no_borrow_block);
    builder.seal_block(no_borrow_block);
    let diff = builder.ins().isub(lhs_limb, rhs_plus_borrow);
    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(merge, &[BlockArg::Value(diff), BlockArg::Value(zero)]);

    builder.switch_to_block(borrow_block);
    builder.seal_block(borrow_block);
    let base = builder.ins().iconst(types::I64, 1_i64 << 32);
    let lhs_with_base = builder.ins().iadd(lhs_limb, base);
    let diff = builder.ins().isub(lhs_with_base, rhs_plus_borrow);
    let one = builder.ins().iconst(types::I64, 1);
    builder.ins().jump(merge, &[BlockArg::Value(diff), BlockArg::Value(one)]);

    builder.switch_to_block(merge);
    builder.seal_block(merge);
    let out_limb = builder.block_params(merge)[0];
    let next_borrow = builder.block_params(merge)[1];
    bigint_store_limb(builder, header_ptr, idx, out_limb);
    let next_idx = builder.ins().iadd_imm(idx, 1);
    builder.ins().jump(loop_block, &[BlockArg::Value(next_idx), BlockArg::Value(next_borrow)]);

    builder.switch_to_block(done_block);
    builder.seal_block(done_block);
    bigint_normalize(builder, header_ptr);
    builder.seal_block(loop_block);
    header_ptr
}

fn emit_bigint_addsub(
    builder: &mut FunctionBuilder,
    alloc_ref: FuncRef,
    lhs_ptr: Value,
    lhs_sign: Value,
    rhs_ptr: Value,
    rhs_sign: Value,
) -> Value {
    let merge = builder.create_block();
    builder.append_block_param(merge, types::I64);
    let same_sign_block = builder.create_block();
    builder.ins().jump(same_sign_block, &[]);

    builder.switch_to_block(same_sign_block);
    builder.seal_block(same_sign_block);
    let signs_equal = builder.ins().icmp(IntCC::Equal, lhs_sign, rhs_sign);
    let add_block = builder.create_block();
    let diff_sign_block = builder.create_block();
    builder.ins().brif(signs_equal, add_block, &[], diff_sign_block, &[]);

    builder.switch_to_block(add_block);
    builder.seal_block(add_block);
    let sum_ptr = bigint_add_abs(builder, alloc_ref, lhs_ptr, rhs_ptr);
    bigint_store_sign(builder, sum_ptr, lhs_sign);
    bigint_normalize(builder, sum_ptr);
    builder.ins().jump(merge, &[BlockArg::Value(sum_ptr)]);

    builder.switch_to_block(diff_sign_block);
    builder.seal_block(diff_sign_block);
    let cmp = bigint_cmp_abs(builder, lhs_ptr, rhs_ptr);
    let cmp_zero = builder.ins().icmp_imm(IntCC::Equal, cmp, 0);
    let equal_block = builder.create_block();
    let cmp_non_zero_block = builder.create_block();
    builder.ins().brif(cmp_zero, equal_block, &[], cmp_non_zero_block, &[]);

    builder.switch_to_block(equal_block);
    builder.seal_block(equal_block);
    let zero_cap = builder.ins().iconst(types::I64, 0);
    let zero_ptr = bigint_alloc(builder, alloc_ref, zero_cap);
    let zero = builder.ins().iconst(types::I64, 0);
    bigint_store_sign(builder, zero_ptr, zero);
    bigint_store_len(builder, zero_ptr, zero);
    builder.ins().jump(merge, &[BlockArg::Value(zero_ptr)]);

    builder.switch_to_block(cmp_non_zero_block);
    builder.seal_block(cmp_non_zero_block);
    let lhs_gt = builder.ins().icmp_imm(IntCC::SignedGreaterThan, cmp, 0);
    let lhs_gt_block = builder.create_block();
    let rhs_gt_block = builder.create_block();
    builder.ins().brif(lhs_gt, lhs_gt_block, &[], rhs_gt_block, &[]);

    builder.switch_to_block(lhs_gt_block);
    builder.seal_block(lhs_gt_block);
    let diff_ptr = bigint_sub_abs(builder, alloc_ref, lhs_ptr, rhs_ptr);
    bigint_store_sign(builder, diff_ptr, lhs_sign);
    bigint_normalize(builder, diff_ptr);
    builder.ins().jump(merge, &[BlockArg::Value(diff_ptr)]);

    builder.switch_to_block(rhs_gt_block);
    builder.seal_block(rhs_gt_block);
    let diff_ptr = bigint_sub_abs(builder, alloc_ref, rhs_ptr, lhs_ptr);
    bigint_store_sign(builder, diff_ptr, rhs_sign);
    bigint_normalize(builder, diff_ptr);
    builder.ins().jump(merge, &[BlockArg::Value(diff_ptr)]);

    builder.switch_to_block(merge);
    builder.seal_block(merge);
    builder.block_params(merge)[0]
}

fn bigint_signed_cmp(
    builder: &mut FunctionBuilder,
    lhs_ptr: Value,
    lhs_sign: Value,
    rhs_ptr: Value,
    rhs_sign: Value,
) -> Value {
    let merge = builder.create_block();
    builder.append_block_param(merge, types::I64);

    let signs_equal = builder.ins().icmp(IntCC::Equal, lhs_sign, rhs_sign);
    let same_sign_block = builder.create_block();
    let diff_sign_block = builder.create_block();
    builder.ins().brif(signs_equal, same_sign_block, &[], diff_sign_block, &[]);

    builder.switch_to_block(diff_sign_block);
    builder.seal_block(diff_sign_block);
    let lhs_gt = builder.ins().icmp(IntCC::SignedGreaterThan, lhs_sign, rhs_sign);
    let one = builder.ins().iconst(types::I64, 1);
    let minus_one = builder.ins().iconst(types::I64, -1);
    let raw = builder.ins().select(lhs_gt, one, minus_one);
    builder.ins().jump(merge, &[BlockArg::Value(raw)]);

    builder.switch_to_block(same_sign_block);
    builder.seal_block(same_sign_block);
    let sign_zero = builder.ins().icmp_imm(IntCC::Equal, lhs_sign, 0);
    let zero_block = builder.create_block();
    let cmp_block = builder.create_block();
    builder.ins().brif(sign_zero, zero_block, &[], cmp_block, &[]);

    builder.switch_to_block(zero_block);
    builder.seal_block(zero_block);
    let zero = builder.ins().iconst(types::I64, 0);
    builder.ins().jump(merge, &[BlockArg::Value(zero)]);

    builder.switch_to_block(cmp_block);
    builder.seal_block(cmp_block);
    let cmp = bigint_cmp_abs(builder, lhs_ptr, rhs_ptr);
    let sign_negative = builder.ins().icmp_imm(IntCC::SignedLessThan, lhs_sign, 0);
    let neg_block = builder.create_block();
    let pos_block = builder.create_block();
    builder.ins().brif(sign_negative, neg_block, &[], pos_block, &[]);

    builder.switch_to_block(pos_block);
    builder.seal_block(pos_block);
    builder.ins().jump(merge, &[BlockArg::Value(cmp)]);

    builder.switch_to_block(neg_block);
    builder.seal_block(neg_block);
    let neg_cmp = builder.ins().ineg(cmp);
    builder.ins().jump(merge, &[BlockArg::Value(neg_cmp)]);

    builder.switch_to_block(merge);
    builder.seal_block(merge);
    builder.block_params(merge)[0]
}

fn bigint_zero(builder: &mut FunctionBuilder, alloc_ref: FuncRef) -> Value {
    let zero_cap = builder.ins().iconst(types::I64, 0);
    let zero_ptr = bigint_alloc(builder, alloc_ref, zero_cap);
    let zero = builder.ins().iconst(types::I64, 0);
    bigint_store_sign(builder, zero_ptr, zero);
    bigint_store_len(builder, zero_ptr, zero);
    zero_ptr
}

fn bigint_one(builder: &mut FunctionBuilder, alloc_ref: FuncRef) -> Value {
    let one = builder.ins().iconst(types::I64, 1);
    let ptr = bigint_alloc(builder, alloc_ref, one);
    bigint_store_sign(builder, ptr, one);
    bigint_store_len(builder, ptr, one);
    let zero = builder.ins().iconst(types::I64, 0);
    bigint_store_limb(builder, ptr, zero, one);
    ptr
}

fn bigint_trap_if_negative(builder: &mut FunctionBuilder, header_ptr: Value) {
    let sign = bigint_load_sign(builder, header_ptr);
    let negative = builder.ins().icmp_imm(IntCC::SignedLessThan, sign, 0);
    builder.ins().trapnz(negative, TrapCode::BAD_CONVERSION_TO_INTEGER);
}

fn bigint_bitwise_abs(
    builder: &mut FunctionBuilder,
    alloc_ref: FuncRef,
    lhs: Value,
    rhs: Value,
    op: &str,
) -> Value {
    let lhs_len = bigint_load_len(builder, lhs);
    let rhs_len = bigint_load_len(builder, rhs);
    let cap = if op == "bitand" {
        let lhs_le_rhs = builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, lhs_len, rhs_len);
        builder.ins().select(lhs_le_rhs, lhs_len, rhs_len)
    } else {
        let lhs_ge_rhs = builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lhs_len, rhs_len);
        builder.ins().select(lhs_ge_rhs, lhs_len, rhs_len)
    };
    let result = bigint_alloc(builder, alloc_ref, cap);
    let one = builder.ins().iconst(types::I64, 1);
    let zero = builder.ins().iconst(types::I64, 0);
    bigint_store_sign(builder, result, one);
    bigint_store_len(builder, result, cap);

    let loop_block = builder.create_block();
    let body_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(loop_block, types::I64);
    builder.ins().jump(loop_block, &[BlockArg::Value(zero)]);

    builder.switch_to_block(loop_block);
    let idx = builder.block_params(loop_block)[0];
    let more = builder.ins().icmp(IntCC::UnsignedLessThan, idx, cap);
    builder.ins().brif(more, body_block, &[], done_block, &[]);

    builder.switch_to_block(body_block);
    builder.seal_block(body_block);
    let lhs_limb = if op == "bitand" {
        bigint_load_limb(builder, lhs, idx)
    } else {
        let lhs_in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, idx, lhs_len);
        let lhs_have = builder.create_block();
        let lhs_zero = builder.create_block();
        let lhs_merge = builder.create_block();
        builder.append_block_param(lhs_merge, types::I64);
        builder.ins().brif(lhs_in_bounds, lhs_have, &[], lhs_zero, &[]);
        builder.switch_to_block(lhs_have);
        builder.seal_block(lhs_have);
        let limb = bigint_load_limb(builder, lhs, idx);
        builder.ins().jump(lhs_merge, &[BlockArg::Value(limb)]);
        builder.switch_to_block(lhs_zero);
        builder.seal_block(lhs_zero);
        builder.ins().jump(lhs_merge, &[BlockArg::Value(zero)]);
        builder.switch_to_block(lhs_merge);
        builder.seal_block(lhs_merge);
        builder.block_params(lhs_merge)[0]
    };
    let rhs_limb = if op == "bitand" {
        bigint_load_limb(builder, rhs, idx)
    } else {
        let rhs_in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, idx, rhs_len);
        let rhs_have = builder.create_block();
        let rhs_zero = builder.create_block();
        let rhs_merge = builder.create_block();
        builder.append_block_param(rhs_merge, types::I64);
        builder.ins().brif(rhs_in_bounds, rhs_have, &[], rhs_zero, &[]);
        builder.switch_to_block(rhs_have);
        builder.seal_block(rhs_have);
        let limb = bigint_load_limb(builder, rhs, idx);
        builder.ins().jump(rhs_merge, &[BlockArg::Value(limb)]);
        builder.switch_to_block(rhs_zero);
        builder.seal_block(rhs_zero);
        builder.ins().jump(rhs_merge, &[BlockArg::Value(zero)]);
        builder.switch_to_block(rhs_merge);
        builder.seal_block(rhs_merge);
        builder.block_params(rhs_merge)[0]
    };
    let out_limb = match op {
        "bitand" => builder.ins().band(lhs_limb, rhs_limb),
        "bitor" => builder.ins().bor(lhs_limb, rhs_limb),
        "bitxor" => builder.ins().bxor(lhs_limb, rhs_limb),
        _ => unreachable!(),
    };
    bigint_store_limb(builder, result, idx, out_limb);
    let next = builder.ins().iadd_imm(idx, 1);
    builder.ins().jump(loop_block, &[BlockArg::Value(next)]);

    builder.switch_to_block(done_block);
    builder.seal_block(done_block);
    builder.seal_block(loop_block);
    bigint_normalize(builder, result);
    result
}

fn bigint_shift_left(
    builder: &mut FunctionBuilder,
    alloc_ref: FuncRef,
    lhs: Value,
    shift: Value,
) -> Value {
    let zero = builder.ins().iconst(types::I64, 0);
    let one = builder.ins().iconst(types::I64, 1);
    let lhs_sign = bigint_load_sign(builder, lhs);
    let lhs_is_zero = builder.ins().icmp(IntCC::Equal, lhs_sign, zero);
    let zero_block = builder.create_block();
    let work_block = builder.create_block();
    let merge = builder.create_block();
    builder.append_block_param(merge, types::I64);
    builder.ins().brif(lhs_is_zero, zero_block, &[], work_block, &[]);

    builder.switch_to_block(zero_block);
    let zero_ptr = bigint_zero(builder, alloc_ref);
    builder.ins().jump(merge, &[BlockArg::Value(zero_ptr)]);
    builder.seal_block(zero_block);

    builder.switch_to_block(work_block);
    builder.seal_block(work_block);
    let lhs_len = bigint_load_len(builder, lhs);
    let limb_shift = builder.ins().ushr_imm(shift, 5);
    let bit_shift = builder.ins().band_imm(shift, 31);
    let bit_non_zero = builder.ins().icmp_imm(IntCC::NotEqual, bit_shift, 0);
    let carry_block = builder.create_block();
    let no_carry_block = builder.create_block();
    let extra_merge = builder.create_block();
    builder.append_block_param(extra_merge, types::I64);
    builder.ins().brif(bit_non_zero, carry_block, &[], no_carry_block, &[]);

    builder.switch_to_block(carry_block);
    builder.seal_block(carry_block);
    builder.ins().jump(extra_merge, &[BlockArg::Value(one)]);

    builder.switch_to_block(no_carry_block);
    builder.seal_block(no_carry_block);
    builder.ins().jump(extra_merge, &[BlockArg::Value(zero)]);

    builder.switch_to_block(extra_merge);
    builder.seal_block(extra_merge);
    let extra = builder.block_params(extra_merge)[0];
    let lhs_plus_limb = builder.ins().iadd(lhs_len, limb_shift);
    let cap = builder.ins().iadd(lhs_plus_limb, extra);
    let result = bigint_alloc(builder, alloc_ref, cap);
    bigint_store_sign(builder, result, one);
    bigint_store_len(builder, result, cap);

    let init_loop = builder.create_block();
    let init_body = builder.create_block();
    let init_done = builder.create_block();
    builder.append_block_param(init_loop, types::I64);
    builder.ins().jump(init_loop, &[BlockArg::Value(zero)]);
    builder.switch_to_block(init_loop);
    let init_idx = builder.block_params(init_loop)[0];
    let init_more = builder.ins().icmp(IntCC::UnsignedLessThan, init_idx, cap);
    builder.ins().brif(init_more, init_body, &[], init_done, &[]);
    builder.switch_to_block(init_body);
    builder.seal_block(init_body);
    bigint_store_limb(builder, result, init_idx, zero);
    let init_next = builder.ins().iadd_imm(init_idx, 1);
    builder.ins().jump(init_loop, &[BlockArg::Value(init_next)]);
    builder.switch_to_block(init_done);
    builder.seal_block(init_done);
    builder.seal_block(init_loop);

    let loop_block = builder.create_block();
    let body_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(loop_block, types::I64);
    builder.ins().jump(loop_block, &[BlockArg::Value(zero)]);
    builder.switch_to_block(loop_block);
    let idx = builder.block_params(loop_block)[0];
    let more = builder.ins().icmp(IntCC::UnsignedLessThan, idx, lhs_len);
    builder.ins().brif(more, body_block, &[], done_block, &[]);
    builder.switch_to_block(body_block);
    builder.seal_block(body_block);
    let limb = bigint_load_limb(builder, lhs, idx);
    let dst_idx = builder.ins().iadd(idx, limb_shift);
    let low = builder.ins().ishl(limb, bit_shift);
    let mask = builder.ins().iconst(types::I64, 0xffff_ffff);
    let low32 = builder.ins().band(low, mask);
    let existing_low = bigint_load_limb(builder, result, dst_idx);
    let new_low = builder.ins().iadd(existing_low, low32);
    bigint_store_limb(builder, result, dst_idx, new_low);
    let has_carry = builder.ins().icmp_imm(IntCC::NotEqual, bit_shift, 0);
    let carry_body = builder.create_block();
    let carry_done = builder.create_block();
    builder.ins().brif(has_carry, carry_body, &[], carry_done, &[]);
    builder.switch_to_block(carry_body);
    builder.seal_block(carry_body);
    let high = builder.ins().ushr_imm(low, 32);
    let next_idx = builder.ins().iadd_imm(dst_idx, 1);
    let existing_high = bigint_load_limb(builder, result, next_idx);
    let new_high = builder.ins().iadd(existing_high, high);
    bigint_store_limb(builder, result, next_idx, new_high);
    builder.ins().jump(carry_done, &[]);
    builder.switch_to_block(carry_done);
    builder.seal_block(carry_done);
    let next = builder.ins().iadd_imm(idx, 1);
    builder.ins().jump(loop_block, &[BlockArg::Value(next)]);
    builder.switch_to_block(done_block);
    builder.seal_block(done_block);
    builder.seal_block(loop_block);
    bigint_normalize(builder, result);
    builder.ins().jump(merge, &[BlockArg::Value(result)]);

    builder.switch_to_block(merge);
    builder.seal_block(merge);
    builder.block_params(merge)[0]
}

fn bigint_shift_right(
    builder: &mut FunctionBuilder,
    alloc_ref: FuncRef,
    lhs: Value,
    shift: Value,
) -> Value {
    let zero = builder.ins().iconst(types::I64, 0);
    let lhs_sign = bigint_load_sign(builder, lhs);
    let lhs_is_zero = builder.ins().icmp(IntCC::Equal, lhs_sign, zero);
    let zero_block = builder.create_block();
    let work_block = builder.create_block();
    let merge = builder.create_block();
    builder.append_block_param(merge, types::I64);
    builder.ins().brif(lhs_is_zero, zero_block, &[], work_block, &[]);

    builder.switch_to_block(zero_block);
    let zero_ptr = bigint_zero(builder, alloc_ref);
    builder.ins().jump(merge, &[BlockArg::Value(zero_ptr)]);

    builder.switch_to_block(work_block);
    builder.seal_block(work_block);
    let lhs_len = bigint_load_len(builder, lhs);
    let limb_shift = builder.ins().ushr_imm(shift, 5);
    let enough = builder.ins().icmp(IntCC::UnsignedLessThan, limb_shift, lhs_len);
    let non_zero_block = builder.create_block();
    builder.ins().brif(enough, non_zero_block, &[], zero_block, &[]);
    builder.seal_block(zero_block);

    builder.switch_to_block(non_zero_block);
    builder.seal_block(non_zero_block);
    let result_len = builder.ins().isub(lhs_len, limb_shift);
    let bit_shift = builder.ins().band_imm(shift, 31);
    let result = bigint_alloc(builder, alloc_ref, result_len);
    let one = builder.ins().iconst(types::I64, 1);
    bigint_store_sign(builder, result, one);
    bigint_store_len(builder, result, result_len);

    let loop_block = builder.create_block();
    let body_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(loop_block, types::I64);
    builder.ins().jump(loop_block, &[BlockArg::Value(zero)]);
    builder.switch_to_block(loop_block);
    let idx = builder.block_params(loop_block)[0];
    let more = builder.ins().icmp(IntCC::UnsignedLessThan, idx, result_len);
    builder.ins().brif(more, body_block, &[], done_block, &[]);
    builder.switch_to_block(body_block);
    builder.seal_block(body_block);
    let src_idx = builder.ins().iadd(idx, limb_shift);
    let limb = bigint_load_limb(builder, lhs, src_idx);
    let has_bit_shift = builder.ins().icmp_imm(IntCC::NotEqual, bit_shift, 0);
    let shift_body = builder.create_block();
    let shift_none = builder.create_block();
    let out_merge = builder.create_block();
    builder.append_block_param(out_merge, types::I64);
    builder.ins().brif(has_bit_shift, shift_body, &[], shift_none, &[]);

    builder.switch_to_block(shift_none);
    builder.seal_block(shift_none);
    builder.ins().jump(out_merge, &[BlockArg::Value(limb)]);

    builder.switch_to_block(shift_body);
    builder.seal_block(shift_body);
    let low = builder.ins().ushr(limb, bit_shift);
    let next_src = builder.ins().iadd_imm(src_idx, 1);
    let next_in_bounds = builder.ins().icmp(IntCC::UnsignedLessThan, next_src, lhs_len);
    let next_have = builder.create_block();
    let next_zero = builder.create_block();
    let next_merge = builder.create_block();
    builder.append_block_param(next_merge, types::I64);
    builder.ins().brif(next_in_bounds, next_have, &[], next_zero, &[]);

    builder.switch_to_block(next_have);
    builder.seal_block(next_have);
    let next_limb = bigint_load_limb(builder, lhs, next_src);
    let thirty_two = builder.ins().iconst(types::I64, 32);
    let inv_shift = builder.ins().isub(thirty_two, bit_shift);
    let high = builder.ins().ishl(next_limb, inv_shift);
    let mask = builder.ins().iconst(types::I64, 0xffff_ffff);
    let high_masked = builder.ins().band(high, mask);
    let merged = builder.ins().bor(low, high_masked);
    builder.ins().jump(next_merge, &[BlockArg::Value(merged)]);

    builder.switch_to_block(next_zero);
    builder.seal_block(next_zero);
    builder.ins().jump(next_merge, &[BlockArg::Value(low)]);

    builder.switch_to_block(next_merge);
    builder.seal_block(next_merge);
    let merged_limb = builder.block_params(next_merge)[0];
    builder.ins().jump(out_merge, &[BlockArg::Value(merged_limb)]);

    builder.switch_to_block(out_merge);
    builder.seal_block(out_merge);
    let out_limb = builder.block_params(out_merge)[0];
    bigint_store_limb(builder, result, idx, out_limb);
    let next = builder.ins().iadd_imm(idx, 1);
    builder.ins().jump(loop_block, &[BlockArg::Value(next)]);
    builder.switch_to_block(done_block);
    builder.seal_block(done_block);
    builder.seal_block(loop_block);
    bigint_normalize(builder, result);
    builder.ins().jump(merge, &[BlockArg::Value(result)]);

    builder.switch_to_block(merge);
    builder.seal_block(merge);
    builder.block_params(merge)[0]
}

fn bigint_mul_abs(
    builder: &mut FunctionBuilder,
    alloc_ref: FuncRef,
    lhs: Value,
    rhs: Value,
) -> Value {
    let lhs_len = bigint_load_len(builder, lhs);
    let rhs_len = bigint_load_len(builder, rhs);
    let cap = builder.ins().iadd(lhs_len, rhs_len);
    let result = bigint_alloc(builder, alloc_ref, cap);
    bigint_store_len(builder, result, cap);

    let init_loop = builder.create_block();
    let init_body = builder.create_block();
    let init_done = builder.create_block();
    let zero = builder.ins().iconst(types::I64, 0);
    builder.append_block_param(init_loop, types::I64);
    builder.ins().jump(init_loop, &[BlockArg::Value(zero)]);

    builder.switch_to_block(init_loop);
    let init_idx = builder.block_params(init_loop)[0];
    let init_more = builder.ins().icmp(IntCC::UnsignedLessThan, init_idx, cap);
    builder.ins().brif(init_more, init_body, &[], init_done, &[]);

    builder.switch_to_block(init_body);
    builder.seal_block(init_body);
    bigint_store_limb(builder, result, init_idx, zero);
    let init_next = builder.ins().iadd_imm(init_idx, 1);
    builder.ins().jump(init_loop, &[BlockArg::Value(init_next)]);

    builder.switch_to_block(init_done);
    builder.seal_block(init_done);
    builder.seal_block(init_loop);

    let outer_loop = builder.create_block();
    let outer_body = builder.create_block();
    let outer_done = builder.create_block();
    builder.append_block_param(outer_loop, types::I64);
    builder.ins().jump(outer_loop, &[BlockArg::Value(zero)]);

    builder.switch_to_block(outer_loop);
    let i = builder.block_params(outer_loop)[0];
    let outer_more = builder.ins().icmp(IntCC::UnsignedLessThan, i, lhs_len);
    builder.ins().brif(outer_more, outer_body, &[], outer_done, &[]);

    builder.switch_to_block(outer_body);
    builder.seal_block(outer_body);
    let lhs_limb = bigint_load_limb(builder, lhs, i);
    let inner_loop = builder.create_block();
    let inner_body = builder.create_block();
    let inner_done = builder.create_block();
    builder.append_block_param(inner_loop, types::I64);
    builder.append_block_param(inner_loop, types::I64);
    builder.append_block_param(inner_done, types::I64);
    builder.ins().jump(inner_loop, &[BlockArg::Value(zero), BlockArg::Value(zero)]);

    builder.switch_to_block(inner_loop);
    let j = builder.block_params(inner_loop)[0];
    let carry = builder.block_params(inner_loop)[1];
    let inner_more = builder.ins().icmp(IntCC::UnsignedLessThan, j, rhs_len);
    builder.ins().brif(inner_more, inner_body, &[], inner_done, &[BlockArg::Value(carry)]);

    builder.switch_to_block(inner_body);
    builder.seal_block(inner_body);
    let rhs_limb = bigint_load_limb(builder, rhs, j);
    let idx = builder.ins().iadd(i, j);
    let existing = bigint_load_limb(builder, result, idx);
    let prod = builder.ins().imul(lhs_limb, rhs_limb);
    let tmp = builder.ins().iadd(existing, prod);
    let total = builder.ins().iadd(tmp, carry);
    let mask = builder.ins().iconst(types::I64, 0xffff_ffff);
    let low = builder.ins().band(total, mask);
    bigint_store_limb(builder, result, idx, low);
    let next_carry = builder.ins().ushr_imm(total, 32);
    let next_j = builder.ins().iadd_imm(j, 1);
    builder.ins().jump(inner_loop, &[BlockArg::Value(next_j), BlockArg::Value(next_carry)]);

    builder.switch_to_block(inner_done);
    builder.seal_block(inner_done);
    builder.seal_block(inner_loop);
    let final_carry = builder.block_params(inner_done)[0];
    let carry_loop = builder.create_block();
    let carry_body = builder.create_block();
    let carry_done = builder.create_block();
    let carry_idx0 = builder.ins().iadd(i, rhs_len);
    builder.append_block_param(carry_loop, types::I64);
    builder.append_block_param(carry_loop, types::I64);
    builder.ins().jump(carry_loop, &[BlockArg::Value(carry_idx0), BlockArg::Value(final_carry)]);

    builder.switch_to_block(carry_loop);
    let carry_idx = builder.block_params(carry_loop)[0];
    let carry_val = builder.block_params(carry_loop)[1];
    let carry_more = builder.ins().icmp_imm(IntCC::NotEqual, carry_val, 0);
    builder.ins().brif(carry_more, carry_body, &[], carry_done, &[]);

    builder.switch_to_block(carry_body);
    builder.seal_block(carry_body);
    let existing = bigint_load_limb(builder, result, carry_idx);
    let total = builder.ins().iadd(existing, carry_val);
    let mask = builder.ins().iconst(types::I64, 0xffff_ffff);
    let low = builder.ins().band(total, mask);
    bigint_store_limb(builder, result, carry_idx, low);
    let next_carry = builder.ins().ushr_imm(total, 32);
    let next_idx = builder.ins().iadd_imm(carry_idx, 1);
    builder.ins().jump(carry_loop, &[BlockArg::Value(next_idx), BlockArg::Value(next_carry)]);

    builder.switch_to_block(carry_done);
    builder.seal_block(carry_done);
    builder.seal_block(carry_loop);
    let next_i = builder.ins().iadd_imm(i, 1);
    builder.ins().jump(outer_loop, &[BlockArg::Value(next_i)]);

    builder.switch_to_block(outer_done);
    builder.seal_block(outer_done);
    builder.seal_block(outer_loop);
    bigint_normalize(builder, result);
    result
}

fn define_runtime_fn(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    params: &[Type],
    returns: &[Type],
    build: impl FnOnce(&mut FunctionBuilder, &[Value], &mut Function),
) {
    let mut ctx = module.make_context();
    ctx.func.signature = runtime_sig_with_returns(isa, params, returns);
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
    oom_host_addr: i64,
    bigint_enabled: bool,
    list_enabled: bool,
    list_mutation_enabled: bool,
    map_enabled: bool,
) -> RuntimeBuiltins {
    let ids = declare_runtime_function_ids(
        module,
        isa,
        bigint_enabled,
        list_enabled,
        list_mutation_enabled,
        map_enabled,
    );
    define_rt_host_oom_shim(module, isa, flags, ids.oom, oom_host_addr);
    define_rt_alloc_from_addrs(
        module,
        isa,
        flags,
        ids.alloc,
        ids.oom,
        arena_base_addr,
        arena_offset_addr,
    );
    define_runtime_operations(
        module,
        isa,
        flags,
        &ids,
        bigint_enabled,
        list_enabled,
        list_mutation_enabled,
        map_enabled,
    );
    ids.builtins
}

fn define_rt_host_print_shim(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    host_addr: i64,
) {
    define_runtime_scalar_fn(module, isa, flags, id, &[types::I64], |b, p, func| {
        let sig_ref = func.import_signature(runtime_sig(isa, &[types::I64]));
        let callee = b.ins().iconst(types::I64, host_addr);
        let call = b.ins().call_indirect(sig_ref, callee, &[p[0]]);
        let out = b.inst_results(call)[0];
        b.ins().return_(&[out]);
    });
}

fn define_rt_host_oom_shim(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    host_addr: i64,
) {
    define_runtime_scalar_fn(module, isa, flags, id, &[], |b, _p, func| {
        let sig_ref = func.import_signature(runtime_sig(isa, &[]));
        let callee = b.ins().iconst(types::I64, host_addr);
        let call = b.ins().call_indirect(sig_ref, callee, &[]);
        let out = b.inst_results(call)[0];
        b.ins().return_(&[out]);
    });
}

fn define_rt_host_oom_import_wrapper(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    host_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_scalar_fn(module, isa, flags, id, &[], |b, _p, func| {
        let host_ref = unsafe { (&mut *module_ptr).declare_func_in_func(host_id, func) };
        let call = b.ins().call(host_ref, &[]);
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
    define_runtime_scalar_fn(
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
    oom_id: FuncId,
    data: &RuntimeData,
) {
    let module_ptr: *mut _ = module;
    define_runtime_scalar_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
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
        let ok_block = b.create_block();
        let oom_block = b.create_block();
        b.ins().brif(ok, ok_block, &[], oom_block, &[]);

        b.switch_to_block(oom_block);
        b.seal_block(oom_block);
        let oom_ref = unsafe { (&mut *module_ptr).declare_func_in_func(oom_id, func) };
        let _ = b.ins().call(oom_ref, &[]);
        b.ins().trap(TrapCode::HEAP_OUT_OF_BOUNDS);

        b.switch_to_block(ok_block);
        b.seal_block(ok_block);
        b.ins().store(MemFlags::new(), new_off, off_addr, 0);
        b.ins().return_(&[aligned]);
    });
}

fn define_rt_alloc_from_addrs(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    oom_id: FuncId,
    arena_base_addr: i64,
    arena_offset_addr: i64,
) {
    let module_ptr: *mut _ = module;
    define_runtime_scalar_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
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
        let ok_block = b.create_block();
        let oom_block = b.create_block();
        b.ins().brif(ok, ok_block, &[], oom_block, &[]);

        b.switch_to_block(oom_block);
        b.seal_block(oom_block);
        let oom_ref = unsafe { (&mut *module_ptr).declare_func_in_func(oom_id, func) };
        let _ = b.ins().call(oom_ref, &[]);
        b.ins().trap(TrapCode::HEAP_OUT_OF_BOUNDS);

        b.switch_to_block(ok_block);
        b.seal_block(ok_block);
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
    let _ = alloc_id;
    define_runtime_pair_fn(module, isa, flags, id, &[types::I64], |b, p, _| {
        let tag = b.ins().iconst(types::I64, TAG_INT);
        b.ins().return_(&[tag, p[0]]);
    });
}

fn define_rt_pair_print_wrapper(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    scalar_print_id: FuncId,
    box_value_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
        let box_ref = unsafe { (&mut *module_ptr).declare_func_in_func(box_value_id, func) };
        let print_ref = unsafe { (&mut *module_ptr).declare_func_in_func(scalar_print_id, func) };
        let boxed_call = b.ins().call(box_ref, &[p[0], p[1]]);
        let boxed = b.inst_results(boxed_call)[0];
        let _ = b.ins().call(print_ref, &[boxed]);
        let zero_tag = b.ins().iconst(types::I64, TAG_INT);
        let zero_payload = b.ins().iconst(types::I64, 0);
        b.ins().return_(&[zero_tag, zero_payload]);
    });
}

fn define_rt_box_value(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_scalar_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
        let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
        let size = b.ins().iconst(types::I64, VALUE_SIZE);
        let align = b.ins().iconst(types::I64, 8);
        let call = b.ins().call(alloc_ref, &[size, align]);
        let ptr = b.inst_results(call)[0];
        let tag = b.ins().ireduce(types::I8, p[0]);
        b.ins().store(MemFlags::new(), tag, ptr, 0);
        b.ins().store(MemFlags::new(), p[1], ptr, VALUE_PAYLOAD_OFFSET);
        b.ins().return_(&[ptr]);
    });
}

fn define_rt_value_to_i64(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
) {
    define_runtime_scalar_fn(module, isa, flags, id, &[types::I64], |b, p, _| {
        let payload = rt_payload_for_tag(b, p[0], TAG_INT);
        b.ins().return_(&[payload]);
    });
}

fn define_rt_value_is_truthy(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
) {
    define_runtime_scalar_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, _| {
        let tag = p[0];
        let one = b.ins().iconst(types::I64, 1);
        let zero = b.ins().iconst(types::I64, 0);
        let is_int = b.ins().icmp_imm(IntCC::Equal, tag, TAG_INT);
        let int_block = b.create_block();
        let list_block = b.create_block();
        let merge = b.create_block();
        b.append_block_param(merge, types::I64);
        b.ins().brif(is_int, int_block, &[], list_block, &[]);

        b.switch_to_block(int_block);
        b.seal_block(int_block);
        let raw = p[1];
        let nz = b.ins().icmp_imm(IntCC::NotEqual, raw, 0);
        let int_truthy = b.ins().select(nz, one, zero);
        b.ins().jump(merge, &[BlockArg::Value(int_truthy)]);

        b.switch_to_block(list_block);
        b.seal_block(list_block);
        let is_list = b.ins().icmp_imm(IntCC::Equal, tag, TAG_LIST);
        b.ins().trapz(is_list, TrapCode::BAD_CONVERSION_TO_INTEGER);
        let header = p[1];
        let len = b.ins().load(types::I64, MemFlags::new(), header, LIST_LEN_OFFSET);
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
    _to_i64_id: FuncId,
    int_id: FuncId,
    bigint_from_int_id: Option<FuncId>,
    bigint_id: Option<FuncId>,
    op: &str,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let make_int = unsafe { (&mut *module_ptr).declare_func_in_func(int_id, func) };
            let bigint_from_int_ref = bigint_from_int_id
                .map(|id| unsafe { (&mut *module_ptr).declare_func_in_func(id, func) });
            let bigint_ref =
                bigint_id.map(|id| unsafe { (&mut *module_ptr).declare_func_in_func(id, func) });
            let lhs_is_int = b.ins().icmp_imm(IntCC::Equal, p[0], TAG_INT);
            let rhs_is_int = b.ins().icmp_imm(IntCC::Equal, p[2], TAG_INT);
            let both_int = b.ins().band(lhs_is_int, rhs_is_int);
            let int_block = b.create_block();
            let non_int_block = b.create_block();
            let merge_block = b.create_block();
            b.append_block_param(merge_block, types::I64);
            b.append_block_param(merge_block, types::I64);
            b.ins().brif(both_int, int_block, &[], non_int_block, &[]);

            b.switch_to_block(int_block);
            b.seal_block(int_block);
            let lhs = p[1];
            let rhs = p[3];
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
                "bitand" => b.ins().band(lhs, rhs),
                "bitor" => b.ins().bor(lhs, rhs),
                "bitxor" => b.ins().bxor(lhs, rhs),
                "shl" | "shr" => {
                    let rhs_non_neg = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, rhs, 0);
                    let rhs_lt_width = b.ins().icmp_imm(IntCC::SignedLessThan, rhs, 64);
                    let rhs_in_range = b.ins().band(rhs_non_neg, rhs_lt_width);
                    b.ins().trapz(rhs_in_range, TrapCode::BAD_CONVERSION_TO_INTEGER);
                    if op == "shl" { b.ins().ishl(lhs, rhs) } else { b.ins().sshr(lhs, rhs) }
                }
                _ => unreachable!(),
            };
            let out = b.ins().call(make_int, &[raw]);
            let result_tag = b.inst_results(out)[0];
            let result_payload = b.inst_results(out)[1];
            b.ins()
                .jump(merge_block, &[BlockArg::Value(result_tag), BlockArg::Value(result_payload)]);

            b.switch_to_block(non_int_block);
            b.seal_block(non_int_block);
            if let (Some(bigint_ref), Some(bigint_from_int_ref)) = (bigint_ref, bigint_from_int_ref)
            {
                let lhs_is_bigint = b.ins().icmp_imm(IntCC::Equal, p[0], TAG_BIGINT);
                let rhs_is_bigint = b.ins().icmp_imm(IntCC::Equal, p[2], TAG_BIGINT);
                let trap_block = b.create_block();
                if matches!(op, "shl" | "shr") {
                    let lhs_bigint_rhs_int = b.ins().band(lhs_is_bigint, rhs_is_int);
                    let bigint_block = b.create_block();
                    b.ins().brif(lhs_bigint_rhs_int, bigint_block, &[], trap_block, &[]);

                    b.switch_to_block(bigint_block);
                    b.seal_block(bigint_block);
                    let out = b.ins().call(bigint_ref, &[p[0], p[1], p[2], p[3]]);
                    let result_tag = b.inst_results(out)[0];
                    let result_payload = b.inst_results(out)[1];
                    b.ins().jump(
                        merge_block,
                        &[BlockArg::Value(result_tag), BlockArg::Value(result_payload)],
                    );
                } else {
                    let both_bigint = b.ins().band(lhs_is_bigint, rhs_is_bigint);
                    let bigint_block = b.create_block();
                    let lhs_promote_check_block = b.create_block();
                    let lhs_promote_block = b.create_block();
                    let rhs_promote_check_block = b.create_block();
                    let rhs_promote_block = b.create_block();
                    b.ins().brif(both_bigint, bigint_block, &[], lhs_promote_check_block, &[]);

                    b.switch_to_block(bigint_block);
                    b.seal_block(bigint_block);
                    let out = b.ins().call(bigint_ref, &[p[0], p[1], p[2], p[3]]);
                    let result_tag = b.inst_results(out)[0];
                    let result_payload = b.inst_results(out)[1];
                    b.ins().jump(
                        merge_block,
                        &[BlockArg::Value(result_tag), BlockArg::Value(result_payload)],
                    );

                    b.switch_to_block(lhs_promote_check_block);
                    b.seal_block(lhs_promote_check_block);
                    let lhs_int_rhs_bigint = b.ins().band(lhs_is_int, rhs_is_bigint);
                    b.ins().brif(
                        lhs_int_rhs_bigint,
                        lhs_promote_block,
                        &[],
                        rhs_promote_check_block,
                        &[],
                    );

                    b.switch_to_block(lhs_promote_block);
                    b.seal_block(lhs_promote_block);
                    let lhs_big = b.ins().call(bigint_from_int_ref, &[p[0], p[1]]);
                    let lhs_big_tag = b.inst_results(lhs_big)[0];
                    let lhs_big_payload = b.inst_results(lhs_big)[1];
                    let out = b.ins().call(bigint_ref, &[lhs_big_tag, lhs_big_payload, p[2], p[3]]);
                    let result_tag = b.inst_results(out)[0];
                    let result_payload = b.inst_results(out)[1];
                    b.ins().jump(
                        merge_block,
                        &[BlockArg::Value(result_tag), BlockArg::Value(result_payload)],
                    );

                    b.switch_to_block(rhs_promote_check_block);
                    b.seal_block(rhs_promote_check_block);
                    let rhs_int_lhs_bigint = b.ins().band(lhs_is_bigint, rhs_is_int);
                    b.ins().brif(rhs_int_lhs_bigint, rhs_promote_block, &[], trap_block, &[]);

                    b.switch_to_block(rhs_promote_block);
                    b.seal_block(rhs_promote_block);
                    let rhs_big = b.ins().call(bigint_from_int_ref, &[p[2], p[3]]);
                    let rhs_big_tag = b.inst_results(rhs_big)[0];
                    let rhs_big_payload = b.inst_results(rhs_big)[1];
                    let out = b.ins().call(bigint_ref, &[p[0], p[1], rhs_big_tag, rhs_big_payload]);
                    let result_tag = b.inst_results(out)[0];
                    let result_payload = b.inst_results(out)[1];
                    b.ins().jump(
                        merge_block,
                        &[BlockArg::Value(result_tag), BlockArg::Value(result_payload)],
                    );
                }

                b.switch_to_block(trap_block);
                b.seal_block(trap_block);
                b.ins().trap(TrapCode::BAD_CONVERSION_TO_INTEGER);
            } else {
                b.ins().trap(TrapCode::BAD_CONVERSION_TO_INTEGER);
            }

            b.switch_to_block(merge_block);
            b.seal_block(merge_block);
            let result_tag = b.block_params(merge_block)[0];
            let result_payload = b.block_params(merge_block)[1];
            b.ins().return_(&[result_tag, result_payload]);
        },
    );
}

fn define_rt_bigint_from_int(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
        let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
        let raw = pair_payload_for_tag(b, p[0], p[1], TAG_INT);
        let zero = b.ins().iconst(types::I64, 0);
        let is_zero = b.ins().icmp(IntCC::Equal, raw, zero);
        let zero_block = b.create_block();
        let non_zero_block = b.create_block();
        let merge_block = b.create_block();
        b.append_block_param(merge_block, types::I64);
        b.ins().brif(is_zero, zero_block, &[], non_zero_block, &[]);

        b.switch_to_block(zero_block);
        b.seal_block(zero_block);
        let zero_ptr = bigint_alloc(b, alloc_ref, zero);
        b.ins().jump(merge_block, &[BlockArg::Value(zero_ptr)]);

        b.switch_to_block(non_zero_block);
        b.seal_block(non_zero_block);
        let is_negative = b.ins().icmp_imm(IntCC::SignedLessThan, raw, 0);
        let neg_block = b.create_block();
        let pos_block = b.create_block();
        let sign_merge = b.create_block();
        b.append_block_param(sign_merge, types::I64);
        b.append_block_param(sign_merge, types::I64);
        b.ins().brif(is_negative, neg_block, &[], pos_block, &[]);

        b.switch_to_block(neg_block);
        b.seal_block(neg_block);
        let sign = b.ins().iconst(types::I64, -1);
        let abs = b.ins().isub(zero, raw);
        b.ins().jump(sign_merge, &[BlockArg::Value(sign), BlockArg::Value(abs)]);

        b.switch_to_block(pos_block);
        b.seal_block(pos_block);
        let sign = b.ins().iconst(types::I64, 1);
        b.ins().jump(sign_merge, &[BlockArg::Value(sign), BlockArg::Value(raw)]);

        b.switch_to_block(sign_merge);
        b.seal_block(sign_merge);
        let sign = b.block_params(sign_merge)[0];
        let abs = b.block_params(sign_merge)[1];
        let high = b.ins().ushr_imm(abs, 32);
        let has_high = b.ins().icmp_imm(IntCC::NotEqual, high, 0);
        let high_block = b.create_block();
        let low_block = b.create_block();
        let cap_merge = b.create_block();
        b.append_block_param(cap_merge, types::I64);
        b.ins().brif(has_high, high_block, &[], low_block, &[]);

        b.switch_to_block(high_block);
        b.seal_block(high_block);
        let two = b.ins().iconst(types::I64, 2);
        b.ins().jump(cap_merge, &[BlockArg::Value(two)]);

        b.switch_to_block(low_block);
        b.seal_block(low_block);
        let one = b.ins().iconst(types::I64, 1);
        b.ins().jump(cap_merge, &[BlockArg::Value(one)]);

        b.switch_to_block(cap_merge);
        b.seal_block(cap_merge);
        let cap = b.block_params(cap_merge)[0];
        let header_ptr = bigint_alloc(b, alloc_ref, cap);
        bigint_store_sign(b, header_ptr, sign);
        bigint_store_len(b, header_ptr, cap);
        let mask = b.ins().iconst(types::I64, 0xffff_ffff);
        let low = b.ins().band(abs, mask);
        let zero_index = b.ins().iconst(types::I64, 0);
        bigint_store_limb(b, header_ptr, zero_index, low);
        let one_index = b.ins().iconst(types::I64, 1);
        let has_second = b.ins().icmp_imm(IntCC::Equal, cap, 2);
        let second_block = b.create_block();
        let done_block = b.create_block();
        b.ins().brif(has_second, second_block, &[], done_block, &[]);

        b.switch_to_block(second_block);
        b.seal_block(second_block);
        bigint_store_limb(b, header_ptr, one_index, high);
        b.ins().jump(done_block, &[]);

        b.switch_to_block(done_block);
        b.seal_block(done_block);
        bigint_normalize(b, header_ptr);
        b.ins().jump(merge_block, &[BlockArg::Value(header_ptr)]);

        b.switch_to_block(merge_block);
        b.seal_block(merge_block);
        let tag = b.ins().iconst(types::I64, TAG_BIGINT);
        let ptr = b.block_params(merge_block)[0];
        b.ins().return_(&[tag, ptr]);
    });
}

fn define_rt_bigint_add(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
            let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_BIGINT);
            let rhs_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_BIGINT);
            let lhs_sign = bigint_load_sign(b, lhs_ptr);
            let rhs_sign = bigint_load_sign(b, rhs_ptr);
            let result_ptr = emit_bigint_addsub(b, alloc_ref, lhs_ptr, lhs_sign, rhs_ptr, rhs_sign);
            let tag = b.ins().iconst(types::I64, TAG_BIGINT);
            b.ins().return_(&[tag, result_ptr]);
        },
    );
}

fn define_rt_bigint_subtract(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
            let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_BIGINT);
            let rhs_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_BIGINT);
            let lhs_sign = bigint_load_sign(b, lhs_ptr);
            let rhs_sign = bigint_load_sign(b, rhs_ptr);
            let zero = b.ins().iconst(types::I64, 0);
            let rhs_is_zero = b.ins().icmp(IntCC::Equal, rhs_sign, zero);
            let neg_block = b.create_block();
            let zero_block = b.create_block();
            let merge = b.create_block();
            b.append_block_param(merge, types::I64);
            b.ins().brif(rhs_is_zero, zero_block, &[], neg_block, &[]);

            b.switch_to_block(zero_block);
            b.seal_block(zero_block);
            b.ins().jump(merge, &[BlockArg::Value(zero)]);

            b.switch_to_block(neg_block);
            b.seal_block(neg_block);
            let neg_rhs_sign = b.ins().isub(zero, rhs_sign);
            b.ins().jump(merge, &[BlockArg::Value(neg_rhs_sign)]);

            b.switch_to_block(merge);
            b.seal_block(merge);
            let effective_rhs_sign = b.block_params(merge)[0];
            let result_ptr =
                emit_bigint_addsub(b, alloc_ref, lhs_ptr, lhs_sign, rhs_ptr, effective_rhs_sign);
            let tag = b.ins().iconst(types::I64, TAG_BIGINT);
            b.ins().return_(&[tag, result_ptr]);
        },
    );
}

fn define_rt_bigint_compare(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    int_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let make_int = unsafe { (&mut *module_ptr).declare_func_in_func(int_id, func) };
            let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_BIGINT);
            let rhs_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_BIGINT);
            let lhs_sign = bigint_load_sign(b, lhs_ptr);
            let rhs_sign = bigint_load_sign(b, rhs_ptr);
            let raw = bigint_signed_cmp(b, lhs_ptr, lhs_sign, rhs_ptr, rhs_sign);
            let out = b.ins().call(make_int, &[raw]);
            let result_tag = b.inst_results(out)[0];
            let result_payload = b.inst_results(out)[1];
            b.ins().return_(&[result_tag, result_payload]);
        },
    );
}

fn define_rt_bigint_multiply(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
            let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_BIGINT);
            let rhs_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_BIGINT);
            let lhs_sign = bigint_load_sign(b, lhs_ptr);
            let rhs_sign = bigint_load_sign(b, rhs_ptr);

            let merge = b.create_block();
            b.append_block_param(merge, types::I64);

            let lhs_zero = b.ins().icmp_imm(IntCC::Equal, lhs_sign, 0);
            let rhs_zero = b.ins().icmp_imm(IntCC::Equal, rhs_sign, 0);
            let either_zero = b.ins().bor(lhs_zero, rhs_zero);
            let zero_block = b.create_block();
            let mul_block = b.create_block();
            b.ins().brif(either_zero, zero_block, &[], mul_block, &[]);

            b.switch_to_block(zero_block);
            b.seal_block(zero_block);
            let zero_ptr = bigint_zero(b, alloc_ref);
            b.ins().jump(merge, &[BlockArg::Value(zero_ptr)]);

            b.switch_to_block(mul_block);
            b.seal_block(mul_block);
            let result_ptr = bigint_mul_abs(b, alloc_ref, lhs_ptr, rhs_ptr);
            let signs_equal = b.ins().icmp(IntCC::Equal, lhs_sign, rhs_sign);
            let pos_one = b.ins().iconst(types::I64, 1);
            let neg_one = b.ins().iconst(types::I64, -1);
            let out_sign = b.ins().select(signs_equal, pos_one, neg_one);
            bigint_store_sign(b, result_ptr, out_sign);
            bigint_normalize(b, result_ptr);
            b.ins().jump(merge, &[BlockArg::Value(result_ptr)]);

            b.switch_to_block(merge);
            b.seal_block(merge);
            let result_ptr = b.block_params(merge)[0];
            let tag = b.ins().iconst(types::I64, TAG_BIGINT);
            b.ins().return_(&[tag, result_ptr]);
        },
    );
}

fn define_rt_bigint_divide(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
            let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_BIGINT);
            let rhs_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_BIGINT);
            let lhs_sign = bigint_load_sign(b, lhs_ptr);
            let rhs_sign = bigint_load_sign(b, rhs_ptr);
            let zero = b.ins().iconst(types::I64, 0);
            let one = b.ins().iconst(types::I64, 1);
            let neg_one = b.ins().iconst(types::I64, -1);

            let rhs_is_zero = b.ins().icmp(IntCC::Equal, rhs_sign, zero);
            b.ins().trapnz(rhs_is_zero, TrapCode::INTEGER_DIVISION_BY_ZERO);

            let lhs_is_zero = b.ins().icmp(IntCC::Equal, lhs_sign, zero);
            let zero_block = b.create_block();
            let work_block = b.create_block();
            let done_block = b.create_block();
            b.append_block_param(done_block, types::I64);
            b.ins().brif(lhs_is_zero, zero_block, &[], work_block, &[]);

            b.switch_to_block(zero_block);
            b.seal_block(zero_block);
            let zero_ptr = bigint_zero(b, alloc_ref);
            b.ins().jump(done_block, &[BlockArg::Value(zero_ptr)]);

            b.switch_to_block(work_block);
            b.seal_block(work_block);
            let outer_loop = b.create_block();
            let outer_body = b.create_block();
            let outer_done = b.create_block();
            b.append_block_param(outer_loop, types::I64);
            b.append_block_param(outer_loop, types::I64);
            let quotient0 = bigint_zero(b, alloc_ref);
            b.ins().jump(outer_loop, &[BlockArg::Value(quotient0), BlockArg::Value(lhs_ptr)]);

            b.switch_to_block(outer_loop);
            let quotient = b.block_params(outer_loop)[0];
            let remainder = b.block_params(outer_loop)[1];
            let cmp = bigint_cmp_abs(b, remainder, rhs_ptr);
            let has_more = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, cmp, 0);
            b.ins().brif(has_more, outer_body, &[], outer_done, &[]);

            b.switch_to_block(outer_body);
            b.seal_block(outer_body);
            let inner_loop = b.create_block();
            let inner_body = b.create_block();
            let inner_done = b.create_block();
            b.append_block_param(inner_loop, types::I64);
            b.append_block_param(inner_loop, types::I64);
            let multiple0 = bigint_one(b, alloc_ref);
            b.ins().jump(inner_loop, &[BlockArg::Value(rhs_ptr), BlockArg::Value(multiple0)]);

            b.switch_to_block(inner_loop);
            let current = b.block_params(inner_loop)[0];
            let multiple = b.block_params(inner_loop)[1];
            let doubled = bigint_add_abs(b, alloc_ref, current, current);
            let doubled_cmp = bigint_cmp_abs(b, doubled, remainder);
            let can_double = b.ins().icmp_imm(IntCC::SignedLessThanOrEqual, doubled_cmp, 0);
            b.ins().brif(can_double, inner_body, &[], inner_done, &[]);

            b.switch_to_block(inner_body);
            b.seal_block(inner_body);
            let doubled_multiple = bigint_add_abs(b, alloc_ref, multiple, multiple);
            b.ins()
                .jump(inner_loop, &[BlockArg::Value(doubled), BlockArg::Value(doubled_multiple)]);

            b.switch_to_block(inner_done);
            b.seal_block(inner_done);
            let best_current = b.block_params(inner_loop)[0];
            let best_multiple = b.block_params(inner_loop)[1];
            let next_remainder = bigint_sub_abs(b, alloc_ref, remainder, best_current);
            let next_quotient = bigint_add_abs(b, alloc_ref, quotient, best_multiple);
            b.ins().jump(
                outer_loop,
                &[BlockArg::Value(next_quotient), BlockArg::Value(next_remainder)],
            );

            b.switch_to_block(outer_done);
            let raw_quotient = b.block_params(outer_loop)[0];
            let signs_equal = b.ins().icmp(IntCC::Equal, lhs_sign, rhs_sign);
            let out_sign = b.ins().select(signs_equal, one, neg_one);
            bigint_store_sign(b, raw_quotient, out_sign);
            bigint_normalize(b, raw_quotient);
            b.ins().jump(done_block, &[BlockArg::Value(raw_quotient)]);

            b.seal_block(outer_loop);
            b.seal_block(outer_done);
            b.seal_block(inner_loop);

            b.switch_to_block(done_block);
            b.seal_block(done_block);
            let result_ptr = b.block_params(done_block)[0];
            let tag = b.ins().iconst(types::I64, TAG_BIGINT);
            b.ins().return_(&[tag, result_ptr]);
        },
    );
}

fn define_rt_bigint_modulo(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
            let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_BIGINT);
            let rhs_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_BIGINT);
            let lhs_sign = bigint_load_sign(b, lhs_ptr);
            let rhs_sign = bigint_load_sign(b, rhs_ptr);
            let zero = b.ins().iconst(types::I64, 0);

            let rhs_is_zero = b.ins().icmp(IntCC::Equal, rhs_sign, zero);
            b.ins().trapnz(rhs_is_zero, TrapCode::INTEGER_DIVISION_BY_ZERO);

            let lhs_is_zero = b.ins().icmp(IntCC::Equal, lhs_sign, zero);
            let zero_block = b.create_block();
            let work_block = b.create_block();
            let done_block = b.create_block();
            b.append_block_param(done_block, types::I64);
            b.ins().brif(lhs_is_zero, zero_block, &[], work_block, &[]);

            b.switch_to_block(zero_block);
            b.seal_block(zero_block);
            let zero_ptr = bigint_zero(b, alloc_ref);
            b.ins().jump(done_block, &[BlockArg::Value(zero_ptr)]);

            b.switch_to_block(work_block);
            b.seal_block(work_block);
            let outer_loop = b.create_block();
            let outer_body = b.create_block();
            let outer_done = b.create_block();
            b.append_block_param(outer_loop, types::I64);
            b.ins().jump(outer_loop, &[BlockArg::Value(lhs_ptr)]);

            b.switch_to_block(outer_loop);
            let remainder = b.block_params(outer_loop)[0];
            let cmp = bigint_cmp_abs(b, remainder, rhs_ptr);
            let has_more = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, cmp, 0);
            b.ins().brif(has_more, outer_body, &[], outer_done, &[]);

            b.switch_to_block(outer_body);
            b.seal_block(outer_body);
            let inner_loop = b.create_block();
            let inner_body = b.create_block();
            let inner_done = b.create_block();
            b.append_block_param(inner_loop, types::I64);
            let current0 = rhs_ptr;
            b.ins().jump(inner_loop, &[BlockArg::Value(current0)]);

            b.switch_to_block(inner_loop);
            let current = b.block_params(inner_loop)[0];
            let doubled = bigint_add_abs(b, alloc_ref, current, current);
            let doubled_cmp = bigint_cmp_abs(b, doubled, remainder);
            let can_double = b.ins().icmp_imm(IntCC::SignedLessThanOrEqual, doubled_cmp, 0);
            b.ins().brif(can_double, inner_body, &[], inner_done, &[]);

            b.switch_to_block(inner_body);
            b.seal_block(inner_body);
            b.ins().jump(inner_loop, &[BlockArg::Value(doubled)]);

            b.switch_to_block(inner_done);
            b.seal_block(inner_done);
            let best_current = b.block_params(inner_loop)[0];
            let next_remainder = bigint_sub_abs(b, alloc_ref, remainder, best_current);
            b.ins().jump(outer_loop, &[BlockArg::Value(next_remainder)]);

            b.switch_to_block(outer_done);
            let raw_remainder = b.block_params(outer_loop)[0];
            bigint_store_sign(b, raw_remainder, lhs_sign);
            bigint_normalize(b, raw_remainder);
            b.ins().jump(done_block, &[BlockArg::Value(raw_remainder)]);

            b.seal_block(outer_loop);
            b.seal_block(outer_done);
            b.seal_block(inner_loop);

            b.switch_to_block(done_block);
            b.seal_block(done_block);
            let result_ptr = b.block_params(done_block)[0];
            let tag = b.ins().iconst(types::I64, TAG_BIGINT);
            b.ins().return_(&[tag, result_ptr]);
        },
    );
}

fn define_rt_bigint_bitand(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
            let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_BIGINT);
            let rhs_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_BIGINT);
            bigint_trap_if_negative(b, lhs_ptr);
            bigint_trap_if_negative(b, rhs_ptr);
            let result_ptr = bigint_bitwise_abs(b, alloc_ref, lhs_ptr, rhs_ptr, "bitand");
            let tag = b.ins().iconst(types::I64, TAG_BIGINT);
            b.ins().return_(&[tag, result_ptr]);
        },
    );
}

fn define_rt_bigint_bitor(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
            let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_BIGINT);
            let rhs_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_BIGINT);
            bigint_trap_if_negative(b, lhs_ptr);
            bigint_trap_if_negative(b, rhs_ptr);
            let result_ptr = bigint_bitwise_abs(b, alloc_ref, lhs_ptr, rhs_ptr, "bitor");
            let tag = b.ins().iconst(types::I64, TAG_BIGINT);
            b.ins().return_(&[tag, result_ptr]);
        },
    );
}

fn define_rt_bigint_bitxor(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
            let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_BIGINT);
            let rhs_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_BIGINT);
            bigint_trap_if_negative(b, lhs_ptr);
            bigint_trap_if_negative(b, rhs_ptr);
            let result_ptr = bigint_bitwise_abs(b, alloc_ref, lhs_ptr, rhs_ptr, "bitxor");
            let tag = b.ins().iconst(types::I64, TAG_BIGINT);
            b.ins().return_(&[tag, result_ptr]);
        },
    );
}

fn define_rt_bigint_shl(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
            let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_BIGINT);
            let shift = pair_payload_for_tag(b, p[2], p[3], TAG_INT);
            let non_neg = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, shift, 0);
            b.ins().trapz(non_neg, TrapCode::BAD_CONVERSION_TO_INTEGER);
            bigint_trap_if_negative(b, lhs_ptr);
            let result_ptr = bigint_shift_left(b, alloc_ref, lhs_ptr, shift);
            let tag = b.ins().iconst(types::I64, TAG_BIGINT);
            b.ins().return_(&[tag, result_ptr]);
        },
    );
}

fn define_rt_bigint_shr(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let alloc_ref = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
            let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_BIGINT);
            let shift = pair_payload_for_tag(b, p[2], p[3], TAG_INT);
            let non_neg = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, shift, 0);
            b.ins().trapz(non_neg, TrapCode::BAD_CONVERSION_TO_INTEGER);
            bigint_trap_if_negative(b, lhs_ptr);
            let result_ptr = bigint_shift_right(b, alloc_ref, lhs_ptr, shift);
            let tag = b.ins().iconst(types::I64, TAG_BIGINT);
            b.ins().return_(&[tag, result_ptr]);
        },
    );
}

fn define_rt_compare_op(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    _to_i64_id: FuncId,
    int_id: FuncId,
    bigint_from_int_id: Option<FuncId>,
    bigint_compare_id: Option<FuncId>,
    cc: IntCC,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let make_int = unsafe { (&mut *module_ptr).declare_func_in_func(int_id, func) };
            let bigint_from_int_ref = bigint_from_int_id
                .map(|id| unsafe { (&mut *module_ptr).declare_func_in_func(id, func) });
            let bigint_compare_ref = bigint_compare_id
                .map(|id| unsafe { (&mut *module_ptr).declare_func_in_func(id, func) });
            let one = b.ins().iconst(types::I64, 1);
            let zero = b.ins().iconst(types::I64, 0);
            let is_equality = matches!(cc, IntCC::Equal | IntCC::NotEqual);
            let neq_result = matches!(cc, IntCC::NotEqual);
            let lhs_is_int = b.ins().icmp_imm(IntCC::Equal, p[0], TAG_INT);
            let rhs_is_int = b.ins().icmp_imm(IntCC::Equal, p[2], TAG_INT);
            let both_int = b.ins().band(lhs_is_int, rhs_is_int);
            let int_block = b.create_block();
            let non_int_block = b.create_block();
            let merge = b.create_block();
            let trap_block = b.create_block();
            b.append_block_param(merge, types::I64);
            b.ins().brif(both_int, int_block, &[], non_int_block, &[]);

            b.switch_to_block(int_block);
            b.seal_block(int_block);
            let int_cmp = b.ins().icmp(cc, p[1], p[3]);
            let int_raw = b.ins().select(int_cmp, one, zero);
            b.ins().jump(merge, &[BlockArg::Value(int_raw)]);

            b.switch_to_block(non_int_block);
            b.seal_block(non_int_block);
            if is_equality {
                let lhs_is_string = b.ins().icmp_imm(IntCC::Equal, p[0], TAG_STRING);
                let rhs_is_string = b.ins().icmp_imm(IntCC::Equal, p[2], TAG_STRING);
                let both_string = b.ins().band(lhs_is_string, rhs_is_string);
                let any_string = b.ins().bor(lhs_is_string, rhs_is_string);
                let string_block = b.create_block();
                let string_mixed_block = b.create_block();
                let after_string_block = b.create_block();
                b.ins().brif(both_string, string_block, &[], string_mixed_block, &[]);

                b.switch_to_block(string_block);
                b.seal_block(string_block);
                let lhs_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_STRING);
                let rhs_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_STRING);
                let string_eq = string_eq_bytes(b, lhs_ptr, rhs_ptr);
                let string_raw = if neq_result { b.ins().bxor(string_eq, one) } else { string_eq };
                b.ins().jump(merge, &[BlockArg::Value(string_raw)]);

                b.switch_to_block(string_mixed_block);
                b.seal_block(string_mixed_block);
                b.ins().brif(
                    any_string,
                    merge,
                    &[BlockArg::Value(if neq_result { one } else { zero })],
                    after_string_block,
                    &[],
                );

                b.switch_to_block(after_string_block);
                b.seal_block(after_string_block);
            }
            if let (Some(bigint_compare_ref), Some(bigint_from_int_ref)) =
                (bigint_compare_ref, bigint_from_int_ref)
            {
                let lhs_is_bigint = b.ins().icmp_imm(IntCC::Equal, p[0], TAG_BIGINT);
                let rhs_is_bigint = b.ins().icmp_imm(IntCC::Equal, p[2], TAG_BIGINT);
                let both_bigint = b.ins().band(lhs_is_bigint, rhs_is_bigint);
                let bigint_block = b.create_block();
                let lhs_promote_check_block = b.create_block();
                let lhs_promote_block = b.create_block();
                let rhs_promote_check_block = b.create_block();
                let rhs_promote_block = b.create_block();
                b.ins().brif(both_bigint, bigint_block, &[], lhs_promote_check_block, &[]);

                b.switch_to_block(bigint_block);
                b.seal_block(bigint_block);
                let cmp_call = b.ins().call(bigint_compare_ref, &[p[0], p[1], p[2], p[3]]);
                let cmp_raw = b.inst_results(cmp_call)[1];
                let cmp = b.ins().icmp_imm(cc, cmp_raw, 0);
                let bigint_raw = b.ins().select(cmp, one, zero);
                b.ins().jump(merge, &[BlockArg::Value(bigint_raw)]);

                b.switch_to_block(lhs_promote_check_block);
                b.seal_block(lhs_promote_check_block);
                let lhs_int_rhs_bigint = b.ins().band(lhs_is_int, rhs_is_bigint);
                b.ins().brif(
                    lhs_int_rhs_bigint,
                    lhs_promote_block,
                    &[],
                    rhs_promote_check_block,
                    &[],
                );

                b.switch_to_block(lhs_promote_block);
                b.seal_block(lhs_promote_block);
                let lhs_big = b.ins().call(bigint_from_int_ref, &[p[0], p[1]]);
                let lhs_big_tag = b.inst_results(lhs_big)[0];
                let lhs_big_payload = b.inst_results(lhs_big)[1];
                let cmp_call =
                    b.ins().call(bigint_compare_ref, &[lhs_big_tag, lhs_big_payload, p[2], p[3]]);
                let cmp_raw = b.inst_results(cmp_call)[1];
                let cmp = b.ins().icmp_imm(cc, cmp_raw, 0);
                let bigint_raw = b.ins().select(cmp, one, zero);
                b.ins().jump(merge, &[BlockArg::Value(bigint_raw)]);

                b.switch_to_block(rhs_promote_check_block);
                b.seal_block(rhs_promote_check_block);
                let rhs_int_lhs_bigint = b.ins().band(lhs_is_bigint, rhs_is_int);
                b.ins().brif(rhs_int_lhs_bigint, rhs_promote_block, &[], trap_block, &[]);

                b.switch_to_block(rhs_promote_block);
                b.seal_block(rhs_promote_block);
                let rhs_big = b.ins().call(bigint_from_int_ref, &[p[2], p[3]]);
                let rhs_big_tag = b.inst_results(rhs_big)[0];
                let rhs_big_payload = b.inst_results(rhs_big)[1];
                let cmp_call =
                    b.ins().call(bigint_compare_ref, &[p[0], p[1], rhs_big_tag, rhs_big_payload]);
                let cmp_raw = b.inst_results(cmp_call)[1];
                let cmp = b.ins().icmp_imm(cc, cmp_raw, 0);
                let bigint_raw = b.ins().select(cmp, one, zero);
                b.ins().jump(merge, &[BlockArg::Value(bigint_raw)]);
            } else {
                b.ins().jump(trap_block, &[]);
            }

            b.switch_to_block(merge);
            b.seal_block(merge);
            let raw = b.block_params(merge)[0];
            let out = b.ins().call(make_int, &[raw]);
            let result_tag = b.inst_results(out)[0];
            let result_payload = b.inst_results(out)[1];
            b.ins().return_(&[result_tag, result_payload]);

            b.switch_to_block(trap_block);
            b.seal_block(trap_block);
            b.ins().trapnz(one, TrapCode::BAD_CONVERSION_TO_INTEGER);
            let zero_pair = b.ins().iconst(types::I64, 0);
            b.ins().return_(&[zero_pair, zero_pair]);
        },
    );
}

fn define_rt_map_new(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(module, isa, flags, id, &[], |b, _p, func| {
        let alloc = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
        let cap = b.ins().iconst(types::I64, 64);
        let entry_size = b.ins().iconst(types::I64, MAP_ENTRY_SIZE);
        let data_bytes = b.ins().imul(cap, entry_size);
        let align = b.ins().iconst(types::I64, 8);
        let data_call = b.ins().call(alloc, &[data_bytes, align]);
        let entries_ptr = b.inst_results(data_call)[0];

        let header_size = b.ins().iconst(types::I64, MAP_HEADER_SIZE);
        let header_call = b.ins().call(alloc, &[header_size, align]);
        let header_ptr = b.inst_results(header_call)[0];
        b.ins().store(MemFlags::new(), entries_ptr, header_ptr, MAP_PTR_OFFSET);
        let zero = b.ins().iconst(types::I64, 0);
        b.ins().store(MemFlags::new(), zero, header_ptr, MAP_LEN_OFFSET);
        b.ins().store(MemFlags::new(), cap, header_ptr, MAP_CAP_OFFSET);

        let loop_block = b.create_block();
        let body_block = b.create_block();
        let done_block = b.create_block();
        b.append_block_param(loop_block, types::I64);
        b.ins().jump(loop_block, &[BlockArg::Value(zero)]);

        b.switch_to_block(loop_block);
        let idx = b.block_params(loop_block)[0];
        let more = b.ins().icmp(IntCC::UnsignedLessThan, idx, cap);
        b.ins().brif(more, body_block, &[], done_block, &[]);

        b.switch_to_block(body_block);
        let entry_ptr = map_entry_ptr(b, entries_ptr, idx);
        b.ins().store(MemFlags::new(), zero, entry_ptr, MAP_ENTRY_STATE_OFFSET);
        let next = b.ins().iadd_imm(idx, 1);
        b.ins().jump(loop_block, &[BlockArg::Value(next)]);
        b.seal_block(body_block);

        b.switch_to_block(done_block);
        let tag = b.ins().iconst(types::I64, TAG_MAP);
        b.ins().return_(&[tag, header_ptr]);
        b.seal_block(done_block);
        b.seal_block(loop_block);
    });
}

fn define_rt_map_len(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    int_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
        let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_MAP);
        let len = map_load_len(b, header_ptr);
        let make_int = unsafe { (&mut *module_ptr).declare_func_in_func(int_id, func) };
        let out = b.ins().call(make_int, &[len]);
        let result_tag = b.inst_results(out)[0];
        let result_payload = b.inst_results(out)[1];
        b.ins().return_(&[result_tag, result_payload]);
    });
}

fn define_rt_map_has(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    int_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_MAP);
            let key_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_STRING);
            let entries_ptr = map_load_ptr(b, header_ptr);
            let len = map_load_len(b, header_ptr);
            let zero = b.ins().iconst(types::I64, 0);
            let one = b.ins().iconst(types::I64, 1);

            let loop_block = b.create_block();
            let body_block = b.create_block();
            let found_block = b.create_block();
            let done_block = b.create_block();
            b.append_block_param(loop_block, types::I64);
            b.append_block_param(done_block, types::I64);
            b.ins().jump(loop_block, &[BlockArg::Value(zero)]);

            b.switch_to_block(loop_block);
            let idx = b.block_params(loop_block)[0];
            let more = b.ins().icmp(IntCC::UnsignedLessThan, idx, len);
            b.ins().brif(more, body_block, &[], done_block, &[BlockArg::Value(zero)]);

            b.switch_to_block(body_block);
            let entry_ptr = map_entry_ptr(b, entries_ptr, idx);
            let state =
                b.ins().load(types::I64, MemFlags::new(), entry_ptr, MAP_ENTRY_STATE_OFFSET);
            let occupied = b.ins().icmp_imm(IntCC::Equal, state, MAP_ENTRY_OCCUPIED);
            let skip_block = b.create_block();
            let cmp_block = b.create_block();
            b.ins().brif(occupied, cmp_block, &[], skip_block, &[]);
            b.seal_block(body_block);

            b.switch_to_block(cmp_block);
            let stored_key =
                b.ins().load(types::I64, MemFlags::new(), entry_ptr, MAP_ENTRY_KEY_OFFSET);
            let equal = string_eq_bytes(b, stored_key, key_ptr);
            b.ins().brif(equal, found_block, &[], skip_block, &[]);
            b.seal_block(cmp_block);

            b.switch_to_block(found_block);
            b.ins().jump(done_block, &[BlockArg::Value(one)]);
            b.seal_block(found_block);

            b.switch_to_block(skip_block);
            let next = b.ins().iadd_imm(idx, 1);
            b.ins().jump(loop_block, &[BlockArg::Value(next)]);
            b.seal_block(skip_block);

            b.switch_to_block(done_block);
            let raw = b.block_params(done_block)[0];
            let make_int = unsafe { (&mut *module_ptr).declare_func_in_func(int_id, func) };
            let out = b.ins().call(make_int, &[raw]);
            let result_tag = b.inst_results(out)[0];
            let result_payload = b.inst_results(out)[1];
            b.ins().return_(&[result_tag, result_payload]);
            b.seal_block(done_block);
            b.seal_block(loop_block);
        },
    );
}

fn define_rt_map_get(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
) {
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, _func| {
            let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_MAP);
            let key_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_STRING);
            let entries_ptr = map_load_ptr(b, header_ptr);
            let len = map_load_len(b, header_ptr);
            let zero = b.ins().iconst(types::I64, 0);

            let loop_block = b.create_block();
            let body_block = b.create_block();
            let found_block = b.create_block();
            let trap_block = b.create_block();
            b.append_block_param(loop_block, types::I64);
            b.ins().jump(loop_block, &[BlockArg::Value(zero)]);

            b.switch_to_block(loop_block);
            let idx = b.block_params(loop_block)[0];
            let more = b.ins().icmp(IntCC::UnsignedLessThan, idx, len);
            b.ins().brif(more, body_block, &[], trap_block, &[]);

            b.switch_to_block(body_block);
            let entry_ptr = map_entry_ptr(b, entries_ptr, idx);
            let state =
                b.ins().load(types::I64, MemFlags::new(), entry_ptr, MAP_ENTRY_STATE_OFFSET);
            let occupied = b.ins().icmp_imm(IntCC::Equal, state, MAP_ENTRY_OCCUPIED);
            let skip_block = b.create_block();
            let cmp_block = b.create_block();
            b.ins().brif(occupied, cmp_block, &[], skip_block, &[]);
            b.seal_block(body_block);

            b.switch_to_block(cmp_block);
            let stored_key =
                b.ins().load(types::I64, MemFlags::new(), entry_ptr, MAP_ENTRY_KEY_OFFSET);
            let equal = string_eq_bytes(b, stored_key, key_ptr);
            b.ins().brif(equal, found_block, &[], skip_block, &[]);
            b.seal_block(cmp_block);

            b.switch_to_block(found_block);
            let tag =
                b.ins().load(types::I64, MemFlags::new(), entry_ptr, MAP_ENTRY_VALUE_TAG_OFFSET);
            let payload = b.ins().load(
                types::I64,
                MemFlags::new(),
                entry_ptr,
                MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            );
            b.ins().return_(&[tag, payload]);
            b.seal_block(found_block);

            b.switch_to_block(skip_block);
            let next = b.ins().iadd_imm(idx, 1);
            b.ins().jump(loop_block, &[BlockArg::Value(next)]);
            b.seal_block(skip_block);

            b.switch_to_block(trap_block);
            let one = b.ins().iconst(types::I64, 1);
            b.ins().trapnz(one, TrapCode::HEAP_OUT_OF_BOUNDS);
            b.ins().return_(&[zero, zero]);
            b.seal_block(trap_block);
            b.seal_block(loop_block);
        },
    );
}

fn define_rt_map_set(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    _alloc_id: FuncId,
    _memcpy_id: FuncId,
) {
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64, types::I64, types::I64],
        |b, p, _func| {
            let map_tag = p[0];
            let map_payload = p[1];
            let key_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_STRING);
            let value_tag = p[4];
            let value_payload = p[5];
            let header_ptr = pair_payload_for_tag(b, map_tag, map_payload, TAG_MAP);
            let entries_ptr = map_load_ptr(b, header_ptr);
            let len = map_load_len(b, header_ptr);
            let cap = map_load_cap(b, header_ptr);
            let zero = b.ins().iconst(types::I64, 0);

            let loop_block = b.create_block();
            let body_block = b.create_block();
            let insert_block = b.create_block();
            let done_block = b.create_block();
            let full_block = b.create_block();
            b.append_block_param(loop_block, types::I64);
            b.ins().jump(loop_block, &[BlockArg::Value(zero)]);

            b.switch_to_block(loop_block);
            let idx = b.block_params(loop_block)[0];
            let more = b.ins().icmp(IntCC::UnsignedLessThan, idx, len);
            b.ins().brif(more, body_block, &[], insert_block, &[]);

            b.switch_to_block(body_block);
            let entry_ptr = map_entry_ptr(b, entries_ptr, idx);
            let stored_key =
                b.ins().load(types::I64, MemFlags::new(), entry_ptr, MAP_ENTRY_KEY_OFFSET);
            let equal = string_eq_bytes(b, stored_key, key_ptr);
            let next_block = b.create_block();
            let update_block = b.create_block();
            b.ins().brif(equal, update_block, &[], next_block, &[]);
            b.seal_block(body_block);

            b.switch_to_block(update_block);
            b.ins().store(MemFlags::new(), value_tag, entry_ptr, MAP_ENTRY_VALUE_TAG_OFFSET);
            b.ins().store(
                MemFlags::new(),
                value_payload,
                entry_ptr,
                MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            );
            b.ins().return_(&[map_tag, map_payload]);
            b.seal_block(update_block);

            b.switch_to_block(next_block);
            let next = b.ins().iadd_imm(idx, 1);
            b.ins().jump(loop_block, &[BlockArg::Value(next)]);
            b.seal_block(next_block);

            b.switch_to_block(insert_block);
            let has_room = b.ins().icmp(IntCC::UnsignedLessThan, len, cap);
            b.ins().brif(has_room, done_block, &[], full_block, &[]);
            b.seal_block(insert_block);

            b.switch_to_block(done_block);
            let entry_ptr = map_entry_ptr(b, entries_ptr, len);
            let zero_hash = b.ins().iconst(types::I64, 0);
            let occupied = b.ins().iconst(types::I64, MAP_ENTRY_OCCUPIED);
            b.ins().store(MemFlags::new(), zero_hash, entry_ptr, MAP_ENTRY_HASH_OFFSET);
            b.ins().store(MemFlags::new(), key_ptr, entry_ptr, MAP_ENTRY_KEY_OFFSET);
            b.ins().store(MemFlags::new(), value_tag, entry_ptr, MAP_ENTRY_VALUE_TAG_OFFSET);
            b.ins().store(
                MemFlags::new(),
                value_payload,
                entry_ptr,
                MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            );
            b.ins().store(MemFlags::new(), occupied, entry_ptr, MAP_ENTRY_STATE_OFFSET);
            let new_len = b.ins().iadd_imm(len, 1);
            b.ins().store(MemFlags::new(), new_len, header_ptr, MAP_LEN_OFFSET);
            b.ins().return_(&[map_tag, map_payload]);
            b.seal_block(done_block);

            b.switch_to_block(full_block);
            let one = b.ins().iconst(types::I64, 1);
            b.ins().trapnz(one, TrapCode::HEAP_OUT_OF_BOUNDS);
            b.ins().return_(&[zero, zero]);
            b.seal_block(full_block);
            b.seal_block(loop_block);
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
    define_runtime_pair_fn(module, isa, flags, id, &[], |b, _p, func| {
        let alloc = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
        let data_bytes = b.ins().iconst(types::I64, LIST_INITIAL_CAPACITY * VALUE_SIZE);
        let align = b.ins().iconst(types::I64, 8);
        let data_call = b.ins().call(alloc, &[data_bytes, align]);
        let data_ptr = b.inst_results(data_call)[0];

        let header_size = b.ins().iconst(types::I64, LIST_HEADER_SIZE);
        let header_call = b.ins().call(alloc, &[header_size, align]);
        let header_ptr = b.inst_results(header_call)[0];
        b.ins().store(MemFlags::new(), data_ptr, header_ptr, LIST_PTR_OFFSET);
        let zero = b.ins().iconst(types::I64, 0);
        b.ins().store(MemFlags::new(), zero, header_ptr, LIST_LEN_OFFSET);
        let cap = b.ins().iconst(types::I64, LIST_INITIAL_CAPACITY);
        b.ins().store(MemFlags::new(), cap, header_ptr, LIST_CAP_OFFSET);

        let tag = b.ins().iconst(types::I64, TAG_LIST);
        b.ins().return_(&[tag, header_ptr]);
    });
}

fn define_rt_list_push(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    alloc_id: FuncId,
    memcpy_id: FuncId,
    _box_value_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let list_tag = p[0];
            let list_payload = p[1];
            let value_tag = p[2];
            let value_payload = p[3];
            let header_ptr = pair_payload_for_tag(b, list_tag, list_payload, TAG_LIST);
            let len = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
            let cap = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_CAP_OFFSET);
            let data_ptr = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);
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
            let bytes = b.ins().ishl_imm(new_cap, 4);
            let align = b.ins().iconst(types::I64, 8);
            let new_data_call = b.ins().call(alloc, &[bytes, align]);
            let new_data_ptr = b.inst_results(new_data_call)[0];
            let old_bytes = b.ins().ishl_imm(len, 4);
            let _ = b.ins().call(memcpy, &[new_data_ptr, data_ptr, old_bytes]);
            b.ins().store(MemFlags::new(), new_data_ptr, header_ptr, LIST_PTR_OFFSET);
            b.ins().store(MemFlags::new(), new_cap, header_ptr, LIST_CAP_OFFSET);
            b.ins().jump(cont_block, &[BlockArg::Value(new_data_ptr)]);

            b.switch_to_block(cont_block);
            b.seal_block(cont_block);
            let active_data_ptr = b.block_params(cont_block)[0];

            let off = b.ins().ishl_imm(len, 4);
            let elem_ptr = b.ins().iadd(active_data_ptr, off);
            b.ins().store(MemFlags::new(), value_tag, elem_ptr, 0);
            b.ins().store(MemFlags::new(), value_payload, elem_ptr, VALUE_PAYLOAD_OFFSET);
            let new_len = b.ins().iadd_imm(len, 1);
            b.ins().store(MemFlags::new(), new_len, header_ptr, LIST_LEN_OFFSET);
            b.ins().return_(&[list_tag, list_payload]);
        },
    );
}

fn define_rt_list_len(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    int_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
        let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_LIST);
        let len = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
        let make_int = unsafe { (&mut *module_ptr).declare_func_in_func(int_id, func) };
        let out = b.ins().call(make_int, &[len]);
        let result_tag = b.inst_results(out)[0];
        let result_payload = b.inst_results(out)[1];
        b.ins().return_(&[result_tag, result_payload]);
    });
}

fn define_rt_list_get(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    _to_i64_id: FuncId,
) {
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, _func| {
            let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_LIST);
            let idx = pair_payload_for_tag(b, p[2], p[3], TAG_INT);
            let non_neg = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, idx, 0);
            b.ins().trapz(non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
            let len = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
            let in_bounds = b.ins().icmp(IntCC::UnsignedLessThan, idx, len);
            b.ins().trapz(in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);
            let data_ptr = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);
            let off = b.ins().ishl_imm(idx, 4);
            let elem_ptr = b.ins().iadd(data_ptr, off);
            let tag = b.ins().load(types::I64, MemFlags::new(), elem_ptr, 0);
            let payload = b.ins().load(types::I64, MemFlags::new(), elem_ptr, VALUE_PAYLOAD_OFFSET);
            b.ins().return_(&[tag, payload]);
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
    define_runtime_pair_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, _| {
        let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_LIST);
        let len = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
        let non_empty = b.ins().icmp_imm(IntCC::NotEqual, len, 0);
        b.ins().trapz(non_empty, TrapCode::HEAP_OUT_OF_BOUNDS);
        let new_len = b.ins().iadd_imm(len, -1);
        b.ins().store(MemFlags::new(), new_len, header_ptr, LIST_LEN_OFFSET);
        let data_ptr = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);
        let off = b.ins().ishl_imm(new_len, 4);
        let elem_ptr = b.ins().iadd(data_ptr, off);
        let tag = b.ins().load(types::I64, MemFlags::new(), elem_ptr, 0);
        let payload = b.ins().load(types::I64, MemFlags::new(), elem_ptr, VALUE_PAYLOAD_OFFSET);
        b.ins().return_(&[tag, payload]);
    });
}

fn define_rt_list_delete(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    _value_to_i64_id: FuncId,
) {
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, _| {
            let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_LIST);
            let idx = pair_payload_for_tag(b, p[2], p[3], TAG_INT);
            let non_neg = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, idx, 0);
            b.ins().trapz(non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
            let len = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
            let in_bounds = b.ins().icmp(IntCC::UnsignedLessThan, idx, len);
            b.ins().trapz(in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);

            let data_ptr = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);
            let removed_off = b.ins().ishl_imm(idx, 4);
            let removed_ptr = b.ins().iadd(data_ptr, removed_off);
            let removed_tag = b.ins().load(types::I64, MemFlags::new(), removed_ptr, 0);
            let removed_payload =
                b.ins().load(types::I64, MemFlags::new(), removed_ptr, VALUE_PAYLOAD_OFFSET);

            let loop_block = b.create_block();
            let body_block = b.create_block();
            let done_block = b.create_block();
            b.append_block_param(loop_block, types::I64);
            let start = b.ins().iadd_imm(idx, 1);
            b.ins().jump(loop_block, &[BlockArg::Value(start)]);

            b.switch_to_block(loop_block);
            let cur = b.block_params(loop_block)[0];
            let more = b.ins().icmp(IntCC::UnsignedLessThan, cur, len);
            b.ins().brif(more, body_block, &[], done_block, &[]);

            b.switch_to_block(body_block);
            let src_off = b.ins().ishl_imm(cur, 4);
            let src_ptr = b.ins().iadd(data_ptr, src_off);
            let moved_tag = b.ins().load(types::I64, MemFlags::new(), src_ptr, 0);
            let moved_payload =
                b.ins().load(types::I64, MemFlags::new(), src_ptr, VALUE_PAYLOAD_OFFSET);
            let dst_index = b.ins().iadd_imm(cur, -1);
            let dst_off = b.ins().ishl_imm(dst_index, 4);
            let dst_ptr = b.ins().iadd(data_ptr, dst_off);
            b.ins().store(MemFlags::new(), moved_tag, dst_ptr, 0);
            b.ins().store(MemFlags::new(), moved_payload, dst_ptr, VALUE_PAYLOAD_OFFSET);
            let next = b.ins().iadd_imm(cur, 1);
            b.ins().jump(loop_block, &[BlockArg::Value(next)]);
            b.seal_block(body_block);

            b.switch_to_block(done_block);
            let new_len = b.ins().iadd_imm(len, -1);
            b.ins().store(MemFlags::new(), new_len, header_ptr, LIST_LEN_OFFSET);
            b.ins().return_(&[removed_tag, removed_payload]);
            b.seal_block(done_block);
            b.seal_block(loop_block);
        },
    );
}

fn define_rt_map_delete(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
) {
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64],
        |b, p, _func| {
            let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_MAP);
            let key_ptr = pair_payload_for_tag(b, p[2], p[3], TAG_STRING);
            let entries_ptr = map_load_ptr(b, header_ptr);
            let len = map_load_len(b, header_ptr);
            let zero = b.ins().iconst(types::I64, 0);

            let loop_block = b.create_block();
            let body_block = b.create_block();
            let found_block = b.create_block();
            let trap_block = b.create_block();
            b.append_block_param(loop_block, types::I64);
            b.ins().jump(loop_block, &[BlockArg::Value(zero)]);

            b.switch_to_block(loop_block);
            let idx = b.block_params(loop_block)[0];
            let more = b.ins().icmp(IntCC::UnsignedLessThan, idx, len);
            b.ins().brif(more, body_block, &[], trap_block, &[]);

            b.switch_to_block(body_block);
            let entry_ptr = map_entry_ptr(b, entries_ptr, idx);
            let state =
                b.ins().load(types::I64, MemFlags::new(), entry_ptr, MAP_ENTRY_STATE_OFFSET);
            let occupied = b.ins().icmp_imm(IntCC::Equal, state, MAP_ENTRY_OCCUPIED);
            let skip_block = b.create_block();
            let cmp_block = b.create_block();
            b.ins().brif(occupied, cmp_block, &[], skip_block, &[]);
            b.seal_block(body_block);

            b.switch_to_block(cmp_block);
            let stored_key =
                b.ins().load(types::I64, MemFlags::new(), entry_ptr, MAP_ENTRY_KEY_OFFSET);
            let equal = string_eq_bytes(b, stored_key, key_ptr);
            b.ins().brif(equal, found_block, &[], skip_block, &[]);
            b.seal_block(cmp_block);

            b.switch_to_block(found_block);
            let removed_tag =
                b.ins().load(types::I64, MemFlags::new(), entry_ptr, MAP_ENTRY_VALUE_TAG_OFFSET);
            let removed_payload = b.ins().load(
                types::I64,
                MemFlags::new(),
                entry_ptr,
                MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            );
            let shift_loop = b.create_block();
            let shift_body = b.create_block();
            let done_block = b.create_block();
            let start = b.ins().iadd_imm(idx, 1);
            b.append_block_param(shift_loop, types::I64);
            b.ins().jump(shift_loop, &[BlockArg::Value(start)]);
            b.seal_block(found_block);

            b.switch_to_block(shift_loop);
            let cur = b.block_params(shift_loop)[0];
            let shift_more = b.ins().icmp(IntCC::UnsignedLessThan, cur, len);
            b.ins().brif(shift_more, shift_body, &[], done_block, &[]);

            b.switch_to_block(shift_body);
            let src_ptr = map_entry_ptr(b, entries_ptr, cur);
            let dst_index = b.ins().iadd_imm(cur, -1);
            let dst_ptr = map_entry_ptr(b, entries_ptr, dst_index);
            let moved_hash =
                b.ins().load(types::I64, MemFlags::new(), src_ptr, MAP_ENTRY_HASH_OFFSET);
            let moved_key =
                b.ins().load(types::I64, MemFlags::new(), src_ptr, MAP_ENTRY_KEY_OFFSET);
            let moved_tag =
                b.ins().load(types::I64, MemFlags::new(), src_ptr, MAP_ENTRY_VALUE_TAG_OFFSET);
            let moved_payload =
                b.ins().load(types::I64, MemFlags::new(), src_ptr, MAP_ENTRY_VALUE_PAYLOAD_OFFSET);
            let moved_state =
                b.ins().load(types::I64, MemFlags::new(), src_ptr, MAP_ENTRY_STATE_OFFSET);
            b.ins().store(MemFlags::new(), moved_hash, dst_ptr, MAP_ENTRY_HASH_OFFSET);
            b.ins().store(MemFlags::new(), moved_key, dst_ptr, MAP_ENTRY_KEY_OFFSET);
            b.ins().store(MemFlags::new(), moved_tag, dst_ptr, MAP_ENTRY_VALUE_TAG_OFFSET);
            b.ins().store(MemFlags::new(), moved_payload, dst_ptr, MAP_ENTRY_VALUE_PAYLOAD_OFFSET);
            b.ins().store(MemFlags::new(), moved_state, dst_ptr, MAP_ENTRY_STATE_OFFSET);
            let next = b.ins().iadd_imm(cur, 1);
            b.ins().jump(shift_loop, &[BlockArg::Value(next)]);
            b.seal_block(shift_body);

            b.switch_to_block(done_block);
            let new_len = b.ins().iadd_imm(len, -1);
            b.ins().store(MemFlags::new(), new_len, header_ptr, MAP_LEN_OFFSET);
            let last_ptr = map_entry_ptr(b, entries_ptr, new_len);
            b.ins().store(MemFlags::new(), zero, last_ptr, MAP_ENTRY_STATE_OFFSET);
            b.ins().return_(&[removed_tag, removed_payload]);
            b.seal_block(done_block);
            b.seal_block(shift_loop);

            b.switch_to_block(skip_block);
            let next = b.ins().iadd_imm(idx, 1);
            b.ins().jump(loop_block, &[BlockArg::Value(next)]);
            b.seal_block(skip_block);

            b.switch_to_block(trap_block);
            let one = b.ins().iconst(types::I64, 1);
            b.ins().trapnz(one, TrapCode::HEAP_OUT_OF_BOUNDS);
            b.ins().return_(&[zero, zero]);
            b.seal_block(trap_block);
            b.seal_block(loop_block);
        },
    );
}

fn define_rt_map_keys(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    list_new_id: FuncId,
    list_push_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
        let list_new_ref = unsafe { (&mut *module_ptr).declare_func_in_func(list_new_id, func) };
        let list_push_ref = unsafe { (&mut *module_ptr).declare_func_in_func(list_push_id, func) };
        let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_MAP);
        let entries_ptr = map_load_ptr(b, header_ptr);
        let len = map_load_len(b, header_ptr);

        let create_call = b.ins().call(list_new_ref, &[]);
        let list_tag = b.inst_results(create_call)[0];
        let list_payload = b.inst_results(create_call)[1];
        let zero = b.ins().iconst(types::I64, 0);
        let string_tag = b.ins().iconst(types::I64, TAG_STRING);

        let loop_block = b.create_block();
        let body_block = b.create_block();
        let done_block = b.create_block();
        b.append_block_param(loop_block, types::I64);
        b.ins().jump(loop_block, &[BlockArg::Value(zero)]);

        b.switch_to_block(loop_block);
        let idx = b.block_params(loop_block)[0];
        let more = b.ins().icmp(IntCC::UnsignedLessThan, idx, len);
        b.ins().brif(more, body_block, &[], done_block, &[]);

        b.switch_to_block(body_block);
        let entry_ptr = map_entry_ptr(b, entries_ptr, idx);
        let key_ptr = b.ins().load(types::I64, MemFlags::new(), entry_ptr, MAP_ENTRY_KEY_OFFSET);
        let _push = b.ins().call(list_push_ref, &[list_tag, list_payload, string_tag, key_ptr]);
        let next = b.ins().iadd_imm(idx, 1);
        b.ins().jump(loop_block, &[BlockArg::Value(next)]);
        b.seal_block(body_block);

        b.switch_to_block(done_block);
        b.ins().return_(&[list_tag, list_payload]);
        b.seal_block(done_block);
        b.seal_block(loop_block);
    });
}

fn define_runtime_scalar_fn(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    params: &[Type],
    build: impl FnOnce(&mut FunctionBuilder, &[Value], &mut Function),
) {
    define_runtime_fn(module, isa, flags, id, params, &[types::I64], build);
}

fn define_runtime_pair_fn(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    params: &[Type],
    build: impl FnOnce(&mut FunctionBuilder, &[Value], &mut Function),
) {
    define_runtime_fn(module, isa, flags, id, params, &[types::I64, types::I64], build);
}

fn define_rt_list_insert(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    _to_i64_id: FuncId,
    alloc_id: FuncId,
    memcpy_id: FuncId,
    _box_value_id: FuncId,
) {
    let module_ptr: *mut _ = module;
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64, types::I64, types::I64],
        |b, p, func| {
            let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_LIST);
            let idx = pair_payload_for_tag(b, p[2], p[3], TAG_INT);
            let non_neg = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, idx, 0);
            b.ins().trapz(non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);

            let len = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
            let in_bounds = b.ins().icmp(IntCC::UnsignedLessThanOrEqual, idx, len);
            b.ins().trapz(in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);

            let cap = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_CAP_OFFSET);
            let data_ptr = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);

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
            let bytes = b.ins().ishl_imm(new_cap, 4);
            let align = b.ins().iconst(types::I64, 8);
            let new_data_call = b.ins().call(alloc, &[bytes, align]);
            let new_data_ptr = b.inst_results(new_data_call)[0];
            let old_bytes = b.ins().ishl_imm(len, 4);
            let _ = b.ins().call(memcpy, &[new_data_ptr, data_ptr, old_bytes]);
            b.ins().store(MemFlags::new(), new_data_ptr, header_ptr, LIST_PTR_OFFSET);
            b.ins().store(MemFlags::new(), new_cap, header_ptr, LIST_CAP_OFFSET);
            b.ins().jump(cont_block, &[BlockArg::Value(new_data_ptr)]);

            b.switch_to_block(cont_block);
            b.seal_block(cont_block);
            let active_data_ptr = b.block_params(cont_block)[0];
            let loop_block = b.create_block();
            let body_block = b.create_block();
            let done_block = b.create_block();
            b.append_block_param(loop_block, types::I64);
            b.ins().jump(loop_block, &[BlockArg::Value(len)]);

            b.switch_to_block(loop_block);
            let cur = b.block_params(loop_block)[0];
            let should_shift = b.ins().icmp(IntCC::UnsignedGreaterThan, cur, idx);
            b.ins().brif(should_shift, body_block, &[], done_block, &[]);

            b.switch_to_block(body_block);
            b.seal_block(body_block);
            let src_index = b.ins().iadd_imm(cur, -1);
            let src_off = b.ins().ishl_imm(src_index, 4);
            let dst_off = b.ins().ishl_imm(cur, 4);
            let src_ptr = b.ins().iadd(active_data_ptr, src_off);
            let dst_ptr = b.ins().iadd(active_data_ptr, dst_off);
            let moved_tag = b.ins().load(types::I64, MemFlags::new(), src_ptr, 0);
            let moved_payload =
                b.ins().load(types::I64, MemFlags::new(), src_ptr, VALUE_PAYLOAD_OFFSET);
            b.ins().store(MemFlags::new(), moved_tag, dst_ptr, 0);
            b.ins().store(MemFlags::new(), moved_payload, dst_ptr, VALUE_PAYLOAD_OFFSET);
            b.ins().jump(loop_block, &[BlockArg::Value(src_index)]);

            b.switch_to_block(done_block);
            b.seal_block(done_block);
            b.seal_block(loop_block);
            let insert_off = b.ins().ishl_imm(idx, 4);
            let insert_ptr = b.ins().iadd(active_data_ptr, insert_off);
            b.ins().store(MemFlags::new(), p[4], insert_ptr, 0);
            b.ins().store(MemFlags::new(), p[5], insert_ptr, VALUE_PAYLOAD_OFFSET);
            let new_len = b.ins().iadd_imm(len, 1);
            b.ins().store(MemFlags::new(), new_len, header_ptr, LIST_LEN_OFFSET);
            b.ins().return_(&[p[0], p[1]]);
        },
    );
}

fn define_rt_list_set(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    _to_i64_id: FuncId,
    _box_value_id: FuncId,
) {
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64, types::I64, types::I64],
        |b, p, _func| {
            let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_LIST);
            let idx = pair_payload_for_tag(b, p[2], p[3], TAG_INT);
            let non_neg = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, idx, 0);
            b.ins().trapz(non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
            let len = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
            let in_bounds = b.ins().icmp(IntCC::UnsignedLessThan, idx, len);
            b.ins().trapz(in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);
            let data_ptr = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);
            let off = b.ins().ishl_imm(idx, 4);
            let elem_ptr = b.ins().iadd(data_ptr, off);
            b.ins().store(MemFlags::new(), p[4], elem_ptr, 0);
            b.ins().store(MemFlags::new(), p[5], elem_ptr, VALUE_PAYLOAD_OFFSET);
            b.ins().return_(&[p[4], p[5]]);
        },
    );
}

fn define_rt_list_swap(
    module: &mut impl CraneliftModule,
    isa: &OwnedTargetIsa,
    flags: &settings::Flags,
    id: FuncId,
    _to_i64_id: FuncId,
) {
    define_runtime_pair_fn(
        module,
        isa,
        flags,
        id,
        &[types::I64, types::I64, types::I64, types::I64, types::I64, types::I64],
        |b, p, _func| {
            let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_LIST);
            let i = pair_payload_for_tag(b, p[2], p[3], TAG_INT);
            let j = pair_payload_for_tag(b, p[4], p[5], TAG_INT);

            let i_non_neg = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, i, 0);
            b.ins().trapz(i_non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);
            let j_non_neg = b.ins().icmp_imm(IntCC::SignedGreaterThanOrEqual, j, 0);
            b.ins().trapz(j_non_neg, TrapCode::HEAP_OUT_OF_BOUNDS);

            let len = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
            let i_in_bounds = b.ins().icmp(IntCC::UnsignedLessThan, i, len);
            b.ins().trapz(i_in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);
            let j_in_bounds = b.ins().icmp(IntCC::UnsignedLessThan, j, len);
            b.ins().trapz(j_in_bounds, TrapCode::HEAP_OUT_OF_BOUNDS);

            let data_ptr = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);
            let i_off = b.ins().ishl_imm(i, 4);
            let i_ptr = b.ins().iadd(data_ptr, i_off);
            let j_off = b.ins().ishl_imm(j, 4);
            let j_ptr = b.ins().iadd(data_ptr, j_off);
            let i_tag = b.ins().load(types::I64, MemFlags::new(), i_ptr, 0);
            let i_payload = b.ins().load(types::I64, MemFlags::new(), i_ptr, VALUE_PAYLOAD_OFFSET);
            let j_tag = b.ins().load(types::I64, MemFlags::new(), j_ptr, 0);
            let j_payload = b.ins().load(types::I64, MemFlags::new(), j_ptr, VALUE_PAYLOAD_OFFSET);
            b.ins().store(MemFlags::new(), j_tag, i_ptr, 0);
            b.ins().store(MemFlags::new(), j_payload, i_ptr, VALUE_PAYLOAD_OFFSET);
            b.ins().store(MemFlags::new(), i_tag, j_ptr, 0);
            b.ins().store(MemFlags::new(), i_payload, j_ptr, VALUE_PAYLOAD_OFFSET);
            b.ins().return_(&[p[0], p[1]]);
        },
    );
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
    define_runtime_pair_fn(module, isa, flags, id, &[types::I64, types::I64], |b, p, func| {
        let header_ptr = pair_payload_for_tag(b, p[0], p[1], TAG_LIST);
        let src_ptr = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_PTR_OFFSET);
        let len = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_LEN_OFFSET);
        let cap = b.ins().load(types::I64, MemFlags::new(), header_ptr, LIST_CAP_OFFSET);

        let alloc = unsafe { (&mut *module_ptr).declare_func_in_func(alloc_id, func) };
        let memcpy = unsafe { (&mut *module_ptr).declare_func_in_func(memcpy_id, func) };
        let align = b.ins().iconst(types::I64, 8);
        let bytes = b.ins().ishl_imm(cap, 4);
        let new_data_call = b.ins().call(alloc, &[bytes, align]);
        let new_data = b.inst_results(new_data_call)[0];
        let _copy = b.ins().call(memcpy, &[new_data, src_ptr, bytes]);

        let header_size = b.ins().iconst(types::I64, LIST_HEADER_SIZE);
        let new_header_call = b.ins().call(alloc, &[header_size, align]);
        let new_header = b.inst_results(new_header_call)[0];
        b.ins().store(MemFlags::new(), new_data, new_header, LIST_PTR_OFFSET);
        b.ins().store(MemFlags::new(), len, new_header, LIST_LEN_OFFSET);
        b.ins().store(MemFlags::new(), cap, new_header, LIST_CAP_OFFSET);

        let tag = b.ins().iconst(types::I64, TAG_LIST);
        b.ins().return_(&[tag, new_header]);
    });
}
