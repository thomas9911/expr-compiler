use super::*;
use crate::methods::{method_target_functions, resolve_method};
use crate::module::stdlib_function;
use crate::parser::{MapEntryAst, MapKeyAst};

impl<'ctx> LlvmCompiler<'ctx> {
    pub(super) fn compile_ast(
        &self,
        ast: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        match ast {
            Ast::Literal(LiteralAst::Bool(value)) => {
                self.int_value(self.i64_type.const_int(if *value { 1 } else { 0 }, false))
            }
            Ast::Literal(LiteralAst::Integer(n)) => {
                self.int_value(self.i64_type.const_int(*n as u64, true))
            }
            Ast::Literal(LiteralAst::String(value)) => {
                self.build_string_literal(value.as_bytes(), "string_literal")
            }
            Ast::Literal(LiteralAst::BigInt(digits)) => {
                self.build_bigint_literal(digits, "bigint_literal")
            }
            Ast::Lambda { .. } => unimplemented!("anonymous functions"),
            Ast::FunctionRef(name) => {
                self.allocate_closure_for_function(name, vars, capture_slots, env_ptr, function)
            }
            Ast::MultiValue(values) => self.compile_multi_value_ast(
                values,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            ),
            Ast::ListLiteral(items) => self.compile_list_literal_ast(
                items,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            ),
            Ast::MapLiteral(entries) => self.compile_map_literal_ast(
                entries,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            ),
            Ast::StructLiteral { .. } => unimplemented!("struct literals"),
            Ast::FieldAccess { .. } => unimplemented!("struct field access"),
            Ast::Index { collection, index, .. } => self.compile_index_ast(
                collection,
                index,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            ),
            Ast::IndexAssign { collection, index, value, .. } => self.compile_index_assign_ast(
                collection,
                index,
                value,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            ),
            Ast::Expression(ExpressionAst { function: name, args, .. }) => self
                .compile_expression_ast(
                    name,
                    args,
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                ),
            Ast::MethodCall { receiver, method, args, .. } => {
                let function_analysis = self.function_analysis(current_function_name);
                let receiver_shape =
                    infer_ast_value_shape(receiver, function_analysis, &self.value_kind_analysis);
                let resolved_function = resolve_method(&receiver_shape, method.as_str())
                    .or_else(|_| {
                        let mut candidates = method_target_functions(method.as_str())
                            .into_iter()
                            .filter(|function| {
                                is_builtin_name(function.as_str())
                                    || stdlib_function(function.as_str()).is_some()
                                    || self.function_arities.contains_key(function.as_str())
                            })
                            .collect::<Vec<_>>();
                        candidates.sort_unstable();
                        candidates.dedup();
                        match candidates.as_slice() {
                            [function] => Ok(function.clone()),
                            _ => Err(crate::methods::MethodResolutionError::UnknownReceiver),
                        }
                    })
                    .unwrap_or_else(|err| {
                        panic!(
                            "method call should have been validated before LLVM codegen: {err:?}"
                        )
                    });
                let mut resolved_args = Vec::with_capacity(args.len() + 1);
                resolved_args.push((**receiver).clone());
                resolved_args.extend(args.iter().cloned());
                self.compile_expression_ast(
                    resolved_function.as_str(),
                    &resolved_args,
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                )
            }
            Ast::Block(block) => self.compile_block_ast(
                block,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            ),
            Ast::Variable(name) => {
                self.resolve_named_value(name, vars, capture_slots, env_ptr, function)
            }
            Ast::Assign { name, value, .. } => self.compile_assign_ast(
                name,
                value,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            ),
            Ast::MultiAssign { names, value, .. } => self.compile_multi_assign_ast(
                names,
                value,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            ),
            Ast::If { condition, then, else_, .. } => self.compile_if_ast(
                condition,
                then,
                else_,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            ),
            Ast::FunctionDef(_) => unimplemented!("nested function definitions"),
            Ast::StructDef(_) => unimplemented!("struct declarations"),
        }
    }

    fn compile_multi_value_ast(
        &self,
        values: &[Ast],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        let compiled = values
            .iter()
            .map(|value| {
                self.compile_ast(
                    value,
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                )
            })
            .collect::<Vec<_>>();
        self.compile_multi_compiled_values(&compiled)
    }

    fn compile_multi_compiled_values(
        &self,
        compiled: &[CompiledValue<'ctx>],
    ) -> CompiledValue<'ctx> {
        let alloc = self.require_func("__alloc");
        let align = self.i64_type.const_int(8, false);
        let data_bytes =
            self.i64_type.const_int((compiled.len() as i64 * VALUE_SIZE) as u64, false);
        let data_raw = self.build_boxed_call(alloc, &[data_bytes, align], "multi_data_alloc");
        let header_size = self.i64_type.const_int(MULTI_HEADER_SIZE as u64, false);
        let header_raw = self.build_boxed_call(alloc, &[header_size, align], "multi_header_alloc");
        let header_ptr = self
            .builder
            .build_int_to_ptr(
                header_raw,
                self.context.ptr_type(Default::default()),
                "multi_header_ptr",
            )
            .expect("failed to convert multi header ptr");
        let len_ptr = self
            .builder
            .build_struct_gep(self.multi_header_type(), header_ptr, 0, "multi_len_ptr")
            .expect("failed to build multi len gep");
        self.builder
            .build_store(len_ptr, self.i64_type.const_int(compiled.len() as u64, false))
            .expect("failed to store multi len");
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(self.multi_header_type(), header_ptr, 1, "multi_data_ptr_ptr")
            .expect("failed to build multi data ptr gep");
        match self.runtime_mode {
            LlvmRuntimeMode::Native => {
                let data_ptr = self
                    .builder
                    .build_int_to_ptr(
                        data_raw,
                        self.context.ptr_type(Default::default()),
                        "multi_data_ptr",
                    )
                    .expect("failed to convert multi data ptr");
                self.builder
                    .build_store(data_ptr_ptr, data_ptr)
                    .expect("failed to store multi data ptr");
            }
            LlvmRuntimeMode::Wasm => {
                self.builder
                    .build_store(data_ptr_ptr, data_raw)
                    .expect("failed to store wasm multi data ptr");
            }
            #[cfg(feature = "wasi")]
            LlvmRuntimeMode::WasiPreview1Command => {
                self.builder
                    .build_store(data_ptr_ptr, data_raw)
                    .expect("failed to store wasi multi data ptr");
            }
        }
        for (index, value) in compiled.iter().enumerate() {
            let slot_offset = self.i64_type.const_int((index as i64 * VALUE_SIZE) as u64, false);
            let slot_tag_ptr = self.build_value_ptr_from_base(
                data_raw,
                slot_offset,
                &format!("multi_value_{index}_tag"),
            );
            self.builder
                .build_store(
                    slot_tag_ptr,
                    self.builder
                        .build_int_truncate(
                            value.tag,
                            self.context.i8_type(),
                            &format!("multi_value_{index}_tag_i8"),
                        )
                        .expect("failed to truncate multi tag"),
                )
                .expect("failed to store multi tag");
            let payload_offset = self
                .builder
                .build_int_add(
                    slot_offset,
                    self.i64_type.const_int(VALUE_PAYLOAD_OFFSET as u64, false),
                    &format!("multi_value_{index}_payload_offset"),
                )
                .expect("failed to compute multi payload offset");
            let slot_payload_ptr = self.build_i64_ptr_from_base(
                data_raw,
                payload_offset,
                &format!("multi_value_{index}_payload_ptr"),
            );
            self.builder
                .build_store(slot_payload_ptr, value.payload)
                .expect("failed to store multi payload");
        }
        CompiledValue { tag: self.i64_type.const_int(TAG_MULTI as u64, false), payload: header_raw }
    }

    fn compile_multi_assign_ast(
        &self,
        names: &[String],
        value: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        let multi_value =
            self.compile_ast(value, vars, capture_slots, env_ptr, function, current_function_name);
        let mut last = None;
        for (index, name) in names.iter().enumerate() {
            let unpacked = self.build_multi_value_load(multi_value, index, function);
            let ptr = vars.get(name).unwrap_or_else(|| {
                panic!("internal compiler error: assignment target '{name}' has no local slot")
            });
            self.builder
                .build_store(
                    *ptr,
                    self.make_pair_value(
                        unpacked.tag,
                        unpacked.payload,
                        &format!("multi_assign_{index}_pair"),
                    ),
                )
                .expect("failed to store multi assignment value");
            last = Some(unpacked);
        }
        last.expect("multi assignment must have at least one target")
    }

    fn build_multi_value_load(
        &self,
        multi_value: CompiledValue<'ctx>,
        index: usize,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let trap_block = self.context.append_basic_block(function, "multi_load_trap");
        let ok_block = self.context.append_basic_block(function, "multi_load_ok");
        let payload =
            self.expect_tag_payload(multi_value, TAG_MULTI, "multi_value", ok_block, trap_block);
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(ok_block);

        let len = self.build_multi_len_load(payload, "multi_len");
        let index_value = self.i64_type.const_int(index as u64, false);
        let in_bounds = self
            .builder
            .build_int_compare(IntPredicate::ULT, index_value, len, "multi_in_bounds")
            .expect("failed to compare multi bounds");
        let bounds_ok = self.context.append_basic_block(function, "multi_bounds_ok");
        let bounds_trap = self.context.append_basic_block(function, "multi_bounds_trap");
        self.builder
            .build_conditional_branch(in_bounds, bounds_ok, bounds_trap)
            .expect("failed to branch on multi bounds");
        self.builder.position_at_end(bounds_trap);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(bounds_ok);

        let slot_offset = self
            .builder
            .build_int_mul(
                index_value,
                self.i64_type.const_int(VALUE_SIZE as u64, false),
                "multi_slot_offset",
            )
            .expect("failed to compute multi slot offset");
        let data_raw = self.build_multi_data_raw_load(payload, "multi_data_raw");
        let tag_ptr = self.build_value_ptr_from_base(data_raw, slot_offset, "multi_tag_ptr");
        let tag_i8 = self
            .builder
            .build_load(self.context.i8_type(), tag_ptr, "multi_tag_i8")
            .expect("failed to load multi tag")
            .into_int_value();
        let tag = self
            .builder
            .build_int_z_extend(tag_i8, self.i64_type, "multi_tag")
            .expect("failed to extend multi tag");
        let payload_offset = self
            .builder
            .build_int_add(
                slot_offset,
                self.i64_type.const_int(VALUE_PAYLOAD_OFFSET as u64, false),
                "multi_payload_offset",
            )
            .expect("failed to compute multi payload offset");
        let payload_ptr =
            self.build_i64_ptr_from_base(data_raw, payload_offset, "multi_payload_ptr");
        let payload_value = self
            .builder
            .build_load(self.i64_type, payload_ptr, "multi_payload")
            .expect("failed to load multi payload")
            .into_int_value();
        CompiledValue { tag, payload: payload_value }
    }

    fn multi_header_type(&self) -> inkwell::types::StructType<'ctx> {
        let data_ptr_field = match self.runtime_mode {
            LlvmRuntimeMode::Native => self.context.ptr_type(Default::default()).into(),
            LlvmRuntimeMode::Wasm => self.i64_type.into(),
            #[cfg(feature = "wasi")]
            LlvmRuntimeMode::WasiPreview1Command => self.i64_type.into(),
        };
        self.context.struct_type(&[self.i64_type.into(), data_ptr_field], false)
    }

    fn build_multi_len_load(&self, payload: IntValue<'ctx>, label: &str) -> IntValue<'ctx> {
        let header_ptr = self
            .builder
            .build_int_to_ptr(
                payload,
                self.context.ptr_type(Default::default()),
                &format!("{label}_header_ptr"),
            )
            .expect("failed to convert multi header ptr");
        let len_ptr = self
            .builder
            .build_struct_gep(self.multi_header_type(), header_ptr, 0, &format!("{label}_len_ptr"))
            .expect("failed to build multi len gep");
        self.builder
            .build_load(self.i64_type, len_ptr, &format!("{label}_len"))
            .expect("failed to load multi len")
            .into_int_value()
    }

    fn build_multi_data_raw_load(&self, payload: IntValue<'ctx>, label: &str) -> IntValue<'ctx> {
        let header_ptr = self
            .builder
            .build_int_to_ptr(
                payload,
                self.context.ptr_type(Default::default()),
                &format!("{label}_header_ptr"),
            )
            .expect("failed to convert multi header ptr");
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(
                self.multi_header_type(),
                header_ptr,
                1,
                &format!("{label}_data_ptr_ptr"),
            )
            .expect("failed to build multi data ptr gep");
        match self.runtime_mode {
            LlvmRuntimeMode::Native => {
                let ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(Default::default()),
                        data_ptr_ptr,
                        &format!("{label}_data_ptr"),
                    )
                    .expect("failed to load multi data ptr")
                    .into_pointer_value();
                self.builder
                    .build_ptr_to_int(ptr, self.i64_type, &format!("{label}_data_raw"))
                    .expect("failed to convert multi data ptr to int")
            }
            LlvmRuntimeMode::Wasm => self
                .builder
                .build_load(self.i64_type, data_ptr_ptr, &format!("{label}_data_raw"))
                .expect("failed to load wasm multi data raw")
                .into_int_value(),
            #[cfg(feature = "wasi")]
            LlvmRuntimeMode::WasiPreview1Command => self
                .builder
                .build_load(self.i64_type, data_ptr_ptr, &format!("{label}_data_raw"))
                .expect("failed to load wasi multi data raw")
                .into_int_value(),
        }
    }

    fn build_value_ptr_from_base(
        &self,
        base_raw: IntValue<'ctx>,
        offset: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        let addr = self
            .builder
            .build_int_add(base_raw, offset, &format!("{label}_addr"))
            .expect("failed to compute value ptr address");
        self.builder
            .build_int_to_ptr(addr, self.context.ptr_type(Default::default()), label)
            .expect("failed to convert value ptr")
    }

    fn build_i64_ptr_from_base(
        &self,
        base_raw: IntValue<'ctx>,
        offset: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        let addr = self
            .builder
            .build_int_add(base_raw, offset, &format!("{label}_addr"))
            .expect("failed to compute i64 ptr address");
        self.builder
            .build_int_to_ptr(addr, self.context.ptr_type(Default::default()), label)
            .expect("failed to convert i64 ptr")
    }

    fn compile_list_literal_ast(
        &self,
        items: &[Ast],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        let list = self.build_internal_call(self.require_func("__rt_list_new"), &[], "list_new");
        for item in items {
            let value = self.compile_ast(
                item,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            );
            let _ = self.build_internal_call(
                self.require_func("__rt_list_push"),
                &[list, value],
                "list_push",
            );
        }
        list
    }

    fn compile_map_literal_ast(
        &self,
        entries: &[MapEntryAst],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        let map = self.build_internal_call(self.require_func("__rt_map_new"), &[], "map_new");
        for entry in entries {
            let key = match &entry.key {
                MapKeyAst::Static(key) => {
                    self.build_string_literal(key.as_bytes(), "map_key_literal")
                }
                MapKeyAst::Dynamic(key) => self.compile_ast(
                    key,
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                ),
            };
            let value = self.compile_ast(
                &entry.value,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            );
            let _ = self.build_internal_call(
                self.require_func("__rt_map_set"),
                &[map, key, value],
                "map_set",
            );
        }
        map
    }

    fn compile_index_ast(
        &self,
        collection: &Ast,
        index: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        let function_analysis = self.function_analysis(current_function_name);
        let collection_shape =
            infer_ast_value_shape(collection, function_analysis, &self.value_kind_analysis);
        let index_shape =
            infer_ast_value_shape(index, function_analysis, &self.value_kind_analysis);
        let collection = self.compile_ast(
            collection,
            vars,
            capture_slots,
            env_ptr,
            function,
            current_function_name,
        );
        let index =
            self.compile_ast(index, vars, capture_slots, env_ptr, function, current_function_name);
        if shape_is_exact_kind(&collection_shape, KindSet::list())
            && shape_is_exact_kind(&index_shape, KindSet::int())
        {
            let idx = index.payload;
            let trap_block = self.context.append_basic_block(function, "list_get_bounds_trap");
            self.build_index_bounds_check(collection.payload, idx, "list_get", trap_block);
            let result = self.build_list_value_load(collection.payload, idx, "list_get");
            let ok_block = self.builder.get_insert_block().expect("missing list_get ok block");
            self.builder.position_at_end(trap_block);
            self.build_trap_and_unreachable();
            self.builder.position_at_end(ok_block);
            result
        } else {
            self.build_internal_call(
                self.require_func("__rt_list_get"),
                &[collection, index],
                "list_get",
            )
        }
    }

    fn compile_index_assign_ast(
        &self,
        collection: &Ast,
        index: &Ast,
        value: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        let collection = self.compile_ast(
            collection,
            vars,
            capture_slots,
            env_ptr,
            function,
            current_function_name,
        );
        let index =
            self.compile_ast(index, vars, capture_slots, env_ptr, function, current_function_name);
        let value =
            self.compile_ast(value, vars, capture_slots, env_ptr, function, current_function_name);
        self.build_internal_call(
            self.require_func("__rt_list_set"),
            &[collection, index, value],
            "list_set",
        )
    }

    fn compile_expression_ast(
        &self,
        name: &str,
        args: &[Ast],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        if matches!(
            name,
            "is_int"
                | "is_bigint"
                | "is_string"
                | "is_list"
                | "is_map"
                | "is_map_iter"
                | "is_function"
                | "is_string_iter"
        ) {
            assert_eq!(args.len(), 1, "{name} expects 1 argument");
            let value = self.compile_ast(
                &args[0],
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            );
            let expected_tag = match name {
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
            return self.compile_is_tag_predicate(value, expected_tag, name);
        }
        if matches!(
            name,
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
        ) && args.len() == 2
        {
            let function_analysis = self.function_analysis(current_function_name);
            let lhs_shape =
                infer_ast_value_shape(&args[0], function_analysis, &self.value_kind_analysis);
            let rhs_shape =
                infer_ast_value_shape(&args[1], function_analysis, &self.value_kind_analysis);
            let lhs_exact_int = shape_is_exact_kind(&lhs_shape, KindSet::int());
            let rhs_exact_int = shape_is_exact_kind(&rhs_shape, KindSet::int());
            let lhs_exact_bigint = shape_is_exact_kind(&lhs_shape, KindSet::bigint());
            let rhs_exact_bigint = shape_is_exact_kind(&rhs_shape, KindSet::bigint());
            let exact_int_case = lhs_exact_int && rhs_exact_int;
            let exact_bigint_case = lhs_exact_bigint && rhs_exact_bigint;
            let exact_bigint_shift_case =
                matches!(name, "shl" | "shr") && lhs_exact_bigint && rhs_exact_int;
            if exact_int_case || exact_bigint_case || exact_bigint_shift_case {
                let lhs = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let rhs = self.compile_ast(
                    &args[1],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                return if exact_int_case {
                    self.compile_exact_int_operator(name, lhs, rhs)
                } else {
                    self.compile_exact_bigint_operator(name, lhs, rhs, function)
                };
            }
        }
        if name == "not" {
            assert_eq!(args.len(), 1, "{name} expects 1 argument");
            return self.compile_logical_not(
                &args[0],
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            );
        }
        if name == "and" || name == "or" {
            assert_eq!(args.len(), 2, "{name} expects 2 arguments");
            return self.compile_logical_op(
                name,
                &args[0],
                &args[1],
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            );
        }
        if name == "list_map" {
            return self.compile_list_map(
                args,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            );
        }
        if name == "list_filter" {
            return self.compile_list_filter(
                args,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            );
        }
        if name == "list_range" {
            return self.compile_list_range(
                args,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            );
        }
        if let Some(value) = self.compile_string_expression_ast(
            name,
            args,
            vars,
            capture_slots,
            env_ptr,
            function,
            current_function_name,
        ) {
            return value;
        }

        let compiled = args
            .iter()
            .map(|arg| {
                self.compile_ast(arg, vars, capture_slots, env_ptr, function, current_function_name)
            })
            .collect::<Vec<_>>();
        if name.is_empty() {
            return compiled[0];
        }
        self.compile_named_expression_ast(
            name,
            args,
            &compiled,
            vars,
            capture_slots,
            env_ptr,
            function,
            current_function_name,
        )
    }

    fn compile_string_expression_ast(
        &self,
        name: &str,
        args: &[Ast],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> Option<CompiledValue<'ctx>> {
        match name {
            "bytes_len" => {
                assert_eq!(args.len(), 1, "bytes_len expects 1 argument");
                let function_analysis = self.function_analysis(current_function_name);
                let value_shape =
                    infer_ast_value_shape(&args[0], function_analysis, &self.value_kind_analysis);
                let value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let len = if shape_is_exact_kind(&value_shape, KindSet::string()) {
                    self.build_string_len_load(value.payload, "bytes_len")
                } else {
                    let trap_block = self.context.append_basic_block(function, "bytes_len_trap");
                    let ok_block = self.context.append_basic_block(function, "bytes_len_ok");
                    let raw = self.expect_tag_payload(
                        value,
                        TAG_STRING,
                        "bytes_len",
                        ok_block,
                        trap_block,
                    );
                    self.builder.position_at_end(trap_block);
                    self.build_trap_and_unreachable();
                    self.builder.position_at_end(ok_block);
                    self.build_string_len_load(raw, "bytes_len")
                };
                Some(self.int_value(len))
            }
            "bytes_get" => {
                assert_eq!(args.len(), 2, "bytes_get expects 2 arguments");
                let function_analysis = self.function_analysis(current_function_name);
                let string_shape =
                    infer_ast_value_shape(&args[0], function_analysis, &self.value_kind_analysis);
                let index_shape =
                    infer_ast_value_shape(&args[1], function_analysis, &self.value_kind_analysis);
                let string_value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let index_value = self.compile_ast(
                    &args[1],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                if shape_is_exact_kind(&string_shape, KindSet::string())
                    && shape_is_exact_kind(&index_shape, KindSet::int())
                {
                    let idx = index_value.payload;
                    let trap_block =
                        self.context.append_basic_block(function, "bytes_get_bounds_trap");
                    let ok_block = self.context.append_basic_block(function, "bytes_get_ok");
                    let non_neg = self
                        .builder
                        .build_int_compare(
                            IntPredicate::SGE,
                            idx,
                            self.i64_type.const_zero(),
                            "bytes_get_non_neg",
                        )
                        .expect("failed bytes_get non-neg compare");
                    let len = self.build_string_len_load(string_value.payload, "bytes_get");
                    let in_bounds = self
                        .builder
                        .build_int_compare(IntPredicate::ULT, idx, len, "bytes_get_in_bounds")
                        .expect("failed bytes_get in-bounds compare");
                    let ok = self
                        .builder
                        .build_and(non_neg, in_bounds, "bytes_get_ok_cond")
                        .expect("failed bytes_get ok cond");
                    self.builder
                        .build_conditional_branch(ok, ok_block, trap_block)
                        .expect("failed bytes_get branch");
                    self.builder.position_at_end(trap_block);
                    self.build_trap_and_unreachable();
                    self.builder.position_at_end(ok_block);
                    let data_ptr = self.build_string_ptr_load(string_value.payload, "bytes_get");
                    let base = self
                        .builder
                        .build_ptr_to_int(data_ptr, self.i64_type, "bytes_get_base")
                        .expect("failed bytes_get ptr-to-int");
                    let addr = self
                        .builder
                        .build_int_add(base, idx, "bytes_get_addr")
                        .expect("failed bytes_get addr");
                    let ptr = self
                        .builder
                        .build_int_to_ptr(
                            addr,
                            self.context.ptr_type(Default::default()),
                            "bytes_get_ptr",
                        )
                        .expect("failed bytes_get ptr");
                    let byte = self
                        .builder
                        .build_load(self.context.i8_type(), ptr, "bytes_get_byte")
                        .expect("failed bytes_get load")
                        .into_int_value();
                    let raw = self
                        .builder
                        .build_int_z_extend(byte, self.i64_type, "bytes_get_i64")
                        .expect("failed bytes_get zext");
                    Some(self.int_value(raw))
                } else {
                    Some(self.build_bytes_get(string_value, index_value, function))
                }
            }
            "bytes_pop" => {
                assert_eq!(args.len(), 1, "bytes_pop expects 1 argument");
                let string_value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                Some(self.build_bytes_pop(string_value, function))
            }
            "bytes_push" => {
                assert_eq!(args.len(), 2, "bytes_push expects 2 arguments");
                let string_value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let byte_value = self.compile_ast(
                    &args[1],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                Some(self.build_bytes_push(string_value, byte_value, function))
            }
            "bytes_insert" => {
                assert_eq!(args.len(), 3, "bytes_insert expects 3 arguments");
                let string_value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let index_value = self.compile_ast(
                    &args[1],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let byte_value = self.compile_ast(
                    &args[2],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                Some(self.build_bytes_insert(string_value, index_value, byte_value, function))
            }
            "bytes_remove" => {
                assert_eq!(args.len(), 2, "bytes_remove expects 2 arguments");
                let string_value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let index_value = self.compile_ast(
                    &args[1],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                Some(self.build_bytes_remove(string_value, index_value, function))
            }
            "bytes_set" => {
                assert_eq!(args.len(), 3, "bytes_set expects 3 arguments");
                let string_value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let index_value = self.compile_ast(
                    &args[1],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let byte_value = self.compile_ast(
                    &args[2],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                Some(self.build_bytes_set(string_value, index_value, byte_value, function))
            }
            "bytes_slice" => {
                assert_eq!(args.len(), 3, "bytes_slice expects 3 arguments");
                let function_analysis = self.function_analysis(current_function_name);
                let string_shape =
                    infer_ast_value_shape(&args[0], function_analysis, &self.value_kind_analysis);
                let start_shape =
                    infer_ast_value_shape(&args[1], function_analysis, &self.value_kind_analysis);
                let end_shape =
                    infer_ast_value_shape(&args[2], function_analysis, &self.value_kind_analysis);
                let string_value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let start_value = self.compile_ast(
                    &args[1],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let end_value = self.compile_ast(
                    &args[2],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                if shape_is_exact_kind(&string_shape, KindSet::string())
                    && shape_is_exact_kind(&start_shape, KindSet::int())
                    && shape_is_exact_kind(&end_shape, KindSet::int())
                {
                    Some(self.build_bytes_slice_known_string(
                        string_value.payload,
                        start_value.payload,
                        end_value.payload,
                        function,
                    ))
                } else {
                    Some(self.build_bytes_slice(string_value, start_value, end_value, function))
                }
            }
            "string_chars" => {
                assert_eq!(args.len(), 1, "string_chars expects 1 argument");
                let string_value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                Some(self.build_string_chars(string_value, function))
            }
            "string_iter_done" => {
                assert_eq!(args.len(), 1, "string_iter_done expects 1 argument");
                let iter_value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                Some(self.build_string_iter_done(iter_value, function))
            }
            "string_iter_next" => {
                assert_eq!(args.len(), 1, "string_iter_next expects 1 argument");
                let iter_value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                Some(self.build_string_iter_next(iter_value, function))
            }
            "string_copy" => {
                assert_eq!(args.len(), 1, "string_copy expects 1 argument");
                let string_value = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                Some(self.build_string_copy(string_value, function))
            }
            "string_concat" => {
                assert_eq!(args.len(), 2, "string_concat expects 2 arguments");
                let lhs = self.compile_ast(
                    &args[0],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                let rhs = self.compile_ast(
                    &args[1],
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
                Some(self.build_string_concat(lhs, rhs, function))
            }
            _ => None,
        }
    }

    fn compile_named_expression_ast(
        &self,
        name: &str,
        args: &[Ast],
        compiled: &[CompiledValue<'ctx>],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        let function_analysis = self.function_analysis(current_function_name);
        if let Some(value) = self.compile_generic_runtime_expression_ast(name, compiled, function) {
            return value;
        }
        if let Some(value) = self.compile_list_named_expression_ast(
            name,
            args,
            compiled,
            function,
            function_analysis,
        ) {
            return value;
        }
        if let Some(value) = self.compile_map_named_expression_ast(name, compiled) {
            return value;
        }
        self.compile_fallback_named_expression_ast(
            name,
            compiled,
            vars,
            capture_slots,
            env_ptr,
            function,
        )
    }

    fn compile_generic_runtime_expression_ast(
        &self,
        name: &str,
        compiled: &[CompiledValue<'ctx>],
        function: FunctionValue<'ctx>,
    ) -> Option<CompiledValue<'ctx>> {
        match name {
            "add" => Some(self.build_internal_call(
                self.require_func("__op_add"),
                &[compiled[0], compiled[1]],
                "add",
            )),
            "subtract" => Some(self.build_internal_call(
                self.require_func("__op_subtract"),
                &[compiled[0], compiled[1]],
                "subtract",
            )),
            "multiply" => Some(self.build_internal_call(
                self.require_func("__op_multiply"),
                &[compiled[0], compiled[1]],
                "multiply",
            )),
            "divide" => Some(self.build_internal_call(
                self.require_func("__op_divide"),
                &[compiled[0], compiled[1]],
                "divide",
            )),
            "modulo" => Some(self.build_internal_call(
                self.require_func("__op_modulo"),
                &[compiled[0], compiled[1]],
                "modulo",
            )),
            "bitand" => Some(self.build_internal_call(
                self.require_func("__op_bitand"),
                &[compiled[0], compiled[1]],
                "bitand",
            )),
            "bitor" => Some(self.build_internal_call(
                self.require_func("__op_bitor"),
                &[compiled[0], compiled[1]],
                "bitor",
            )),
            "bitxor" => Some(self.build_internal_call(
                self.require_func("__op_bitxor"),
                &[compiled[0], compiled[1]],
                "bitxor",
            )),
            "shl" => Some(self.build_internal_call(
                self.require_func("__op_shl"),
                &[compiled[0], compiled[1]],
                "shl",
            )),
            "shr" => Some(self.build_internal_call(
                self.require_func("__op_shr"),
                &[compiled[0], compiled[1]],
                "shr",
            )),
            "gt" => Some(self.build_internal_call(
                self.require_func("__op_gt"),
                &[compiled[0], compiled[1]],
                "gt",
            )),
            "lt" => Some(self.build_internal_call(
                self.require_func("__op_lt"),
                &[compiled[0], compiled[1]],
                "lt",
            )),
            "gte" => Some(self.build_internal_call(
                self.require_func("__op_gte"),
                &[compiled[0], compiled[1]],
                "gte",
            )),
            "lte" => Some(self.build_internal_call(
                self.require_func("__op_lte"),
                &[compiled[0], compiled[1]],
                "lte",
            )),
            "eq" => Some(self.build_internal_call(
                self.require_func("__op_eq"),
                &[compiled[0], compiled[1]],
                "eq",
            )),
            "ne" => Some(self.build_internal_call(
                self.require_func("__op_ne"),
                &[compiled[0], compiled[1]],
                "ne",
            )),
            "bigint_add" | "bigint_subtract" | "bigint_multiply" | "bigint_divide"
            | "bigint_modulo" | "bigint_compare" | "bigint_bitand" | "bigint_bitor"
            | "bigint_bitxor" => Some(self.compile_bigint_builtin(name, compiled, function)),
            "bigint_shl" | "bigint_shr" => {
                Some(self.compile_bigint_shift_builtin(name, compiled[0], compiled[1], function))
            }
            "print" => {
                Some(self.build_internal_call(self.require_func("__rt_print"), compiled, "print"))
            }
            _ => None,
        }
    }

    fn compile_list_named_expression_ast(
        &self,
        name: &str,
        args: &[Ast],
        compiled: &[CompiledValue<'ctx>],
        function: FunctionValue<'ctx>,
        function_analysis: &FunctionValueKindAnalysis,
    ) -> Option<CompiledValue<'ctx>> {
        match name {
            "list_new" => Some(self.build_internal_call(
                self.require_func("__rt_list_new"),
                compiled,
                "list_new",
            )),
            "list_push" => Some(self.build_internal_call(
                self.require_func("__rt_list_push"),
                compiled,
                "list_push",
            )),
            "list_insert" => Some(self.build_internal_call(
                self.require_func("__rt_list_insert"),
                compiled,
                "list_insert",
            )),
            "list_len" => {
                Some(self.compile_list_len_named_expression_ast(args, compiled, function_analysis))
            }
            "list_get" => Some(self.compile_list_get_named_expression_ast(
                args,
                compiled,
                function,
                function_analysis,
            )),
            "list_set" => Some(self.build_internal_call(
                self.require_func("__rt_list_set"),
                compiled,
                "list_set",
            )),
            "list_swap" => Some(self.build_internal_call(
                self.require_func("__rt_list_swap"),
                compiled,
                "list_swap",
            )),
            "list_pop" => Some(self.build_internal_call(
                self.require_func("__rt_list_pop"),
                compiled,
                "list_pop",
            )),
            "list_delete" => Some(self.build_internal_call(
                self.require_func("__rt_list_delete"),
                compiled,
                "list_delete",
            )),
            "list_copy" => Some(self.build_internal_call(
                self.require_func("__rt_list_copy"),
                compiled,
                "list_copy",
            )),
            _ => None,
        }
    }

    fn compile_map_named_expression_ast(
        &self,
        name: &str,
        compiled: &[CompiledValue<'ctx>],
    ) -> Option<CompiledValue<'ctx>> {
        match name {
            "map_new" => Some(self.build_internal_call(
                self.require_func("__rt_map_new"),
                compiled,
                "map_new",
            )),
            "map_len" => Some(self.build_internal_call(
                self.require_func("__rt_map_len"),
                compiled,
                "map_len",
            )),
            "map_has" => Some(self.build_internal_call(
                self.require_func("__rt_map_has"),
                compiled,
                "map_has",
            )),
            "map_get" => Some(self.build_internal_call(
                self.require_func("__rt_map_get"),
                compiled,
                "map_get",
            )),
            "map_delete" => Some(self.build_internal_call(
                self.require_func("__rt_map_delete"),
                compiled,
                "map_delete",
            )),
            "map_iter" => Some(self.build_internal_call(
                self.require_func("__rt_map_iter"),
                compiled,
                "map_iter",
            )),
            "map_iter_done" => Some(self.build_internal_call(
                self.require_func("__rt_map_iter_done"),
                compiled,
                "map_iter_done",
            )),
            "map_iter_next" => {
                let key = self.build_internal_call(
                    self.require_func("__rt_map_iter_key"),
                    compiled,
                    "map_iter_next_key",
                );
                let value = self.build_internal_call(
                    self.require_func("__rt_map_iter_value"),
                    compiled,
                    "map_iter_next_value",
                );
                let _advance = self.build_internal_call(
                    self.require_func("__rt_map_iter_advance"),
                    compiled,
                    "map_iter_next_advance",
                );
                Some(self.compile_multi_compiled_values(&[key, value]))
            }
            "map_iter_key" => Some(self.build_internal_call(
                self.require_func("__rt_map_iter_key"),
                compiled,
                "map_iter_key",
            )),
            "map_iter_value" => Some(self.build_internal_call(
                self.require_func("__rt_map_iter_value"),
                compiled,
                "map_iter_value",
            )),
            "map_iter_advance" => Some(self.build_internal_call(
                self.require_func("__rt_map_iter_advance"),
                compiled,
                "map_iter_advance",
            )),
            "map_set" => Some(self.build_internal_call(
                self.require_func("__rt_map_set"),
                compiled,
                "map_set",
            )),
            _ => None,
        }
    }

    fn compile_list_len_named_expression_ast(
        &self,
        args: &[Ast],
        compiled: &[CompiledValue<'ctx>],
        function_analysis: &FunctionValueKindAnalysis,
    ) -> CompiledValue<'ctx> {
        if shape_is_exact_kind(
            &infer_ast_value_shape(&args[0], function_analysis, &self.value_kind_analysis),
            KindSet::list(),
        ) {
            self.int_value(self.build_list_len_load(compiled[0].payload, "list_len"))
        } else {
            self.build_internal_call(self.require_func("__rt_list_len"), compiled, "list_len")
        }
    }

    fn compile_list_get_named_expression_ast(
        &self,
        args: &[Ast],
        compiled: &[CompiledValue<'ctx>],
        function: FunctionValue<'ctx>,
        function_analysis: &FunctionValueKindAnalysis,
    ) -> CompiledValue<'ctx> {
        let list_shape =
            infer_ast_value_shape(&args[0], function_analysis, &self.value_kind_analysis);
        let index_shape =
            infer_ast_value_shape(&args[1], function_analysis, &self.value_kind_analysis);
        if shape_is_exact_kind(&list_shape, KindSet::list())
            && shape_is_exact_kind(&index_shape, KindSet::int())
        {
            let idx = compiled[1].payload;
            let trap_block = self.context.append_basic_block(function, "list_get_bounds_trap");
            self.build_index_bounds_check(compiled[0].payload, idx, "list_get", trap_block);
            let result = self.build_list_value_load(compiled[0].payload, idx, "list_get");
            let ok_block = self.builder.get_insert_block().expect("missing list_get ok block");
            self.builder.position_at_end(trap_block);
            self.build_trap_and_unreachable();
            self.builder.position_at_end(ok_block);
            result
        } else {
            self.build_internal_call(self.require_func("__rt_list_get"), compiled, "list_get")
        }
    }

    fn compile_fallback_named_expression_ast(
        &self,
        name: &str,
        compiled: &[CompiledValue<'ctx>],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        if vars.contains_key(name) || capture_slots.contains_key(name) {
            let callee = self.resolve_named_value(name, vars, capture_slots, env_ptr, function);
            return self.apply_function_value(callee, compiled, function, name);
        }
        if self.function_ordinals.contains_key(name) {
            return self.build_user_call(
                self.require_func(name),
                self.i64_type.const_zero(),
                compiled,
                name,
            );
        }
        let callee = self.require_func(name);
        self.build_internal_call(callee, compiled, name)
    }

    fn compile_block_ast(
        &self,
        block: &crate::parser::BlockAst,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        let mut last = None;
        for line in &block.lines {
            last = Some(self.compile_ast(
                line,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            ));
        }
        last.expect("empty block")
    }

    fn compile_assign_ast(
        &self,
        name: &str,
        value: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
        let value =
            self.compile_ast(value, vars, capture_slots, env_ptr, function, current_function_name);
        let ptr = vars.get(name).unwrap_or_else(|| {
            panic!("internal compiler error: assignment target '{name}' has no llvm local slot")
        });
        self.builder
            .build_store(*ptr, self.make_pair_value(value.tag, value.payload, name))
            .expect("failed to assign variable");
        value
    }

    fn compile_if_ast(
        &self,
        condition: &Ast,
        then: &crate::parser::BlockAst,
        else_: &Option<crate::parser::BlockAst>,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        current_function_name: &str,
    ) -> CompiledValue<'ctx> {
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
            "truthy",
        );
        let cond = self
            .builder
            .build_int_compare(IntPredicate::NE, truth, self.i64_type.const_zero(), "if_cond")
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
            then_value = self.compile_ast(
                line,
                vars,
                capture_slots,
                env_ptr,
                function,
                current_function_name,
            );
        }
        self.builder.build_unconditional_branch(merge_block).expect("failed to branch from then");
        let then_end = self.builder.get_insert_block().expect("then block should exist");

        self.builder.position_at_end(else_block);
        let mut else_value = self.int_value(self.i64_type.const_zero());
        if let Some(else_block_ast) = else_ {
            for line in &else_block_ast.lines {
                else_value = self.compile_ast(
                    line,
                    vars,
                    capture_slots,
                    env_ptr,
                    function,
                    current_function_name,
                );
            }
        }
        self.builder.build_unconditional_branch(merge_block).expect("failed to branch from else");
        let else_end = self.builder.get_insert_block().expect("else block should exist");

        self.builder.position_at_end(merge_block);
        let tag_phi =
            self.builder.build_phi(self.i64_type, "if_tag").expect("failed to build tag phi");
        tag_phi.add_incoming(&[(&then_value.tag, then_end), (&else_value.tag, else_end)]);
        let payload_phi = self
            .builder
            .build_phi(self.i64_type, "if_payload")
            .expect("failed to build payload phi");
        payload_phi
            .add_incoming(&[(&then_value.payload, then_end), (&else_value.payload, else_end)]);
        CompiledValue {
            tag: tag_phi.as_basic_value().into_int_value(),
            payload: payload_phi.as_basic_value().into_int_value(),
        }
    }
}

#[cfg(all(test, feature = "llvm-backend"))]
mod tests {
    use crate::module::{CodegenBackend, Module};

    #[test]
    fn llvm_compile_if_ast_lowers_nested_if_expressions() {
        let src = "fn main() do\n    if 1 do\n        if 0 do\n            7\n        else\n            41\n        end\n    else\n        0\n    end\nend";
        let jit = Module::from_source(src).compile_to_jit_with_backend(CodegenBackend::Llvm);
        let ptr = jit.get_int_result_fn_ptr("main").expect("llvm int-result wrapper should exist");
        let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
        assert_eq!(func(), 41);
    }
}
