use super::*;

impl<'ctx> LlvmCompiler<'ctx> {
    pub(super) fn validate_unary_callback_ast(
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

    pub(super) fn compile_list_map(
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

    pub(super) fn compile_list_filter(
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

    pub(super) fn compile_list_range(
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

    pub(super) fn build_list_len_load(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
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

    pub(super) fn build_list_len_store(
        &self,
        payload: IntValue<'ctx>,
        len: IntValue<'ctx>,
        label: &str,
    ) {
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

    pub(super) fn build_list_cap_load(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
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

    pub(super) fn build_list_cap_store(
        &self,
        payload: IntValue<'ctx>,
        cap: IntValue<'ctx>,
        label: &str,
    ) {
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

    pub(super) fn build_list_data_ptr_load(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
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

    pub(super) fn build_list_data_ptr_store(
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

    pub(super) fn build_list_value_ptr(
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

    pub(super) fn build_list_value_ptr_from_data_ptr(
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

    pub(super) fn build_list_value_load(
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

    pub(super) fn build_list_value_load_from_data_ptr(
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

    pub(super) fn build_list_value_store(
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

    pub(super) fn build_list_value_store_from_data_ptr(
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

    pub(super) fn build_index_bounds_check(
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

    pub(super) fn list_header_type(&self) -> inkwell::types::StructType<'ctx> {
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

    pub(super) fn define_pair_list_len(&mut self, name: &str, symbol: &str) {
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

    pub(super) fn define_pair_list_new(&mut self, name: &str, symbol: &str) {
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

    pub(super) fn define_pair_list_push(&mut self, name: &str, symbol: &str) {
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

    pub(super) fn define_pair_list_get(&mut self, name: &str, symbol: &str) {
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

    pub(super) fn define_pair_list_set(&mut self, name: &str, symbol: &str) {
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

    pub(super) fn define_pair_list_insert(&mut self, name: &str, symbol: &str) {
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

    pub(super) fn define_pair_list_swap(&mut self, name: &str, symbol: &str) {
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

    pub(super) fn define_pair_list_pop(&mut self, name: &str, symbol: &str) {
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

    pub(super) fn define_pair_list_delete(&mut self, name: &str, symbol: &str) {
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
        let loop_block = self.context.append_basic_block(function, "loop");
        let body_block = self.context.append_basic_block(function, "body");
        let done_block = self.context.append_basic_block(function, "done");
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
            self.expect_tag_payload(list, TAG_LIST, "list_delete_list", ok_block, trap_block);

        self.builder.position_at_end(ok_block);
        let idx = self.expect_tag_int(index, "list_delete_index", trap_block);
        self.build_index_bounds_check(list_payload, idx, "list_delete", trap_block);
        let bounds_ok_block = self.builder.get_insert_block().unwrap();
        let len = self.build_list_len_load(list_payload, "list_delete");
        let removed = self.build_list_value_load(list_payload, idx, "list_delete_removed");
        let start = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "list_delete_start")
            .expect("failed to increment list delete index");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to branch to list_delete loop");

        self.builder.position_at_end(loop_block);
        let cur_phi = self
            .builder
            .build_phi(self.i64_type, "list_delete_cur")
            .expect("failed to build list_delete phi");
        cur_phi.add_incoming(&[(&start, bounds_ok_block)]);
        let cur = cur_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, cur, len, "list_delete_more")
            .expect("failed to compare list_delete cursor");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed to branch list_delete loop");

        self.builder.position_at_end(body_block);
        let moved = self.build_list_value_load(list_payload, cur, "list_delete_src");
        let dst = self
            .builder
            .build_int_sub(cur, self.i64_type.const_int(1, false), "list_delete_dst")
            .expect("failed to decrement list_delete dst");
        self.build_list_value_store(list_payload, dst, moved, "list_delete_shift");
        let next = self
            .builder
            .build_int_add(cur, self.i64_type.const_int(1, false), "list_delete_next")
            .expect("failed to increment list_delete cursor");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to loop list_delete");
        cur_phi.add_incoming(&[(&next, body_block)]);

        self.builder.position_at_end(done_block);
        let new_len = self
            .builder
            .build_int_sub(
                len,
                self.i64_type.const_int(1, false),
                "list_delete_new_len",
            )
            .expect("failed to decrement list_delete len");
        self.build_list_len_store(list_payload, new_len, "list_delete");
        self.builder
            .build_return(Some(&self.make_pair_value(
                removed.tag,
                removed.payload,
                "list_delete_result",
            )))
            .expect("failed to return list_delete");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_list_copy(&mut self, name: &str, symbol: &str) {
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
}
