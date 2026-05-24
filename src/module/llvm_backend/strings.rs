use super::*;

impl<'ctx> LlvmCompiler<'ctx> {
    pub(super) fn build_string_literal(&self, bytes: &[u8], label: &str) -> CompiledValue<'ctx> {
        let alloc = self.require_func("__alloc");
        let len = self.i64_type.const_int(bytes.len() as u64, false);
        let align = self.i64_type.const_int(8, false);
        let data_raw = self.build_boxed_call(alloc, &[len, align], &format!("{label}_data"));
        let data_ptr = self
            .builder
            .build_int_to_ptr(
                data_raw,
                self.context.ptr_type(Default::default()),
                &format!("{label}_data_ptr"),
            )
            .expect("failed to convert string data ptr");
        for (index, byte) in bytes.iter().copied().enumerate() {
            let byte_ptr = unsafe {
                self.builder
                    .build_gep(
                        self.context.i8_type(),
                        data_ptr,
                        &[self.context.i32_type().const_int(index as u64, false)],
                        &format!("{label}_byte_ptr_{index}"),
                    )
                    .expect("failed to gep string byte ptr")
            };
            self.builder
                .build_store(byte_ptr, self.context.i8_type().const_int(byte as u64, false))
                .expect("failed to store string byte");
        }

        let header_size = self.i64_type.const_int(STRING_HEADER_SIZE as u64, false);
        let header_raw =
            self.build_boxed_call(alloc, &[header_size, align], &format!("{label}_header"));
        self.build_string_len_store(header_raw, len, label);
        self.build_string_cap_store(header_raw, len, label);
        self.build_string_ptr_store(header_raw, data_ptr, label);
        CompiledValue {
            tag: self.i64_type.const_int(TAG_STRING as u64, false),
            payload: header_raw,
        }
    }

    pub(super) fn build_string_eq_bytes(
        &self,
        lhs_payload: IntValue<'ctx>,
        rhs_payload: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let lhs_len = self.build_string_len_load(lhs_payload, &format!("{label}_lhs"));
        let rhs_len = self.build_string_len_load(rhs_payload, &format!("{label}_rhs"));
        let len_equal = self
            .builder
            .build_int_compare(IntPredicate::EQ, lhs_len, rhs_len, &format!("{label}_len_eq"))
            .expect("failed to compare string lens");
        let len_equal_block = self.context.append_basic_block(function, &format!("{label}_len_ok"));
        let false_block = self.context.append_basic_block(function, &format!("{label}_false"));
        let loop_block = self.context.append_basic_block(function, &format!("{label}_loop"));
        let body_block = self.context.append_basic_block(function, &format!("{label}_body"));
        let continue_block =
            self.context.append_basic_block(function, &format!("{label}_continue"));
        let done_block = self.context.append_basic_block(function, &format!("{label}_done"));
        self.builder
            .build_conditional_branch(len_equal, len_equal_block, false_block)
            .expect("failed string len branch");

        self.builder.position_at_end(false_block);
        self.builder.build_unconditional_branch(done_block).expect("failed string false branch");
        let false_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(len_equal_block);
        let lhs_data = self.build_string_ptr_load(lhs_payload, &format!("{label}_lhs"));
        let rhs_data = self.build_string_ptr_load(rhs_payload, &format!("{label}_rhs"));
        self.builder.build_unconditional_branch(loop_block).expect("failed string loop jump");
        let len_ok_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_idx"))
            .expect("failed string idx phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), len_ok_end)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, lhs_len, &format!("{label}_more"))
            .expect("failed string loop compare");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed string loop branch");
        let loop_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(body_block);
        let lhs_addr = self
            .builder
            .build_ptr_to_int(lhs_data, self.i64_type, &format!("{label}_lhs_base"))
            .expect("failed lhs ptr to int");
        let rhs_addr = self
            .builder
            .build_ptr_to_int(rhs_data, self.i64_type, &format!("{label}_rhs_base"))
            .expect("failed rhs ptr to int");
        let lhs_elem_addr = self
            .builder
            .build_int_add(lhs_addr, idx, &format!("{label}_lhs_addr"))
            .expect("failed lhs elem addr");
        let rhs_elem_addr = self
            .builder
            .build_int_add(rhs_addr, idx, &format!("{label}_rhs_addr"))
            .expect("failed rhs elem addr");
        let lhs_elem_ptr = self
            .builder
            .build_int_to_ptr(
                lhs_elem_addr,
                self.context.ptr_type(Default::default()),
                &format!("{label}_lhs_ptr"),
            )
            .expect("failed lhs elem ptr");
        let rhs_elem_ptr = self
            .builder
            .build_int_to_ptr(
                rhs_elem_addr,
                self.context.ptr_type(Default::default()),
                &format!("{label}_rhs_ptr"),
            )
            .expect("failed rhs elem ptr");
        let lhs_byte = self
            .builder
            .build_load(self.context.i8_type(), lhs_elem_ptr, &format!("{label}_lhs_byte"))
            .expect("failed lhs byte load")
            .into_int_value();
        let rhs_byte = self
            .builder
            .build_load(self.context.i8_type(), rhs_elem_ptr, &format!("{label}_rhs_byte"))
            .expect("failed rhs byte load")
            .into_int_value();
        let bytes_equal = self
            .builder
            .build_int_compare(IntPredicate::EQ, lhs_byte, rhs_byte, &format!("{label}_byte_eq"))
            .expect("failed byte compare");
        self.builder
            .build_conditional_branch(bytes_equal, continue_block, false_block)
            .expect("failed byte branch");

        self.builder.position_at_end(continue_block);
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), &format!("{label}_next"))
            .expect("failed string next");
        self.builder.build_unconditional_branch(loop_block).expect("failed continue to loop");
        let continue_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next, continue_end)]);

        self.builder.position_at_end(done_block);
        let result_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_result"))
            .expect("failed string result phi");
        result_phi.add_incoming(&[
            (&self.i64_type.const_zero(), false_end),
            (&self.i64_type.const_int(1, false), loop_end),
        ]);
        result_phi.as_basic_value().into_int_value()
    }

    #[cfg(feature = "wasi")]
    pub(super) fn get_or_create_static_bytes_global(
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
        let values =
            bytes.iter().map(|byte| byte_type.const_int(*byte as u64, false)).collect::<Vec<_>>();
        global.set_initializer(&byte_type.const_array(&values));
        global
    }

    #[cfg(feature = "wasi")]
    pub(super) fn build_static_bytes_ptr(
        &self,
        name: &str,
        bytes: &[u8],
        label: &str,
    ) -> PointerValue<'ctx> {
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
    pub(super) fn build_wasi_write_const(&self, global_name: &str, bytes: &[u8], label: &str) {
        let write_bytes = self.require_func("__wasi_write_bytes");
        let ptr = self.build_static_bytes_ptr(global_name, bytes, label);
        self.builder
            .build_call(
                write_bytes,
                &[ptr.into(), self.context.i32_type().const_int(bytes.len() as u64, false).into()],
                &format!("{label}_write"),
            )
            .expect("failed to write static bytes");
    }

    pub(super) fn expect_tag_payload(
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

    pub(super) fn expect_tag_int(
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
            self.builder.get_insert_block().unwrap().get_parent().unwrap(),
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
            self.builder.get_insert_block().unwrap().get_parent().unwrap(),
            &format!("{label}_non_neg_ok"),
        );
        self.builder
            .build_conditional_branch(non_neg, non_neg_block, trap_block)
            .expect("failed to branch on non-negative index");
        self.builder.position_at_end(non_neg_block);
        value.payload
    }

    pub(super) fn build_list_header_ptr(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_int_to_ptr(
                payload,
                self.context.ptr_type(Default::default()),
                &format!("{label}_header_ptr"),
            )
            .expect("failed to convert list payload to pointer")
    }

    pub(super) fn build_string_header_ptr(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_int_to_ptr(
                payload,
                self.context.ptr_type(Default::default()),
                &format!("{label}_string_header_ptr"),
            )
            .expect("failed to convert string payload to pointer")
    }

    pub(super) fn build_string_len_load(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let ptr = self.build_string_header_ptr(payload, label);
        let len_ptr = self
            .builder
            .build_struct_gep(self.string_header_type(), ptr, 0, &format!("{label}_len_ptr"))
            .expect("failed to build string len gep");
        self.builder
            .build_load(self.i64_type, len_ptr, &format!("{label}_len"))
            .expect("failed to load string len")
            .into_int_value()
    }

    pub(super) fn build_string_cap_store(
        &self,
        payload: IntValue<'ctx>,
        cap: IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_string_header_ptr(payload, label);
        let cap_ptr = self
            .builder
            .build_struct_gep(self.string_header_type(), ptr, 1, &format!("{label}_cap_ptr"))
            .expect("failed to build string cap gep");
        self.builder.build_store(cap_ptr, cap).expect("failed to store string cap");
    }

    pub(super) fn build_string_len_store(
        &self,
        payload: IntValue<'ctx>,
        len: IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_string_header_ptr(payload, label);
        let len_ptr = self
            .builder
            .build_struct_gep(self.string_header_type(), ptr, 0, &format!("{label}_len_ptr"))
            .expect("failed to build string len gep");
        self.builder.build_store(len_ptr, len).expect("failed to store string len");
    }

    pub(super) fn build_string_ptr_load(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        let ptr = self.build_string_header_ptr(payload, label);
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(self.string_header_type(), ptr, 2, &format!("{label}_ptr_ptr"))
            .expect("failed to build string ptr gep");
        self.builder
            .build_load(
                self.context.ptr_type(Default::default()),
                data_ptr_ptr,
                &format!("{label}_ptr"),
            )
            .expect("failed to load string ptr")
            .into_pointer_value()
    }

    pub(super) fn build_string_ptr_store(
        &self,
        payload: IntValue<'ctx>,
        ptr_value: PointerValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_string_header_ptr(payload, label);
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(self.string_header_type(), ptr, 2, &format!("{label}_ptr_ptr"))
            .expect("failed to build string ptr gep");
        self.builder.build_store(data_ptr_ptr, ptr_value).expect("failed to store string ptr");
    }

    pub(super) fn build_string_iter_header_ptr(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_int_to_ptr(
                payload,
                self.context.ptr_type(Default::default()),
                &format!("{label}_string_iter_header_ptr"),
            )
            .expect("failed to convert string iter payload to pointer")
    }

    pub(super) fn build_string_iter_string_load(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let ptr = self.build_string_iter_header_ptr(payload, label);
        let string_ptr = self
            .builder
            .build_struct_gep(
                self.string_iter_header_type(),
                ptr,
                0,
                &format!("{label}_string_ptr_ptr"),
            )
            .expect("failed to build string iter string ptr");
        self.builder
            .build_load(self.i64_type, string_ptr, &format!("{label}_string_ptr"))
            .expect("failed to load string iter string ptr")
            .into_int_value()
    }

    pub(super) fn build_string_iter_string_store(
        &self,
        payload: IntValue<'ctx>,
        string_payload: IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_string_iter_header_ptr(payload, label);
        let string_ptr = self
            .builder
            .build_struct_gep(
                self.string_iter_header_type(),
                ptr,
                0,
                &format!("{label}_string_ptr_ptr"),
            )
            .expect("failed to build string iter string store ptr");
        self.builder
            .build_store(string_ptr, string_payload)
            .expect("failed to store string iter string ptr");
    }

    pub(super) fn build_string_iter_index_load(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let ptr = self.build_string_iter_header_ptr(payload, label);
        let index_ptr = self
            .builder
            .build_struct_gep(self.string_iter_header_type(), ptr, 1, &format!("{label}_index_ptr"))
            .expect("failed to build string iter index ptr");
        self.builder
            .build_load(self.i64_type, index_ptr, &format!("{label}_index"))
            .expect("failed to load string iter index")
            .into_int_value()
    }

    pub(super) fn build_string_iter_index_store(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_string_iter_header_ptr(payload, label);
        let index_ptr = self
            .builder
            .build_struct_gep(self.string_iter_header_type(), ptr, 1, &format!("{label}_index_ptr"))
            .expect("failed to build string iter index store ptr");
        self.builder.build_store(index_ptr, index).expect("failed to store string iter index");
    }

    pub(super) fn build_string_concat(
        &self,
        lhs: CompiledValue<'ctx>,
        rhs: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let lhs_trap = self.context.append_basic_block(function, "string_concat_lhs_trap");
        let lhs_ok = self.context.append_basic_block(function, "string_concat_lhs_ok");
        let lhs_raw =
            self.expect_tag_payload(lhs, TAG_STRING, "string_concat_lhs", lhs_ok, lhs_trap);
        self.builder.position_at_end(lhs_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(lhs_ok);
        let rhs_trap = self.context.append_basic_block(function, "string_concat_rhs_trap");
        let rhs_ok = self.context.append_basic_block(function, "string_concat_rhs_ok");
        let rhs_raw =
            self.expect_tag_payload(rhs, TAG_STRING, "string_concat_rhs", rhs_ok, rhs_trap);
        self.builder.position_at_end(rhs_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(rhs_ok);
        let lhs_len = self.build_string_len_load(lhs_raw, "string_concat_lhs");
        let rhs_len = self.build_string_len_load(rhs_raw, "string_concat_rhs");
        let total_len = self
            .builder
            .build_int_add(lhs_len, rhs_len, "string_concat_total_len")
            .expect("failed to add string lengths");

        let alloc = self.require_func("__alloc");
        let align = self.i64_type.const_int(8, false);
        let data_raw = self.build_boxed_call(alloc, &[total_len, align], "string_concat_data");
        let data_ptr = self
            .builder
            .build_int_to_ptr(
                data_raw,
                self.context.ptr_type(Default::default()),
                "string_concat_data_ptr",
            )
            .expect("failed to convert string concat data ptr");
        let lhs_data = self.build_string_ptr_load(lhs_raw, "string_concat_lhs");
        let rhs_data = self.build_string_ptr_load(rhs_raw, "string_concat_rhs");

        let copy_into = |this: &Self,
                         src_ptr: PointerValue<'ctx>,
                         start_index: IntValue<'ctx>,
                         copy_len: IntValue<'ctx>,
                         label: &str| {
            let function = this.builder.get_insert_block().unwrap().get_parent().unwrap();
            let loop_block = this.context.append_basic_block(function, &format!("{label}_loop"));
            let body_block = this.context.append_basic_block(function, &format!("{label}_body"));
            let done_block = this.context.append_basic_block(function, &format!("{label}_done"));
            this.builder
                .build_unconditional_branch(loop_block)
                .expect("failed to branch to string concat loop");
            let entry_end = this.builder.get_insert_block().unwrap();

            this.builder.position_at_end(loop_block);
            let idx_phi = this
                .builder
                .build_phi(this.i64_type, &format!("{label}_idx"))
                .expect("failed to build string concat idx phi");
            idx_phi.add_incoming(&[(&this.i64_type.const_zero(), entry_end)]);
            let idx = idx_phi.as_basic_value().into_int_value();
            let more = this
                .builder
                .build_int_compare(IntPredicate::ULT, idx, copy_len, &format!("{label}_more"))
                .expect("failed to compare string concat idx");
            this.builder
                .build_conditional_branch(more, body_block, done_block)
                .expect("failed to branch in string concat loop");

            this.builder.position_at_end(body_block);
            let src_addr = this
                .builder
                .build_int_add(
                    this.builder
                        .build_ptr_to_int(src_ptr, this.i64_type, &format!("{label}_src_base"))
                        .expect("failed src ptr-to-int"),
                    idx,
                    &format!("{label}_src_addr"),
                )
                .expect("failed string concat src addr");
            let dst_index = this
                .builder
                .build_int_add(start_index, idx, &format!("{label}_dst_index"))
                .expect("failed string concat dst index");
            let dst_addr = this
                .builder
                .build_int_add(
                    this.builder
                        .build_ptr_to_int(data_ptr, this.i64_type, &format!("{label}_dst_base"))
                        .expect("failed dst ptr-to-int"),
                    dst_index,
                    &format!("{label}_dst_addr"),
                )
                .expect("failed string concat dst addr");
            let src_byte_ptr = this
                .builder
                .build_int_to_ptr(
                    src_addr,
                    this.context.ptr_type(Default::default()),
                    &format!("{label}_src_ptr"),
                )
                .expect("failed string concat src ptr");
            let dst_byte_ptr = this
                .builder
                .build_int_to_ptr(
                    dst_addr,
                    this.context.ptr_type(Default::default()),
                    &format!("{label}_dst_ptr"),
                )
                .expect("failed string concat dst ptr");
            let byte = this
                .builder
                .build_load(this.context.i8_type(), src_byte_ptr, &format!("{label}_byte"))
                .expect("failed to load string concat byte");
            this.builder
                .build_store(dst_byte_ptr, byte)
                .expect("failed to store string concat byte");
            let next_idx = this
                .builder
                .build_int_add(idx, this.i64_type.const_int(1, false), &format!("{label}_next_idx"))
                .expect("failed string concat next idx");
            this.builder
                .build_unconditional_branch(loop_block)
                .expect("failed string concat loop continue");
            let body_end = this.builder.get_insert_block().unwrap();
            idx_phi.add_incoming(&[(&next_idx, body_end)]);

            this.builder.position_at_end(done_block);
        };

        copy_into(self, lhs_data, self.i64_type.const_zero(), lhs_len, "string_concat_lhs_copy");
        copy_into(self, rhs_data, lhs_len, rhs_len, "string_concat_rhs_copy");

        let header_size = self.i64_type.const_int(STRING_HEADER_SIZE as u64, false);
        let header_raw =
            self.build_boxed_call(alloc, &[header_size, align], "string_concat_header");
        self.build_string_len_store(header_raw, total_len, "string_concat");
        self.build_string_cap_store(header_raw, total_len, "string_concat");
        self.build_string_ptr_store(header_raw, data_ptr, "string_concat");
        CompiledValue {
            tag: self.i64_type.const_int(TAG_STRING as u64, false),
            payload: header_raw,
        }
    }

    pub(super) fn build_string_header_from_parts(
        &self,
        data_ptr: PointerValue<'ctx>,
        len: IntValue<'ctx>,
        label: &str,
    ) -> CompiledValue<'ctx> {
        let alloc = self.require_func("__alloc");
        let align = self.i64_type.const_int(8, false);
        let header_size = self.i64_type.const_int(STRING_HEADER_SIZE as u64, false);
        let header_raw =
            self.build_boxed_call(alloc, &[header_size, align], &format!("{label}_header"));
        self.build_string_len_store(header_raw, len, label);
        self.build_string_cap_store(header_raw, len, label);
        self.build_string_ptr_store(header_raw, data_ptr, label);
        CompiledValue {
            tag: self.i64_type.const_int(TAG_STRING as u64, false),
            payload: header_raw,
        }
    }

    pub(super) fn build_string_chars(
        &self,
        string_value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let string_trap = self.context.append_basic_block(function, "string_chars_trap");
        let string_ok = self.context.append_basic_block(function, "string_chars_ok");
        let string_raw = self.expect_tag_payload(
            string_value,
            TAG_STRING,
            "string_chars",
            string_ok,
            string_trap,
        );
        self.builder.position_at_end(string_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(string_ok);
        let alloc = self.require_func("__alloc");
        let align = self.i64_type.const_int(8, false);
        let header_size = self.i64_type.const_int(STRING_ITER_HEADER_SIZE as u64, false);
        let header_raw =
            self.build_boxed_call(alloc, &[header_size, align], "string_chars_iter_header");
        self.build_string_iter_string_store(header_raw, string_raw, "string_chars");
        self.build_string_iter_index_store(header_raw, self.i64_type.const_zero(), "string_chars");
        CompiledValue {
            tag: self.i64_type.const_int(TAG_STRING_ITER as u64, false),
            payload: header_raw,
        }
    }

    pub(super) fn build_string_iter_done(
        &self,
        iter_value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let iter_trap = self.context.append_basic_block(function, "string_iter_done_trap");
        let iter_ok = self.context.append_basic_block(function, "string_iter_done_ok");
        let iter_raw = self.expect_tag_payload(
            iter_value,
            TAG_STRING_ITER,
            "string_iter_done",
            iter_ok,
            iter_trap,
        );
        self.builder.position_at_end(iter_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(iter_ok);
        let string_raw = self.build_string_iter_string_load(iter_raw, "string_iter_done");
        let index = self.build_string_iter_index_load(iter_raw, "string_iter_done");
        let len = self.build_string_len_load(string_raw, "string_iter_done");
        let done = self
            .builder
            .build_int_compare(IntPredicate::UGE, index, len, "string_iter_done_cmp")
            .expect("failed string_iter_done compare");
        self.int_value(
            self.builder
                .build_int_z_extend(done, self.i64_type, "string_iter_done_i64")
                .expect("failed string_iter_done zext"),
        )
    }

    pub(super) fn build_string_iter_next(
        &self,
        iter_value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let iter_trap = self.context.append_basic_block(function, "string_iter_next_trap");
        let iter_ok = self.context.append_basic_block(function, "string_iter_next_ok");
        let iter_raw = self.expect_tag_payload(
            iter_value,
            TAG_STRING_ITER,
            "string_iter_next",
            iter_ok,
            iter_trap,
        );
        self.builder.position_at_end(iter_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(iter_ok);
        let string_raw = self.build_string_iter_string_load(iter_raw, "string_iter_next");
        let index = self.build_string_iter_index_load(iter_raw, "string_iter_next");
        let len = self.build_string_len_load(string_raw, "string_iter_next");
        let not_done = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, len, "string_iter_next_not_done")
            .expect("failed string_iter_next done compare");
        let done_trap = self.context.append_basic_block(function, "string_iter_next_done_trap");
        let decode_block = self.context.append_basic_block(function, "string_iter_next_decode");
        self.builder
            .build_conditional_branch(not_done, decode_block, done_trap)
            .expect("failed string_iter_next branch");
        self.builder.position_at_end(done_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(decode_block);
        let data_ptr = self.build_string_ptr_load(string_raw, "string_iter_next");
        let (codepoint, next_index) =
            self.build_utf8_decode_forward(data_ptr, len, index, function, "string_iter_next");
        self.build_string_iter_index_store(iter_raw, next_index, "string_iter_next");
        self.int_value(codepoint)
    }

    pub(super) fn build_utf8_decode_forward(
        &self,
        data_ptr: PointerValue<'ctx>,
        len: IntValue<'ctx>,
        index: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        label: &str,
    ) -> (IntValue<'ctx>, IntValue<'ctx>) {
        let lead = self.build_byte_load_at(data_ptr, index, &format!("{label}_lead"));
        let ascii_block = self.context.append_basic_block(function, &format!("{label}_ascii"));
        let non_ascii_block =
            self.context.append_basic_block(function, &format!("{label}_non_ascii"));
        let two_block = self.context.append_basic_block(function, &format!("{label}_two"));
        let three_or_more_block =
            self.context.append_basic_block(function, &format!("{label}_three_or_more"));
        let three_block = self.context.append_basic_block(function, &format!("{label}_three"));
        let four_block = self.context.append_basic_block(function, &format!("{label}_four"));
        let done_block = self.context.append_basic_block(function, &format!("{label}_done"));

        let is_ascii = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                lead,
                self.i64_type.const_int(0x80, false),
                &format!("{label}_is_ascii"),
            )
            .expect("failed utf8 ascii compare");
        self.builder
            .build_conditional_branch(is_ascii, ascii_block, non_ascii_block)
            .expect("failed utf8 ascii branch");
        let _entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(ascii_block);
        let ascii_cp = lead;
        let ascii_next = self
            .builder
            .build_int_add(index, self.i64_type.const_int(1, false), &format!("{label}_ascii_next"))
            .expect("failed utf8 ascii next");
        self.builder.build_unconditional_branch(done_block).expect("failed utf8 ascii merge");
        let ascii_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(non_ascii_block);
        let is_two = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                lead,
                self.i64_type.const_int(0xe0, false),
                &format!("{label}_is_two"),
            )
            .expect("failed utf8 two compare");
        self.builder
            .build_conditional_branch(is_two, two_block, three_or_more_block)
            .expect("failed utf8 two branch");

        self.builder.position_at_end(two_block);
        let valid_lead = self
            .builder
            .build_int_compare(
                IntPredicate::UGE,
                lead,
                self.i64_type.const_int(0xc2, false),
                &format!("{label}_two_valid_lead"),
            )
            .expect("failed utf8 two valid lead");
        self.build_conditional_trap(valid_lead, function, &format!("{label}_two_lead"));
        let two_idx1 = self
            .builder
            .build_int_add(index, self.i64_type.const_int(1, false), &format!("{label}_two_idx1"))
            .expect("failed utf8 two idx1");
        let has_second = self
            .builder
            .build_int_compare(IntPredicate::ULT, two_idx1, len, &format!("{label}_two_has_second"))
            .expect("failed utf8 two len compare");
        self.build_conditional_trap(has_second, function, &format!("{label}_two_len"));
        let two_b1 = self.build_byte_load_at(data_ptr, two_idx1, &format!("{label}_two_b1"));
        self.build_trap_if_not_continuation_byte(two_b1, function, &format!("{label}_two_b1"));
        let two_cp = self
            .builder
            .build_or(
                self.builder
                    .build_left_shift(
                        self.builder
                            .build_and(
                                lead,
                                self.i64_type.const_int(0x1f, false),
                                &format!("{label}_two_lead_mask"),
                            )
                            .expect("failed utf8 two lead mask"),
                        self.i64_type.const_int(6, false),
                        &format!("{label}_two_lead_shift"),
                    )
                    .expect("failed utf8 two lead shift"),
                self.builder
                    .build_and(
                        two_b1,
                        self.i64_type.const_int(0x3f, false),
                        &format!("{label}_two_b1_mask"),
                    )
                    .expect("failed utf8 two b1 mask"),
                &format!("{label}_two_cp"),
            )
            .expect("failed utf8 two cp");
        let two_next = self
            .builder
            .build_int_add(index, self.i64_type.const_int(2, false), &format!("{label}_two_next"))
            .expect("failed utf8 two next");
        self.builder.build_unconditional_branch(done_block).expect("failed utf8 two merge");
        let two_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(three_or_more_block);
        let is_three = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                lead,
                self.i64_type.const_int(0xf0, false),
                &format!("{label}_is_three"),
            )
            .expect("failed utf8 three compare");
        self.builder
            .build_conditional_branch(is_three, three_block, four_block)
            .expect("failed utf8 three branch");

        self.builder.position_at_end(three_block);
        let three_idx1 = self
            .builder
            .build_int_add(index, self.i64_type.const_int(1, false), &format!("{label}_three_idx1"))
            .expect("failed utf8 three idx1");
        let three_idx2 = self
            .builder
            .build_int_add(index, self.i64_type.const_int(2, false), &format!("{label}_three_idx2"))
            .expect("failed utf8 three idx2");
        let has_third = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                three_idx2,
                len,
                &format!("{label}_three_has_third"),
            )
            .expect("failed utf8 three len compare");
        self.build_conditional_trap(has_third, function, &format!("{label}_three_len"));
        let three_b1 = self.build_byte_load_at(data_ptr, three_idx1, &format!("{label}_three_b1"));
        let three_b2 = self.build_byte_load_at(data_ptr, three_idx2, &format!("{label}_three_b2"));
        self.build_trap_if_not_continuation_byte(three_b1, function, &format!("{label}_three_b1"));
        self.build_trap_if_not_continuation_byte(three_b2, function, &format!("{label}_three_b2"));
        self.build_trap_if_invalid_three_byte_lead(
            lead,
            three_b1,
            function,
            &format!("{label}_three"),
        );
        let three_cp = self
            .builder
            .build_or(
                self.builder
                    .build_or(
                        self.builder
                            .build_left_shift(
                                self.builder
                                    .build_and(
                                        lead,
                                        self.i64_type.const_int(0x0f, false),
                                        &format!("{label}_three_lead_mask"),
                                    )
                                    .expect("failed utf8 three lead mask"),
                                self.i64_type.const_int(12, false),
                                &format!("{label}_three_lead_shift"),
                            )
                            .expect("failed utf8 three lead shift"),
                        self.builder
                            .build_left_shift(
                                self.builder
                                    .build_and(
                                        three_b1,
                                        self.i64_type.const_int(0x3f, false),
                                        &format!("{label}_three_b1_mask"),
                                    )
                                    .expect("failed utf8 three b1 mask"),
                                self.i64_type.const_int(6, false),
                                &format!("{label}_three_b1_shift"),
                            )
                            .expect("failed utf8 three b1 shift"),
                        &format!("{label}_three_hi"),
                    )
                    .expect("failed utf8 three hi"),
                self.builder
                    .build_and(
                        three_b2,
                        self.i64_type.const_int(0x3f, false),
                        &format!("{label}_three_b2_mask"),
                    )
                    .expect("failed utf8 three b2 mask"),
                &format!("{label}_three_cp"),
            )
            .expect("failed utf8 three cp");
        let three_next = self
            .builder
            .build_int_add(index, self.i64_type.const_int(3, false), &format!("{label}_three_next"))
            .expect("failed utf8 three next");
        self.builder.build_unconditional_branch(done_block).expect("failed utf8 three merge");
        let three_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(four_block);
        let valid_four_lead = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                lead,
                self.i64_type.const_int(0xf5, false),
                &format!("{label}_four_valid_lead"),
            )
            .expect("failed utf8 four valid lead");
        self.build_conditional_trap(valid_four_lead, function, &format!("{label}_four_lead"));
        let four_idx1 = self
            .builder
            .build_int_add(index, self.i64_type.const_int(1, false), &format!("{label}_four_idx1"))
            .expect("failed utf8 four idx1");
        let four_idx2 = self
            .builder
            .build_int_add(index, self.i64_type.const_int(2, false), &format!("{label}_four_idx2"))
            .expect("failed utf8 four idx2");
        let four_idx3 = self
            .builder
            .build_int_add(index, self.i64_type.const_int(3, false), &format!("{label}_four_idx3"))
            .expect("failed utf8 four idx3");
        let has_fourth = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                four_idx3,
                len,
                &format!("{label}_four_has_fourth"),
            )
            .expect("failed utf8 four len compare");
        self.build_conditional_trap(has_fourth, function, &format!("{label}_four_len"));
        let four_b1 = self.build_byte_load_at(data_ptr, four_idx1, &format!("{label}_four_b1"));
        let four_b2 = self.build_byte_load_at(data_ptr, four_idx2, &format!("{label}_four_b2"));
        let four_b3 = self.build_byte_load_at(data_ptr, four_idx3, &format!("{label}_four_b3"));
        self.build_trap_if_not_continuation_byte(four_b1, function, &format!("{label}_four_b1"));
        self.build_trap_if_not_continuation_byte(four_b2, function, &format!("{label}_four_b2"));
        self.build_trap_if_not_continuation_byte(four_b3, function, &format!("{label}_four_b3"));
        self.build_trap_if_invalid_four_byte_lead(
            lead,
            four_b1,
            function,
            &format!("{label}_four"),
        );
        let four_cp = self
            .builder
            .build_or(
                self.builder
                    .build_or(
                        self.builder
                            .build_or(
                                self.builder
                                    .build_left_shift(
                                        self.builder
                                            .build_and(
                                                lead,
                                                self.i64_type.const_int(0x07, false),
                                                &format!("{label}_four_lead_mask"),
                                            )
                                            .expect("failed utf8 four lead mask"),
                                        self.i64_type.const_int(18, false),
                                        &format!("{label}_four_lead_shift"),
                                    )
                                    .expect("failed utf8 four lead shift"),
                                self.builder
                                    .build_left_shift(
                                        self.builder
                                            .build_and(
                                                four_b1,
                                                self.i64_type.const_int(0x3f, false),
                                                &format!("{label}_four_b1_mask"),
                                            )
                                            .expect("failed utf8 four b1 mask"),
                                        self.i64_type.const_int(12, false),
                                        &format!("{label}_four_b1_shift"),
                                    )
                                    .expect("failed utf8 four b1 shift"),
                                &format!("{label}_four_hi_a"),
                            )
                            .expect("failed utf8 four hi a"),
                        self.builder
                            .build_left_shift(
                                self.builder
                                    .build_and(
                                        four_b2,
                                        self.i64_type.const_int(0x3f, false),
                                        &format!("{label}_four_b2_mask"),
                                    )
                                    .expect("failed utf8 four b2 mask"),
                                self.i64_type.const_int(6, false),
                                &format!("{label}_four_b2_shift"),
                            )
                            .expect("failed utf8 four b2 shift"),
                        &format!("{label}_four_hi_b"),
                    )
                    .expect("failed utf8 four hi b"),
                self.builder
                    .build_and(
                        four_b3,
                        self.i64_type.const_int(0x3f, false),
                        &format!("{label}_four_b3_mask"),
                    )
                    .expect("failed utf8 four b3 mask"),
                &format!("{label}_four_cp"),
            )
            .expect("failed utf8 four cp");
        let four_next = self
            .builder
            .build_int_add(index, self.i64_type.const_int(4, false), &format!("{label}_four_next"))
            .expect("failed utf8 four next");
        self.builder.build_unconditional_branch(done_block).expect("failed utf8 four merge");
        let four_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(done_block);
        let cp_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_cp_phi"))
            .expect("failed utf8 cp phi");
        let next_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_next_phi"))
            .expect("failed utf8 next phi");
        cp_phi.add_incoming(&[
            (&ascii_cp, ascii_end),
            (&two_cp, two_end),
            (&three_cp, three_end),
            (&four_cp, four_end),
        ]);
        next_phi.add_incoming(&[
            (&ascii_next, ascii_end),
            (&two_next, two_end),
            (&three_next, three_end),
            (&four_next, four_end),
        ]);
        (cp_phi.as_basic_value().into_int_value(), next_phi.as_basic_value().into_int_value())
    }

    pub(super) fn build_byte_load_at(
        &self,
        data_ptr: PointerValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(data_ptr, self.i64_type, &format!("{label}_base"))
                    .expect("failed byte base"),
                index,
                &format!("{label}_addr"),
            )
            .expect("failed byte addr");
        let ptr = self
            .builder
            .build_int_to_ptr(
                addr,
                self.context.ptr_type(Default::default()),
                &format!("{label}_ptr"),
            )
            .expect("failed byte ptr");
        let byte = self
            .builder
            .build_load(self.context.i8_type(), ptr, label)
            .expect("failed byte load")
            .into_int_value();
        self.builder
            .build_int_z_extend(byte, self.i64_type, &format!("{label}_i64"))
            .expect("failed byte zext")
    }

    pub(super) fn build_trap_if_not_continuation_byte(
        &self,
        byte: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        label: &str,
    ) {
        let masked = self
            .builder
            .build_and(byte, self.i64_type.const_int(0xc0, false), &format!("{label}_masked"))
            .expect("failed continuation mask");
        let ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                masked,
                self.i64_type.const_int(0x80, false),
                &format!("{label}_ok"),
            )
            .expect("failed continuation compare");
        self.build_conditional_trap(ok, function, label);
    }

    pub(super) fn build_trap_if_invalid_three_byte_lead(
        &self,
        lead: IntValue<'ctx>,
        b1: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        label: &str,
    ) {
        let not_e0 = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                lead,
                self.i64_type.const_int(0xe0, false),
                &format!("{label}_not_e0"),
            )
            .expect("failed not_e0");
        let b1_ge_a0 = self
            .builder
            .build_int_compare(
                IntPredicate::UGE,
                b1,
                self.i64_type.const_int(0xa0, false),
                &format!("{label}_b1_ge_a0"),
            )
            .expect("failed b1_ge_a0");
        let e0_ok = self
            .builder
            .build_or(not_e0, b1_ge_a0, &format!("{label}_e0_ok"))
            .expect("failed e0_ok");
        self.build_conditional_trap(e0_ok, function, &format!("{label}_e0"));

        let not_ed = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                lead,
                self.i64_type.const_int(0xed, false),
                &format!("{label}_not_ed"),
            )
            .expect("failed not_ed");
        let b1_lt_a0 = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                b1,
                self.i64_type.const_int(0xa0, false),
                &format!("{label}_b1_lt_a0"),
            )
            .expect("failed b1_lt_a0");
        let ed_ok = self
            .builder
            .build_or(not_ed, b1_lt_a0, &format!("{label}_ed_ok"))
            .expect("failed ed_ok");
        self.build_conditional_trap(ed_ok, function, &format!("{label}_ed"));
    }

    pub(super) fn build_trap_if_invalid_four_byte_lead(
        &self,
        lead: IntValue<'ctx>,
        b1: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        label: &str,
    ) {
        let not_f0 = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                lead,
                self.i64_type.const_int(0xf0, false),
                &format!("{label}_not_f0"),
            )
            .expect("failed not_f0");
        let b1_ge_90 = self
            .builder
            .build_int_compare(
                IntPredicate::UGE,
                b1,
                self.i64_type.const_int(0x90, false),
                &format!("{label}_b1_ge_90"),
            )
            .expect("failed b1_ge_90");
        let f0_ok = self
            .builder
            .build_or(not_f0, b1_ge_90, &format!("{label}_f0_ok"))
            .expect("failed f0_ok");
        self.build_conditional_trap(f0_ok, function, &format!("{label}_f0"));

        let not_f4 = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                lead,
                self.i64_type.const_int(0xf4, false),
                &format!("{label}_not_f4"),
            )
            .expect("failed not_f4");
        let b1_lt_90 = self
            .builder
            .build_int_compare(
                IntPredicate::ULT,
                b1,
                self.i64_type.const_int(0x90, false),
                &format!("{label}_b1_lt_90"),
            )
            .expect("failed b1_lt_90");
        let f4_ok = self
            .builder
            .build_or(not_f4, b1_lt_90, &format!("{label}_f4_ok"))
            .expect("failed f4_ok");
        self.build_conditional_trap(f4_ok, function, &format!("{label}_f4"));
    }

    pub(super) fn build_conditional_trap(
        &self,
        ok: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        label: &str,
    ) {
        let ok_block = self.context.append_basic_block(function, &format!("{label}_ok"));
        let trap_block = self.context.append_basic_block(function, &format!("{label}_trap"));
        self.builder
            .build_conditional_branch(ok, ok_block, trap_block)
            .expect("failed conditional trap branch");
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(ok_block);
    }

    pub(super) fn build_bytes_get(
        &self,
        string_value: CompiledValue<'ctx>,
        index_value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let string_trap = self.context.append_basic_block(function, "bytes_get_string_trap");
        let string_ok = self.context.append_basic_block(function, "bytes_get_string_ok");
        let string_raw = self.expect_tag_payload(
            string_value,
            TAG_STRING,
            "bytes_get_string",
            string_ok,
            string_trap,
        );
        self.builder.position_at_end(string_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(string_ok);
        let idx_trap = self.context.append_basic_block(function, "bytes_get_idx_trap");
        let idx = self.expect_tag_int(index_value, "bytes_get_index", idx_trap);

        let trap_block = self.context.append_basic_block(function, "bytes_get_bounds_trap");
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
        let len = self.build_string_len_load(string_raw, "bytes_get");
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

        self.builder.position_at_end(idx_trap);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(ok_block);
        let data_ptr = self.build_string_ptr_load(string_raw, "bytes_get");
        let base = self
            .builder
            .build_ptr_to_int(data_ptr, self.i64_type, "bytes_get_base")
            .expect("failed bytes_get ptr-to-int");
        let addr =
            self.builder.build_int_add(base, idx, "bytes_get_addr").expect("failed bytes_get addr");
        let ptr = self
            .builder
            .build_int_to_ptr(addr, self.context.ptr_type(Default::default()), "bytes_get_ptr")
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
        self.int_value(raw)
    }

    pub(super) fn build_bytes_slice(
        &self,
        string_value: CompiledValue<'ctx>,
        start_value: CompiledValue<'ctx>,
        end_value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let string_trap = self.context.append_basic_block(function, "bytes_slice_string_trap");
        let string_ok = self.context.append_basic_block(function, "bytes_slice_string_ok");
        let string_raw = self.expect_tag_payload(
            string_value,
            TAG_STRING,
            "bytes_slice_string",
            string_ok,
            string_trap,
        );
        self.builder.position_at_end(string_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(string_ok);
        let start_trap = self.context.append_basic_block(function, "bytes_slice_start_trap");
        let start = self.expect_tag_int(start_value, "bytes_slice_start", start_trap);
        let end_trap = self.context.append_basic_block(function, "bytes_slice_end_trap");
        let end = self.expect_tag_int(end_value, "bytes_slice_end", end_trap);

        let len = self.build_string_len_load(string_raw, "bytes_slice");
        let bounds_trap = self.context.append_basic_block(function, "bytes_slice_bounds_trap");
        let bounds_ok = self.context.append_basic_block(function, "bytes_slice_bounds_ok");
        let start_non_neg = self
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                start,
                self.i64_type.const_zero(),
                "bytes_slice_start_non_neg",
            )
            .expect("failed bytes_slice start non-neg");
        let end_non_neg = self
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                end,
                self.i64_type.const_zero(),
                "bytes_slice_end_non_neg",
            )
            .expect("failed bytes_slice end non-neg");
        let start_le_end = self
            .builder
            .build_int_compare(IntPredicate::ULE, start, end, "bytes_slice_start_le_end")
            .expect("failed bytes_slice start<=end");
        let end_in_bounds = self
            .builder
            .build_int_compare(IntPredicate::ULE, end, len, "bytes_slice_end_in_bounds")
            .expect("failed bytes_slice end<=len");
        let non_neg = self
            .builder
            .build_and(start_non_neg, end_non_neg, "bytes_slice_non_neg")
            .expect("failed bytes_slice non_neg");
        let range_ok = self
            .builder
            .build_and(non_neg, start_le_end, "bytes_slice_range_ok")
            .expect("failed bytes_slice range_ok");
        let all_ok = self
            .builder
            .build_and(range_ok, end_in_bounds, "bytes_slice_all_ok")
            .expect("failed bytes_slice all_ok");
        self.builder
            .build_conditional_branch(all_ok, bounds_ok, bounds_trap)
            .expect("failed bytes_slice bounds branch");

        self.builder.position_at_end(start_trap);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(end_trap);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(bounds_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(bounds_ok);
        let slice_len = self
            .builder
            .build_int_sub(end, start, "bytes_slice_len")
            .expect("failed bytes_slice len");
        let alloc = self.require_func("__alloc");
        let align = self.i64_type.const_int(8, false);
        let data_raw = self.build_boxed_call(alloc, &[slice_len, align], "bytes_slice_data");
        let data_ptr = self
            .builder
            .build_int_to_ptr(
                data_raw,
                self.context.ptr_type(Default::default()),
                "bytes_slice_data_ptr",
            )
            .expect("failed bytes_slice data ptr");
        let src_ptr = self.build_string_ptr_load(string_raw, "bytes_slice_src");
        let src_base = self
            .builder
            .build_ptr_to_int(src_ptr, self.i64_type, "bytes_slice_src_base")
            .expect("failed bytes_slice src base");
        let slice_src_addr = self
            .builder
            .build_int_add(src_base, start, "bytes_slice_src_addr")
            .expect("failed bytes_slice src addr");
        let slice_src_ptr = self
            .builder
            .build_int_to_ptr(
                slice_src_addr,
                self.context.ptr_type(Default::default()),
                "bytes_slice_src_ptr",
            )
            .expect("failed bytes_slice src ptr");

        let loop_block = self.context.append_basic_block(function, "bytes_slice_loop");
        let body_block = self.context.append_basic_block(function, "bytes_slice_body");
        let done_block = self.context.append_basic_block(function, "bytes_slice_done");
        self.builder.build_unconditional_branch(loop_block).expect("failed bytes_slice jump");
        let entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, "bytes_slice_idx")
            .expect("failed bytes_slice phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_end)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, slice_len, "bytes_slice_more")
            .expect("failed bytes_slice more");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed bytes_slice loop branch");

        self.builder.position_at_end(body_block);
        let src_addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(slice_src_ptr, self.i64_type, "bytes_slice_copy_src_base")
                    .expect("failed bytes_slice copy src base"),
                idx,
                "bytes_slice_copy_src_addr",
            )
            .expect("failed bytes_slice copy src addr");
        let dst_addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(data_ptr, self.i64_type, "bytes_slice_copy_dst_base")
                    .expect("failed bytes_slice copy dst base"),
                idx,
                "bytes_slice_copy_dst_addr",
            )
            .expect("failed bytes_slice copy dst addr");
        let src_byte_ptr = self
            .builder
            .build_int_to_ptr(
                src_addr,
                self.context.ptr_type(Default::default()),
                "bytes_slice_copy_src_ptr",
            )
            .expect("failed bytes_slice copy src ptr");
        let dst_byte_ptr = self
            .builder
            .build_int_to_ptr(
                dst_addr,
                self.context.ptr_type(Default::default()),
                "bytes_slice_copy_dst_ptr",
            )
            .expect("failed bytes_slice copy dst ptr");
        let byte = self
            .builder
            .build_load(self.context.i8_type(), src_byte_ptr, "bytes_slice_byte")
            .expect("failed bytes_slice load");
        self.builder.build_store(dst_byte_ptr, byte).expect("failed bytes_slice store");
        let next_idx = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "bytes_slice_next_idx")
            .expect("failed bytes_slice next idx");
        self.builder.build_unconditional_branch(loop_block).expect("failed bytes_slice continue");
        let body_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next_idx, body_end)]);

        self.builder.position_at_end(done_block);
        self.build_string_header_from_parts(data_ptr, slice_len, "bytes_slice")
    }

    pub(super) fn build_bytes_pop(
        &self,
        string_value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let string_trap = self.context.append_basic_block(function, "bytes_pop_string_trap");
        let string_ok = self.context.append_basic_block(function, "bytes_pop_string_ok");
        let string_raw = self.expect_tag_payload(
            string_value,
            TAG_STRING,
            "bytes_pop_string",
            string_ok,
            string_trap,
        );
        self.builder.position_at_end(string_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(string_ok);
        let len = self.build_string_len_load(string_raw, "bytes_pop");
        let trap_block = self.context.append_basic_block(function, "bytes_pop_empty_trap");
        let ok_block = self.context.append_basic_block(function, "bytes_pop_ok");
        let non_empty = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                len,
                self.i64_type.const_zero(),
                "bytes_pop_non_empty",
            )
            .expect("failed bytes_pop non-empty compare");
        self.builder
            .build_conditional_branch(non_empty, ok_block, trap_block)
            .expect("failed bytes_pop branch");
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(ok_block);
        let new_len = self
            .builder
            .build_int_sub(len, self.i64_type.const_int(1, false), "bytes_pop_new_len")
            .expect("failed bytes_pop new len");
        self.build_string_len_store(string_raw, new_len, "bytes_pop");
        let data_ptr = self.build_string_ptr_load(string_raw, "bytes_pop");
        let base = self
            .builder
            .build_ptr_to_int(data_ptr, self.i64_type, "bytes_pop_base")
            .expect("failed bytes_pop ptr-to-int");
        let addr = self
            .builder
            .build_int_add(base, new_len, "bytes_pop_addr")
            .expect("failed bytes_pop addr");
        let ptr = self
            .builder
            .build_int_to_ptr(addr, self.context.ptr_type(Default::default()), "bytes_pop_ptr")
            .expect("failed bytes_pop ptr");
        let byte = self
            .builder
            .build_load(self.context.i8_type(), ptr, "bytes_pop_byte")
            .expect("failed bytes_pop load")
            .into_int_value();
        let raw = self
            .builder
            .build_int_z_extend(byte, self.i64_type, "bytes_pop_i64")
            .expect("failed bytes_pop zext");
        self.int_value(raw)
    }

    pub(super) fn build_bytes_push(
        &self,
        string_value: CompiledValue<'ctx>,
        byte_value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let string_trap = self.context.append_basic_block(function, "bytes_push_string_trap");
        let string_ok = self.context.append_basic_block(function, "bytes_push_string_ok");
        let string_raw = self.expect_tag_payload(
            string_value,
            TAG_STRING,
            "bytes_push_string",
            string_ok,
            string_trap,
        );
        self.builder.position_at_end(string_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(string_ok);
        let byte_trap = self.context.append_basic_block(function, "bytes_push_byte_trap");
        let byte_raw = self.expect_tag_int(byte_value, "bytes_push_byte", byte_trap);

        let len = self.build_string_len_load(string_raw, "bytes_push");
        let cap_ptr = self
            .builder
            .build_struct_gep(
                self.string_header_type(),
                self.build_string_header_ptr(string_raw, "bytes_push_cap"),
                1,
                "bytes_push_cap_ptr",
            )
            .expect("failed bytes_push cap gep");
        let cap = self
            .builder
            .build_load(self.i64_type, cap_ptr, "bytes_push_cap")
            .expect("failed bytes_push cap load")
            .into_int_value();
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(
                self.string_header_type(),
                self.build_string_header_ptr(string_raw, "bytes_push_data"),
                2,
                "bytes_push_data_ptr_ptr",
            )
            .expect("failed bytes_push ptr gep");
        let data_ptr = self
            .builder
            .build_load(
                self.context.ptr_type(Default::default()),
                data_ptr_ptr,
                "bytes_push_data_ptr",
            )
            .expect("failed bytes_push data ptr load")
            .into_pointer_value();

        let grow_block = self.context.append_basic_block(function, "bytes_push_grow");
        let write_block = self.context.append_basic_block(function, "bytes_push_write");
        let merge_block = self.context.append_basic_block(function, "bytes_push_merge");
        self.builder
            .build_conditional_branch(
                self.builder
                    .build_int_compare(IntPredicate::ULT, len, cap, "bytes_push_has_capacity")
                    .expect("failed bytes_push capacity compare"),
                write_block,
                grow_block,
            )
            .expect("failed bytes_push branch");

        self.builder.position_at_end(byte_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(grow_block);
        let cap_is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                cap,
                self.i64_type.const_zero(),
                "bytes_push_cap_zero",
            )
            .expect("failed bytes_push cap zero");
        let doubled_cap = self
            .builder
            .build_int_add(cap, cap, "bytes_push_doubled_cap")
            .expect("failed bytes_push doubled cap");
        let new_cap = self
            .builder
            .build_select(
                cap_is_zero,
                self.i64_type.const_int(1, false),
                doubled_cap,
                "bytes_push_new_cap",
            )
            .expect("failed bytes_push new cap select")
            .into_int_value();
        let alloc = self.require_func("__alloc");
        let align = self.i64_type.const_int(8, false);
        let new_data_raw = self.build_boxed_call(alloc, &[new_cap, align], "bytes_push_new_data");
        let new_data_ptr = self
            .builder
            .build_int_to_ptr(
                new_data_raw,
                self.context.ptr_type(Default::default()),
                "bytes_push_new_data_ptr",
            )
            .expect("failed bytes_push new data ptr");
        self.build_copy_bytes_loop(data_ptr, new_data_ptr, len, function, "bytes_push_copy");
        self.builder
            .build_store(data_ptr_ptr, new_data_ptr)
            .expect("failed bytes_push store data ptr");
        self.builder.build_store(cap_ptr, new_cap).expect("failed bytes_push store cap");
        self.builder.build_unconditional_branch(merge_block).expect("failed bytes_push grow merge");
        let grow_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(write_block);
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bytes_push write merge");
        let write_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_block);
        let data_phi = self
            .builder
            .build_phi(self.context.ptr_type(Default::default()), "bytes_push_data_phi")
            .expect("failed bytes_push data phi");
        data_phi.add_incoming(&[(&new_data_ptr, grow_end), (&data_ptr, write_end)]);
        let active_data_ptr = data_phi.as_basic_value().into_pointer_value();
        let addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(active_data_ptr, self.i64_type, "bytes_push_base")
                    .expect("failed bytes_push base"),
                len,
                "bytes_push_addr",
            )
            .expect("failed bytes_push addr");
        let byte_ptr = self
            .builder
            .build_int_to_ptr(
                addr,
                self.context.ptr_type(Default::default()),
                "bytes_push_byte_ptr",
            )
            .expect("failed bytes_push byte ptr");
        let byte_i8 = self
            .builder
            .build_int_truncate(
                self.builder
                    .build_and(byte_raw, self.i64_type.const_int(0xff, false), "bytes_push_mask")
                    .expect("failed bytes_push mask"),
                self.context.i8_type(),
                "bytes_push_i8",
            )
            .expect("failed bytes_push truncate");
        self.builder.build_store(byte_ptr, byte_i8).expect("failed bytes_push store byte");
        let new_len = self
            .builder
            .build_int_add(len, self.i64_type.const_int(1, false), "bytes_push_new_len")
            .expect("failed bytes_push new len");
        self.build_string_len_store(string_raw, new_len, "bytes_push");
        string_value
    }

    pub(super) fn build_bytes_set(
        &self,
        string_value: CompiledValue<'ctx>,
        index_value: CompiledValue<'ctx>,
        byte_value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let string_trap = self.context.append_basic_block(function, "bytes_set_string_trap");
        let string_ok = self.context.append_basic_block(function, "bytes_set_string_ok");
        let string_raw = self.expect_tag_payload(
            string_value,
            TAG_STRING,
            "bytes_set_string",
            string_ok,
            string_trap,
        );
        self.builder.position_at_end(string_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(string_ok);
        let idx_trap = self.context.append_basic_block(function, "bytes_set_idx_trap");
        let idx = self.expect_tag_int(index_value, "bytes_set_index", idx_trap);
        let byte_trap = self.context.append_basic_block(function, "bytes_set_byte_trap");
        let byte_raw = self.expect_tag_int(byte_value, "bytes_set_byte", byte_trap);

        let trap_block = self.context.append_basic_block(function, "bytes_set_bounds_trap");
        let ok_block = self.context.append_basic_block(function, "bytes_set_ok");
        let len = self.build_string_len_load(string_raw, "bytes_set");
        let in_bounds = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, "bytes_set_in_bounds")
            .expect("failed bytes_set bounds compare");
        self.builder
            .build_conditional_branch(in_bounds, ok_block, trap_block)
            .expect("failed bytes_set branch");
        self.builder.position_at_end(idx_trap);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(byte_trap);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(ok_block);
        let data_ptr = self.build_string_ptr_load(string_raw, "bytes_set");
        let addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(data_ptr, self.i64_type, "bytes_set_base")
                    .expect("failed bytes_set base"),
                idx,
                "bytes_set_addr",
            )
            .expect("failed bytes_set addr");
        let byte_ptr = self
            .builder
            .build_int_to_ptr(addr, self.context.ptr_type(Default::default()), "bytes_set_ptr")
            .expect("failed bytes_set ptr");
        let byte_i8 = self
            .builder
            .build_int_truncate(
                self.builder
                    .build_and(byte_raw, self.i64_type.const_int(0xff, false), "bytes_set_mask")
                    .expect("failed bytes_set mask"),
                self.context.i8_type(),
                "bytes_set_i8",
            )
            .expect("failed bytes_set truncate");
        self.builder.build_store(byte_ptr, byte_i8).expect("failed bytes_set store");
        string_value
    }

    pub(super) fn build_bytes_insert(
        &self,
        string_value: CompiledValue<'ctx>,
        index_value: CompiledValue<'ctx>,
        byte_value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let string_trap = self.context.append_basic_block(function, "bytes_insert_string_trap");
        let string_ok = self.context.append_basic_block(function, "bytes_insert_string_ok");
        let string_raw = self.expect_tag_payload(
            string_value,
            TAG_STRING,
            "bytes_insert_string",
            string_ok,
            string_trap,
        );
        self.builder.position_at_end(string_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(string_ok);
        let idx_trap = self.context.append_basic_block(function, "bytes_insert_idx_trap");
        let idx = self.expect_tag_int(index_value, "bytes_insert_index", idx_trap);
        let byte_trap = self.context.append_basic_block(function, "bytes_insert_byte_trap");
        let byte_raw = self.expect_tag_int(byte_value, "bytes_insert_byte", byte_trap);

        let trap_block = self.context.append_basic_block(function, "bytes_insert_bounds_trap");
        let ok_block = self.context.append_basic_block(function, "bytes_insert_ok");
        let len = self.build_string_len_load(string_raw, "bytes_insert");
        let in_bounds = self
            .builder
            .build_int_compare(IntPredicate::ULE, idx, len, "bytes_insert_in_bounds")
            .expect("failed bytes_insert bounds compare");
        self.builder
            .build_conditional_branch(in_bounds, ok_block, trap_block)
            .expect("failed bytes_insert branch");
        self.builder.position_at_end(idx_trap);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(byte_trap);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(ok_block);
        let cap_ptr = self
            .builder
            .build_struct_gep(
                self.string_header_type(),
                self.build_string_header_ptr(string_raw, "bytes_insert_cap"),
                1,
                "bytes_insert_cap_ptr",
            )
            .expect("failed bytes_insert cap gep");
        let cap = self
            .builder
            .build_load(self.i64_type, cap_ptr, "bytes_insert_cap")
            .expect("failed bytes_insert cap load")
            .into_int_value();
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(
                self.string_header_type(),
                self.build_string_header_ptr(string_raw, "bytes_insert_data"),
                2,
                "bytes_insert_data_ptr_ptr",
            )
            .expect("failed bytes_insert ptr gep");
        let data_ptr = self
            .builder
            .build_load(
                self.context.ptr_type(Default::default()),
                data_ptr_ptr,
                "bytes_insert_data_ptr",
            )
            .expect("failed bytes_insert data ptr load")
            .into_pointer_value();

        let grow_block = self.context.append_basic_block(function, "bytes_insert_grow");
        let shift_setup_block =
            self.context.append_basic_block(function, "bytes_insert_shift_setup");
        let merge_block = self.context.append_basic_block(function, "bytes_insert_merge");
        self.builder
            .build_conditional_branch(
                self.builder
                    .build_int_compare(IntPredicate::ULT, len, cap, "bytes_insert_has_capacity")
                    .expect("failed bytes_insert capacity compare"),
                shift_setup_block,
                grow_block,
            )
            .expect("failed bytes_insert capacity branch");

        self.builder.position_at_end(grow_block);
        let cap_is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                cap,
                self.i64_type.const_zero(),
                "bytes_insert_cap_zero",
            )
            .expect("failed bytes_insert cap zero");
        let doubled_cap = self
            .builder
            .build_int_add(cap, cap, "bytes_insert_doubled_cap")
            .expect("failed bytes_insert doubled cap");
        let new_cap = self
            .builder
            .build_select(
                cap_is_zero,
                self.i64_type.const_int(1, false),
                doubled_cap,
                "bytes_insert_new_cap",
            )
            .expect("failed bytes_insert new cap select")
            .into_int_value();
        let alloc = self.require_func("__alloc");
        let align = self.i64_type.const_int(8, false);
        let new_data_raw = self.build_boxed_call(alloc, &[new_cap, align], "bytes_insert_new_data");
        let new_data_ptr = self
            .builder
            .build_int_to_ptr(
                new_data_raw,
                self.context.ptr_type(Default::default()),
                "bytes_insert_new_data_ptr",
            )
            .expect("failed bytes_insert new data ptr");
        self.build_copy_bytes_loop(data_ptr, new_data_ptr, len, function, "bytes_insert_copy");
        self.builder
            .build_store(data_ptr_ptr, new_data_ptr)
            .expect("failed bytes_insert store data ptr");
        self.builder.build_store(cap_ptr, new_cap).expect("failed bytes_insert store cap");
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bytes_insert grow merge");
        let grow_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(shift_setup_block);
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bytes_insert write merge");
        let shift_setup_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_block);
        let data_phi = self
            .builder
            .build_phi(self.context.ptr_type(Default::default()), "bytes_insert_data_phi")
            .expect("failed bytes_insert data phi");
        data_phi.add_incoming(&[(&new_data_ptr, grow_end), (&data_ptr, shift_setup_end)]);
        let active_data_ptr = data_phi.as_basic_value().into_pointer_value();

        let shift_loop = self.context.append_basic_block(function, "bytes_insert_shift_loop");
        let shift_body = self.context.append_basic_block(function, "bytes_insert_shift_body");
        let insert_block = self.context.append_basic_block(function, "bytes_insert_insert");
        self.builder
            .build_unconditional_branch(shift_loop)
            .expect("failed bytes_insert jump to shift loop");
        let shift_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(shift_loop);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, "bytes_insert_shift_idx")
            .expect("failed bytes_insert shift phi");
        idx_phi.add_incoming(&[(&len, shift_entry_end)]);
        let shift_idx = idx_phi.as_basic_value().into_int_value();
        let needs_shift = self
            .builder
            .build_int_compare(IntPredicate::UGT, shift_idx, idx, "bytes_insert_needs_shift")
            .expect("failed bytes_insert shift compare");
        self.builder
            .build_conditional_branch(needs_shift, shift_body, insert_block)
            .expect("failed bytes_insert shift branch");

        self.builder.position_at_end(shift_body);
        let src_idx = self
            .builder
            .build_int_sub(shift_idx, self.i64_type.const_int(1, false), "bytes_insert_src_idx")
            .expect("failed bytes_insert src idx");
        let src_addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(active_data_ptr, self.i64_type, "bytes_insert_src_base")
                    .expect("failed bytes_insert src base"),
                src_idx,
                "bytes_insert_src_addr",
            )
            .expect("failed bytes_insert src addr");
        let dst_addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(active_data_ptr, self.i64_type, "bytes_insert_dst_base")
                    .expect("failed bytes_insert dst base"),
                shift_idx,
                "bytes_insert_dst_addr",
            )
            .expect("failed bytes_insert dst addr");
        let src_ptr = self
            .builder
            .build_int_to_ptr(
                src_addr,
                self.context.ptr_type(Default::default()),
                "bytes_insert_src_ptr",
            )
            .expect("failed bytes_insert src ptr");
        let dst_ptr = self
            .builder
            .build_int_to_ptr(
                dst_addr,
                self.context.ptr_type(Default::default()),
                "bytes_insert_dst_ptr",
            )
            .expect("failed bytes_insert dst ptr");
        let moved_byte = self
            .builder
            .build_load(self.context.i8_type(), src_ptr, "bytes_insert_moved_byte")
            .expect("failed bytes_insert moved byte");
        self.builder
            .build_store(dst_ptr, moved_byte)
            .expect("failed bytes_insert moved byte store");
        self.builder
            .build_unconditional_branch(shift_loop)
            .expect("failed bytes_insert shift continue");
        let shift_body_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&src_idx, shift_body_end)]);

        self.builder.position_at_end(insert_block);
        let insert_addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(active_data_ptr, self.i64_type, "bytes_insert_insert_base")
                    .expect("failed bytes_insert insert base"),
                idx,
                "bytes_insert_insert_addr",
            )
            .expect("failed bytes_insert insert addr");
        let insert_ptr = self
            .builder
            .build_int_to_ptr(
                insert_addr,
                self.context.ptr_type(Default::default()),
                "bytes_insert_insert_ptr",
            )
            .expect("failed bytes_insert insert ptr");
        let byte_i8 = self
            .builder
            .build_int_truncate(
                self.builder
                    .build_and(byte_raw, self.i64_type.const_int(0xff, false), "bytes_insert_mask")
                    .expect("failed bytes_insert mask"),
                self.context.i8_type(),
                "bytes_insert_i8",
            )
            .expect("failed bytes_insert truncate");
        self.builder.build_store(insert_ptr, byte_i8).expect("failed bytes_insert store");
        let new_len = self
            .builder
            .build_int_add(len, self.i64_type.const_int(1, false), "bytes_insert_new_len")
            .expect("failed bytes_insert new len");
        self.build_string_len_store(string_raw, new_len, "bytes_insert");
        string_value
    }

    pub(super) fn build_bytes_remove(
        &self,
        string_value: CompiledValue<'ctx>,
        index_value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let string_trap = self.context.append_basic_block(function, "bytes_remove_string_trap");
        let string_ok = self.context.append_basic_block(function, "bytes_remove_string_ok");
        let string_raw = self.expect_tag_payload(
            string_value,
            TAG_STRING,
            "bytes_remove_string",
            string_ok,
            string_trap,
        );
        self.builder.position_at_end(string_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(string_ok);
        let idx_trap = self.context.append_basic_block(function, "bytes_remove_idx_trap");
        let idx = self.expect_tag_int(index_value, "bytes_remove_index", idx_trap);

        let trap_block = self.context.append_basic_block(function, "bytes_remove_bounds_trap");
        let ok_block = self.context.append_basic_block(function, "bytes_remove_ok");
        let len = self.build_string_len_load(string_raw, "bytes_remove");
        let in_bounds = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, "bytes_remove_in_bounds")
            .expect("failed bytes_remove bounds compare");
        self.builder
            .build_conditional_branch(in_bounds, ok_block, trap_block)
            .expect("failed bytes_remove branch");
        self.builder.position_at_end(idx_trap);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(ok_block);
        let data_ptr = self.build_string_ptr_load(string_raw, "bytes_remove");
        let removed_addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(data_ptr, self.i64_type, "bytes_remove_base")
                    .expect("failed bytes_remove base"),
                idx,
                "bytes_remove_addr",
            )
            .expect("failed bytes_remove addr");
        let removed_ptr = self
            .builder
            .build_int_to_ptr(
                removed_addr,
                self.context.ptr_type(Default::default()),
                "bytes_remove_ptr",
            )
            .expect("failed bytes_remove ptr");
        let removed_byte = self
            .builder
            .build_load(self.context.i8_type(), removed_ptr, "bytes_remove_byte")
            .expect("failed bytes_remove byte load")
            .into_int_value();

        let last_index = self
            .builder
            .build_int_sub(len, self.i64_type.const_int(1, false), "bytes_remove_last_index")
            .expect("failed bytes_remove last index");

        let shift_loop = self.context.append_basic_block(function, "bytes_remove_shift_loop");
        let shift_body = self.context.append_basic_block(function, "bytes_remove_shift_body");
        let done_block = self.context.append_basic_block(function, "bytes_remove_done");
        self.builder
            .build_unconditional_branch(shift_loop)
            .expect("failed bytes_remove jump to loop");
        let shift_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(shift_loop);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, "bytes_remove_shift_idx")
            .expect("failed bytes_remove shift phi");
        idx_phi.add_incoming(&[(&idx, shift_entry_end)]);
        let shift_idx = idx_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, shift_idx, last_index, "bytes_remove_more")
            .expect("failed bytes_remove compare");
        self.builder
            .build_conditional_branch(more, shift_body, done_block)
            .expect("failed bytes_remove branch");

        self.builder.position_at_end(shift_body);
        let src_idx = self
            .builder
            .build_int_add(shift_idx, self.i64_type.const_int(1, false), "bytes_remove_src_idx")
            .expect("failed bytes_remove src idx");
        let src_addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(data_ptr, self.i64_type, "bytes_remove_src_base")
                    .expect("failed bytes_remove src base"),
                src_idx,
                "bytes_remove_src_addr",
            )
            .expect("failed bytes_remove src addr");
        let dst_addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(data_ptr, self.i64_type, "bytes_remove_dst_base")
                    .expect("failed bytes_remove dst base"),
                shift_idx,
                "bytes_remove_dst_addr",
            )
            .expect("failed bytes_remove dst addr");
        let src_ptr = self
            .builder
            .build_int_to_ptr(
                src_addr,
                self.context.ptr_type(Default::default()),
                "bytes_remove_src_ptr",
            )
            .expect("failed bytes_remove src ptr");
        let dst_ptr = self
            .builder
            .build_int_to_ptr(
                dst_addr,
                self.context.ptr_type(Default::default()),
                "bytes_remove_dst_ptr",
            )
            .expect("failed bytes_remove dst ptr");
        let moved_byte = self
            .builder
            .build_load(self.context.i8_type(), src_ptr, "bytes_remove_moved_byte")
            .expect("failed bytes_remove moved byte");
        self.builder
            .build_store(dst_ptr, moved_byte)
            .expect("failed bytes_remove moved byte store");
        let next_idx = self
            .builder
            .build_int_add(shift_idx, self.i64_type.const_int(1, false), "bytes_remove_next_idx")
            .expect("failed bytes_remove next idx");
        self.builder.build_unconditional_branch(shift_loop).expect("failed bytes_remove continue");
        let shift_body_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next_idx, shift_body_end)]);

        self.builder.position_at_end(done_block);
        let new_len = self
            .builder
            .build_int_sub(len, self.i64_type.const_int(1, false), "bytes_remove_new_len")
            .expect("failed bytes_remove new len");
        self.build_string_len_store(string_raw, new_len, "bytes_remove");
        let removed_i64 = self
            .builder
            .build_int_z_extend(removed_byte, self.i64_type, "bytes_remove_i64")
            .expect("failed bytes_remove zext");
        self.int_value(removed_i64)
    }

    pub(super) fn build_string_copy(
        &self,
        string_value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let string_trap = self.context.append_basic_block(function, "string_copy_trap");
        let string_ok = self.context.append_basic_block(function, "string_copy_ok");
        let string_raw = self.expect_tag_payload(
            string_value,
            TAG_STRING,
            "string_copy",
            string_ok,
            string_trap,
        );
        self.builder.position_at_end(string_trap);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(string_ok);
        let len = self.build_string_len_load(string_raw, "string_copy");
        let alloc = self.require_func("__alloc");
        let align = self.i64_type.const_int(8, false);
        let data_raw = self.build_boxed_call(alloc, &[len, align], "string_copy_data");
        let data_ptr = self
            .builder
            .build_int_to_ptr(
                data_raw,
                self.context.ptr_type(Default::default()),
                "string_copy_data_ptr",
            )
            .expect("failed string_copy data ptr");
        let src_ptr = self.build_string_ptr_load(string_raw, "string_copy_src");
        self.build_copy_bytes_loop(src_ptr, data_ptr, len, function, "string_copy_copy");
        self.build_string_header_from_parts(data_ptr, len, "string_copy")
    }

    pub(super) fn build_copy_bytes_loop(
        &self,
        src_ptr: PointerValue<'ctx>,
        dst_ptr: PointerValue<'ctx>,
        len: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        label: &str,
    ) {
        let loop_block = self.context.append_basic_block(function, &format!("{label}_loop"));
        let body_block = self.context.append_basic_block(function, &format!("{label}_body"));
        let done_block = self.context.append_basic_block(function, &format!("{label}_done"));
        self.builder.build_unconditional_branch(loop_block).expect("failed copy loop jump");
        let entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_idx"))
            .expect("failed copy phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_end)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, &format!("{label}_more"))
            .expect("failed copy compare");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed copy branch");

        self.builder.position_at_end(body_block);
        let src_addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(src_ptr, self.i64_type, &format!("{label}_src_base"))
                    .expect("failed copy src base"),
                idx,
                &format!("{label}_src_addr"),
            )
            .expect("failed copy src addr");
        let dst_addr = self
            .builder
            .build_int_add(
                self.builder
                    .build_ptr_to_int(dst_ptr, self.i64_type, &format!("{label}_dst_base"))
                    .expect("failed copy dst base"),
                idx,
                &format!("{label}_dst_addr"),
            )
            .expect("failed copy dst addr");
        let src_byte_ptr = self
            .builder
            .build_int_to_ptr(
                src_addr,
                self.context.ptr_type(Default::default()),
                &format!("{label}_src_ptr"),
            )
            .expect("failed copy src ptr");
        let dst_byte_ptr = self
            .builder
            .build_int_to_ptr(
                dst_addr,
                self.context.ptr_type(Default::default()),
                &format!("{label}_dst_ptr"),
            )
            .expect("failed copy dst ptr");
        let byte = self
            .builder
            .build_load(self.context.i8_type(), src_byte_ptr, &format!("{label}_byte"))
            .expect("failed copy byte load");
        self.builder.build_store(dst_byte_ptr, byte).expect("failed copy byte store");
        let next_idx = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), &format!("{label}_next"))
            .expect("failed copy next idx");
        self.builder.build_unconditional_branch(loop_block).expect("failed copy continue");
        let body_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next_idx, body_end)]);

        self.builder.position_at_end(done_block);
    }

    pub(super) fn string_header_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context.struct_type(
            &[
                self.i64_type.into(),
                self.i64_type.into(),
                self.context.ptr_type(Default::default()).into(),
            ],
            false,
        )
    }

    pub(super) fn string_iter_header_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context.struct_type(&[self.i64_type.into(), self.i64_type.into()], false)
    }
}
