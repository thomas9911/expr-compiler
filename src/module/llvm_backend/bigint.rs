use super::*;

impl<'ctx> LlvmCompiler<'ctx> {
    pub(super) fn build_bigint_literal(&self, digits: &str, label: &str) -> CompiledValue<'ctx> {
        let zero = self.int_value(self.i64_type.const_zero());
        let mut acc = self.build_internal_call(
            self.require_func("bigint_from_int"),
            &[zero],
            &format!("{label}_init"),
        );
        let ten = self.build_internal_call(
            self.require_func("bigint_from_int"),
            &[self.int_value(self.i64_type.const_int(10, false))],
            &format!("{label}_ten"),
        );

        for (index, ch) in digits.chars().enumerate() {
            acc = self.build_internal_call(
                self.require_func("bigint_multiply"),
                &[acc, ten],
                &format!("{label}_mul_{index}"),
            );
            let digit = self.build_internal_call(
                self.require_func("bigint_from_int"),
                &[self.int_value(self.i64_type.const_int(ch.to_digit(10).unwrap() as u64, false))],
                &format!("{label}_digit_{index}"),
            );
            acc = self.build_internal_call(
                self.require_func("bigint_add"),
                &[acc, digit],
                &format!("{label}_add_{index}"),
            );
        }

        acc
    }

    pub(super) fn build_promote_value_to_bigint(
        &self,
        value: CompiledValue<'ctx>,
        function: FunctionValue<'ctx>,
        label: &str,
    ) -> CompiledValue<'ctx> {
        let entry_block = self.builder.get_insert_block().unwrap();
        let bigint_block = self.context.append_basic_block(function, &format!("{label}_bigint"));
        let int_check_block =
            self.context.append_basic_block(function, &format!("{label}_int_check"));
        let int_block = self.context.append_basic_block(function, &format!("{label}_int"));
        let trap_block = self.context.append_basic_block(function, &format!("{label}_trap"));
        let merge_block = self.context.append_basic_block(function, &format!("{label}_merge"));

        let is_bigint = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                &format!("{label}_is_bigint"),
            )
            .expect("failed bigint promotion bigint compare");
        self.builder
            .build_conditional_branch(is_bigint, bigint_block, int_check_block)
            .expect("failed bigint promotion first branch");

        self.builder.position_at_end(bigint_block);
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint promotion bigint merge");
        let bigint_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(int_check_block);
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                &format!("{label}_is_int"),
            )
            .expect("failed bigint promotion int compare");
        self.builder
            .build_conditional_branch(is_int, int_block, trap_block)
            .expect("failed bigint promotion second branch");

        self.builder.position_at_end(int_block);
        let promoted = self.build_internal_call(
            self.require_func("bigint_from_int"),
            &[value],
            &format!("{label}_promoted"),
        );
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint promotion int merge");
        let int_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();

        self.builder.position_at_end(merge_block);
        let tag_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_tag_phi"))
            .expect("failed bigint promotion tag phi");
        let payload_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_payload_phi"))
            .expect("failed bigint promotion payload phi");
        tag_phi.add_incoming(&[(&value.tag, bigint_end), (&promoted.tag, int_end)]);
        payload_phi.add_incoming(&[(&value.payload, bigint_end), (&promoted.payload, int_end)]);
        let promoted_value = CompiledValue {
            tag: tag_phi.as_basic_value().into_int_value(),
            payload: payload_phi.as_basic_value().into_int_value(),
        };
        debug_assert_eq!(entry_block.get_parent(), merge_block.get_parent());
        promoted_value
    }

    pub(super) fn compile_bigint_builtin(
        &self,
        name: &str,
        args: &[CompiledValue<'ctx>],
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        assert_eq!(args.len(), 2, "{name} expects 2 arguments");
        let lhs = self.build_promote_value_to_bigint(args[0], function, &format!("{name}_lhs"));
        let rhs = self.build_promote_value_to_bigint(args[1], function, &format!("{name}_rhs"));
        self.build_internal_call(self.require_func(name), &[lhs, rhs], name)
    }

    pub(super) fn build_bigint_header_ptr(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_int_to_ptr(
                payload,
                self.context.ptr_type(Default::default()),
                &format!("{label}_bigint_header_ptr"),
            )
            .expect("failed to convert bigint payload to pointer")
    }

    pub(super) fn build_bigint_sign_load(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let sign_ptr = self
            .builder
            .build_struct_gep(self.bigint_header_type(), ptr, 0, &format!("{label}_sign_ptr"))
            .expect("failed to build bigint sign gep");
        self.builder
            .build_load(self.i64_type, sign_ptr, &format!("{label}_sign"))
            .expect("failed to load bigint sign")
            .into_int_value()
    }

    pub(super) fn build_bigint_sign_store(
        &self,
        payload: IntValue<'ctx>,
        sign: IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let sign_ptr = self
            .builder
            .build_struct_gep(self.bigint_header_type(), ptr, 0, &format!("{label}_sign_ptr"))
            .expect("failed to build bigint sign gep");
        self.builder.build_store(sign_ptr, sign).expect("failed to store bigint sign");
    }

    pub(super) fn build_bigint_len_load(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let len_ptr = self
            .builder
            .build_struct_gep(self.bigint_header_type(), ptr, 1, &format!("{label}_len_ptr"))
            .expect("failed to build bigint len gep");
        self.builder
            .build_load(self.i64_type, len_ptr, &format!("{label}_len"))
            .expect("failed to load bigint len")
            .into_int_value()
    }

    pub(super) fn build_bigint_len_store(
        &self,
        payload: IntValue<'ctx>,
        len: IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let len_ptr = self
            .builder
            .build_struct_gep(self.bigint_header_type(), ptr, 1, &format!("{label}_len_ptr"))
            .expect("failed to build bigint len gep");
        self.builder.build_store(len_ptr, len).expect("failed to store bigint len");
    }

    pub(super) fn build_bigint_cap_store(
        &self,
        payload: IntValue<'ctx>,
        cap: IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let cap_ptr = self
            .builder
            .build_struct_gep(self.bigint_header_type(), ptr, 2, &format!("{label}_cap_ptr"))
            .expect("failed to build bigint cap gep");
        self.builder.build_store(cap_ptr, cap).expect("failed to store bigint cap");
    }

    pub(super) fn build_bigint_ptr_load(
        &self,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(self.bigint_header_type(), ptr, 3, &format!("{label}_ptr_ptr"))
            .expect("failed to build bigint data ptr gep");
        self.builder
            .build_load(
                self.context.ptr_type(Default::default()),
                data_ptr_ptr,
                &format!("{label}_ptr"),
            )
            .expect("failed to load bigint data ptr")
            .into_pointer_value()
    }

    pub(super) fn build_bigint_ptr_store(
        &self,
        payload: IntValue<'ctx>,
        ptr_value: PointerValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_bigint_header_ptr(payload, label);
        let data_ptr_ptr = self
            .builder
            .build_struct_gep(self.bigint_header_type(), ptr, 3, &format!("{label}_ptr_ptr"))
            .expect("failed to build bigint data ptr gep");
        self.builder.build_store(data_ptr_ptr, ptr_value).expect("failed to store bigint data ptr");
    }

    pub(super) fn build_bigint_limb_ptr(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) -> PointerValue<'ctx> {
        let data_ptr = self.build_bigint_ptr_load(payload, label);
        let byte_off = self
            .builder
            .build_left_shift(
                index,
                self.i64_type.const_int(2, false),
                &format!("{label}_byte_off"),
            )
            .expect("failed to shift bigint limb offset");
        let base = self
            .builder
            .build_ptr_to_int(data_ptr, self.i64_type, &format!("{label}_base_i64"))
            .expect("failed to ptr-to-int bigint data ptr");
        let addr = self
            .builder
            .build_int_add(base, byte_off, &format!("{label}_addr"))
            .expect("failed to compute bigint limb addr");
        self.builder
            .build_int_to_ptr(
                addr,
                self.context.ptr_type(Default::default()),
                &format!("{label}_limb_ptr"),
            )
            .expect("failed to int-to-ptr bigint limb ptr")
    }

    pub(super) fn build_bigint_limb_load(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let ptr = self.build_bigint_limb_ptr(payload, index, label);
        let limb32 = self
            .builder
            .build_load(self.context.i32_type(), ptr, &format!("{label}_limb32"))
            .expect("failed to load bigint limb")
            .into_int_value();
        self.builder
            .build_int_z_extend(limb32, self.i64_type, &format!("{label}_limb64"))
            .expect("failed to zext bigint limb")
    }

    pub(super) fn build_bigint_limb_store(
        &self,
        payload: IntValue<'ctx>,
        index: IntValue<'ctx>,
        limb: IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self.build_bigint_limb_ptr(payload, index, label);
        let limb32 = self
            .builder
            .build_int_truncate(limb, self.context.i32_type(), &format!("{label}_limb32"))
            .expect("failed to truncate bigint limb");
        self.builder.build_store(ptr, limb32).expect("failed to store bigint limb");
    }

    pub(super) fn build_bigint_alloc(&self, cap: IntValue<'ctx>, label: &str) -> IntValue<'ctx> {
        let alloc = self.require_func("__alloc");
        let limb_bytes = self
            .builder
            .build_int_mul(
                cap,
                self.i64_type.const_int(BIGINT_LIMB_SIZE as u64, false),
                &format!("{label}_limb_bytes"),
            )
            .expect("failed to build bigint limb bytes");
        let limb_ptr_raw = self.build_boxed_call(
            alloc,
            &[limb_bytes, self.i64_type.const_int(4, false)],
            &format!("{label}_limb_alloc"),
        );
        let limb_ptr = self
            .builder
            .build_int_to_ptr(
                limb_ptr_raw,
                self.context.ptr_type(Default::default()),
                &format!("{label}_limb_ptr"),
            )
            .expect("failed to convert bigint limb ptr");
        let header_ptr_raw = self.build_boxed_call(
            alloc,
            &[
                self.i64_type.const_int(BIGINT_HEADER_SIZE as u64, false),
                self.i64_type.const_int(8, false),
            ],
            &format!("{label}_header_alloc"),
        );
        self.build_bigint_sign_store(header_ptr_raw, self.i64_type.const_zero(), label);
        self.build_bigint_len_store(header_ptr_raw, self.i64_type.const_zero(), label);
        self.build_bigint_cap_store(header_ptr_raw, cap, label);
        self.build_bigint_ptr_store(header_ptr_raw, limb_ptr, label);
        header_ptr_raw
    }

    pub(super) fn build_bigint_zero(&self, label: &str) -> IntValue<'ctx> {
        let zero = self.i64_type.const_zero();
        let ptr = self.build_bigint_alloc(zero, label);
        self.build_bigint_sign_store(ptr, zero, label);
        self.build_bigint_len_store(ptr, zero, label);
        ptr
    }

    pub(super) fn build_bigint_one(&self, label: &str) -> IntValue<'ctx> {
        let one = self.i64_type.const_int(1, false);
        let ptr = self.build_bigint_alloc(one, label);
        self.build_bigint_sign_store(ptr, one, label);
        self.build_bigint_len_store(ptr, one, label);
        self.build_bigint_limb_store(ptr, self.i64_type.const_zero(), one, label);
        ptr
    }

    pub(super) fn build_bigint_normalize(&self, payload: IntValue<'ctx>, label: &str) {
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .expect("missing function for bigint normalize");
        let loop_block = self.context.append_basic_block(function, &format!("{label}_norm_loop"));
        let body_block = self.context.append_basic_block(function, &format!("{label}_norm_body"));
        let done_block = self.context.append_basic_block(function, &format!("{label}_norm_done"));
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to branch to bigint normalize loop");

        self.builder.position_at_end(loop_block);
        let len = self.build_bigint_len_load(payload, label);
        let has_len = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                len,
                self.i64_type.const_zero(),
                &format!("{label}_norm_has_len"),
            )
            .expect("failed to compare bigint normalize len");
        self.builder
            .build_conditional_branch(has_len, body_block, done_block)
            .expect("failed to branch bigint normalize len");

        self.builder.position_at_end(body_block);
        let last_index = self
            .builder
            .build_int_sub(len, self.i64_type.const_int(1, false), &format!("{label}_last_idx"))
            .expect("failed to build bigint normalize last idx");
        let last = self.build_bigint_limb_load(payload, last_index, &format!("{label}_last"));
        let is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                last,
                self.i64_type.const_zero(),
                &format!("{label}_norm_is_zero"),
            )
            .expect("failed to compare bigint normalize last limb");
        let trim_block = self.context.append_basic_block(function, &format!("{label}_norm_trim"));
        let keep_block = self.context.append_basic_block(function, &format!("{label}_norm_keep"));
        self.builder
            .build_conditional_branch(is_zero, trim_block, keep_block)
            .expect("failed to branch bigint normalize zero");

        self.builder.position_at_end(trim_block);
        self.build_bigint_len_store(payload, last_index, &format!("{label}_trim"));
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed to loop bigint normalize");

        self.builder.position_at_end(keep_block);
        self.builder
            .build_unconditional_branch(done_block)
            .expect("failed to branch bigint normalize done");

        self.builder.position_at_end(done_block);
        let final_len = self.build_bigint_len_load(payload, &format!("{label}_final"));
        let is_zero_len = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                final_len,
                self.i64_type.const_zero(),
                &format!("{label}_final_zero"),
            )
            .expect("failed to compare bigint final len");
        let set_zero_block =
            self.context.append_basic_block(function, &format!("{label}_set_zero_sign"));
        let end_block = self.context.append_basic_block(function, &format!("{label}_norm_end"));
        self.builder
            .build_conditional_branch(is_zero_len, set_zero_block, end_block)
            .expect("failed to branch bigint final zero");

        self.builder.position_at_end(set_zero_block);
        self.build_bigint_sign_store(payload, self.i64_type.const_zero(), &format!("{label}_zero"));
        self.builder
            .build_unconditional_branch(end_block)
            .expect("failed to branch bigint normalize end");

        self.builder.position_at_end(end_block);
    }

    pub(super) fn build_bigint_cmp_abs(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .expect("missing function for bigint cmp abs");
        let merge_block = self.context.append_basic_block(function, &format!("{label}_merge"));
        let lhs_len = self.build_bigint_len_load(lhs, &format!("{label}_lhs"));
        let rhs_len = self.build_bigint_len_load(rhs, &format!("{label}_rhs"));
        let len_eq = self
            .builder
            .build_int_compare(IntPredicate::EQ, lhs_len, rhs_len, &format!("{label}_len_eq"))
            .expect("failed bigint len eq compare");
        let len_cmp_block = self.context.append_basic_block(function, &format!("{label}_len_cmp"));
        let same_len_block =
            self.context.append_basic_block(function, &format!("{label}_same_len"));
        self.builder
            .build_conditional_branch(len_eq, same_len_block, len_cmp_block)
            .expect("failed bigint len branch");

        self.builder.position_at_end(len_cmp_block);
        let lhs_gt = self
            .builder
            .build_int_compare(IntPredicate::UGT, lhs_len, rhs_len, &format!("{label}_lhs_len_gt"))
            .expect("failed bigint len gt compare");
        let len_cmp = self
            .builder
            .build_select(
                lhs_gt,
                self.i64_type.const_int(1, true),
                self.i64_type.const_int((-1i64) as u64, true),
                &format!("{label}_len_cmp_value"),
            )
            .expect("failed bigint len cmp select")
            .into_int_value();
        self.builder.build_unconditional_branch(merge_block).expect("failed bigint len cmp jump");
        let len_cmp_block_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(same_len_block);
        let loop_block = self.context.append_basic_block(function, &format!("{label}_loop"));
        let body_block = self.context.append_basic_block(function, &format!("{label}_body"));
        let equal_block = self.context.append_basic_block(function, &format!("{label}_equal"));
        self.builder.build_unconditional_branch(loop_block).expect("failed bigint cmp loop jump");
        let same_len_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let remaining_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_remaining"))
            .expect("failed bigint cmp phi");
        remaining_phi.add_incoming(&[(&lhs_len, same_len_end)]);
        let remaining = remaining_phi.as_basic_value().into_int_value();
        let has_more = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                remaining,
                self.i64_type.const_zero(),
                &format!("{label}_has_more"),
            )
            .expect("failed bigint cmp remaining compare");
        self.builder
            .build_conditional_branch(has_more, body_block, equal_block)
            .expect("failed bigint cmp loop branch");

        self.builder.position_at_end(body_block);
        let index = self
            .builder
            .build_int_sub(remaining, self.i64_type.const_int(1, false), &format!("{label}_index"))
            .expect("failed bigint cmp index");
        let lhs_limb = self.build_bigint_limb_load(lhs, index, &format!("{label}_lhs_limb"));
        let rhs_limb = self.build_bigint_limb_load(rhs, index, &format!("{label}_rhs_limb"));
        let limb_eq = self
            .builder
            .build_int_compare(IntPredicate::EQ, lhs_limb, rhs_limb, &format!("{label}_limb_eq"))
            .expect("failed bigint cmp limb eq");
        let next_block = self.context.append_basic_block(function, &format!("{label}_next"));
        let diff_block = self.context.append_basic_block(function, &format!("{label}_diff"));
        self.builder
            .build_conditional_branch(limb_eq, next_block, diff_block)
            .expect("failed bigint cmp limb branch");

        self.builder.position_at_end(next_block);
        self.builder.build_unconditional_branch(loop_block).expect("failed bigint cmp continue");
        let next_end = self.builder.get_insert_block().unwrap();
        remaining_phi.add_incoming(&[(&index, next_end)]);

        self.builder.position_at_end(diff_block);
        let lhs_gt = self
            .builder
            .build_int_compare(IntPredicate::UGT, lhs_limb, rhs_limb, &format!("{label}_limb_gt"))
            .expect("failed bigint cmp limb gt");
        let limb_cmp = self
            .builder
            .build_select(
                lhs_gt,
                self.i64_type.const_int(1, true),
                self.i64_type.const_int((-1i64) as u64, true),
                &format!("{label}_limb_cmp"),
            )
            .expect("failed bigint cmp limb select")
            .into_int_value();
        self.builder.build_unconditional_branch(merge_block).expect("failed bigint cmp diff jump");
        let diff_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(equal_block);
        self.builder.build_unconditional_branch(merge_block).expect("failed bigint cmp equal jump");
        let equal_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_block);
        let phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_cmp"))
            .expect("failed bigint cmp result phi");
        phi.add_incoming(&[
            (&len_cmp, len_cmp_block_end),
            (&limb_cmp, diff_end),
            (&self.i64_type.const_zero(), equal_end),
        ]);
        phi.as_basic_value().into_int_value()
    }

    pub(super) fn build_bigint_add_abs(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let lhs_len = self.build_bigint_len_load(lhs, &format!("{label}_lhs"));
        let rhs_len = self.build_bigint_len_load(rhs, &format!("{label}_rhs"));
        let lhs_ge = self
            .builder
            .build_int_compare(IntPredicate::UGE, lhs_len, rhs_len, &format!("{label}_lhs_ge"))
            .expect("failed bigint add abs len compare");
        let max_len = self
            .builder
            .build_select(lhs_ge, lhs_len, rhs_len, &format!("{label}_max_len"))
            .expect("failed bigint add abs max len")
            .into_int_value();
        let cap = self
            .builder
            .build_int_add(max_len, self.i64_type.const_int(1, false), &format!("{label}_cap"))
            .expect("failed bigint add abs cap");
        let result = self.build_bigint_alloc(cap, &format!("{label}_alloc"));
        self.build_bigint_len_store(result, cap, label);

        let loop_block = self.context.append_basic_block(function, &format!("{label}_loop"));
        let body_block = self.context.append_basic_block(function, &format!("{label}_body"));
        let done_block = self.context.append_basic_block(function, &format!("{label}_done"));
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed bigint add abs loop jump");
        let entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_idx"))
            .expect("failed bigint add abs idx phi");
        let carry_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_carry"))
            .expect("failed bigint add abs carry phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_end)]);
        carry_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_end)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let carry = carry_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, max_len, &format!("{label}_more"))
            .expect("failed bigint add abs loop compare");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed bigint add abs loop branch");

        self.builder.position_at_end(body_block);
        let lhs_in = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, lhs_len, &format!("{label}_lhs_in"))
            .expect("failed bigint add abs lhs in");
        let lhs_read = self.context.append_basic_block(function, &format!("{label}_lhs_read"));
        let lhs_zero = self.context.append_basic_block(function, &format!("{label}_lhs_zero"));
        let lhs_merge = self.context.append_basic_block(function, &format!("{label}_lhs_merge"));
        self.builder
            .build_conditional_branch(lhs_in, lhs_read, lhs_zero)
            .expect("failed bigint add abs lhs branch");

        self.builder.position_at_end(lhs_read);
        let lhs_limb_val = self.build_bigint_limb_load(lhs, idx, &format!("{label}_lhs_limb"));
        self.builder.build_unconditional_branch(lhs_merge).expect("failed lhs merge jump");
        let lhs_read_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(lhs_zero);
        self.builder.build_unconditional_branch(lhs_merge).expect("failed lhs zero merge jump");
        let lhs_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(lhs_merge);
        let lhs_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_lhs_phi"))
            .expect("failed lhs phi");
        lhs_phi.add_incoming(&[
            (&lhs_limb_val, lhs_read_end),
            (&self.i64_type.const_zero(), lhs_zero_end),
        ]);
        let lhs_limb = lhs_phi.as_basic_value().into_int_value();

        let rhs_in = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, rhs_len, &format!("{label}_rhs_in"))
            .expect("failed bigint add abs rhs in");
        let rhs_read = self.context.append_basic_block(function, &format!("{label}_rhs_read"));
        let rhs_zero = self.context.append_basic_block(function, &format!("{label}_rhs_zero"));
        let rhs_merge = self.context.append_basic_block(function, &format!("{label}_rhs_merge"));
        self.builder
            .build_conditional_branch(rhs_in, rhs_read, rhs_zero)
            .expect("failed bigint add abs rhs branch");

        self.builder.position_at_end(rhs_read);
        let rhs_limb_val = self.build_bigint_limb_load(rhs, idx, &format!("{label}_rhs_limb"));
        self.builder.build_unconditional_branch(rhs_merge).expect("failed rhs merge jump");
        let rhs_read_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_zero);
        self.builder.build_unconditional_branch(rhs_merge).expect("failed rhs zero merge jump");
        let rhs_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_merge);
        let rhs_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_rhs_phi"))
            .expect("failed rhs phi");
        rhs_phi.add_incoming(&[
            (&rhs_limb_val, rhs_read_end),
            (&self.i64_type.const_zero(), rhs_zero_end),
        ]);
        let rhs_limb = rhs_phi.as_basic_value().into_int_value();

        let tmp = self
            .builder
            .build_int_add(lhs_limb, rhs_limb, &format!("{label}_tmp"))
            .expect("failed bigint add abs tmp");
        let sum = self
            .builder
            .build_int_add(tmp, carry, &format!("{label}_sum"))
            .expect("failed bigint add abs sum");
        let low = self
            .builder
            .build_and(sum, self.i64_type.const_int(0xffff_ffff, false), &format!("{label}_low"))
            .expect("failed bigint add abs low");
        self.build_bigint_limb_store(result, idx, low, &format!("{label}_store"));
        let next_carry = self
            .builder
            .build_right_shift(
                sum,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_next_carry"),
            )
            .expect("failed bigint add abs next carry");
        let next_idx = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), &format!("{label}_next_idx"))
            .expect("failed bigint add abs next idx");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed bigint add abs continue");
        let body_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next_idx, body_end)]);
        carry_phi.add_incoming(&[(&next_carry, body_end)]);

        self.builder.position_at_end(done_block);
        let final_carry = carry_phi.as_basic_value().into_int_value();
        self.build_bigint_limb_store(result, max_len, final_carry, &format!("{label}_final"));
        self.build_bigint_normalize(result, label);
        result
    }

    pub(super) fn build_bigint_sub_abs(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let lhs_len = self.build_bigint_len_load(lhs, &format!("{label}_lhs"));
        let rhs_len = self.build_bigint_len_load(rhs, &format!("{label}_rhs"));
        let result = self.build_bigint_alloc(lhs_len, &format!("{label}_alloc"));
        self.build_bigint_len_store(result, lhs_len, label);

        let loop_block = self.context.append_basic_block(function, &format!("{label}_loop"));
        let body_block = self.context.append_basic_block(function, &format!("{label}_body"));
        let done_block = self.context.append_basic_block(function, &format!("{label}_done"));
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed bigint sub abs loop jump");
        let entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_idx"))
            .expect("failed bigint sub abs idx phi");
        let borrow_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_borrow"))
            .expect("failed bigint sub abs borrow phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_end)]);
        borrow_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_end)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let borrow = borrow_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, lhs_len, &format!("{label}_more"))
            .expect("failed bigint sub abs loop compare");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed bigint sub abs loop branch");

        self.builder.position_at_end(body_block);
        let lhs_limb = self.build_bigint_limb_load(lhs, idx, &format!("{label}_lhs_limb"));
        let rhs_in = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, rhs_len, &format!("{label}_rhs_in"))
            .expect("failed bigint sub abs rhs in");
        let rhs_read = self.context.append_basic_block(function, &format!("{label}_rhs_read"));
        let rhs_zero = self.context.append_basic_block(function, &format!("{label}_rhs_zero"));
        let rhs_merge = self.context.append_basic_block(function, &format!("{label}_rhs_merge"));
        self.builder
            .build_conditional_branch(rhs_in, rhs_read, rhs_zero)
            .expect("failed bigint sub abs rhs branch");

        self.builder.position_at_end(rhs_read);
        let rhs_limb_val = self.build_bigint_limb_load(rhs, idx, &format!("{label}_rhs_limb"));
        self.builder
            .build_unconditional_branch(rhs_merge)
            .expect("failed bigint sub abs rhs merge jump");
        let rhs_read_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_zero);
        self.builder
            .build_unconditional_branch(rhs_merge)
            .expect("failed bigint sub abs rhs zero merge jump");
        let rhs_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_merge);
        let rhs_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_rhs_phi"))
            .expect("failed bigint sub abs rhs phi");
        rhs_phi.add_incoming(&[
            (&rhs_limb_val, rhs_read_end),
            (&self.i64_type.const_zero(), rhs_zero_end),
        ]);
        let rhs_limb = rhs_phi.as_basic_value().into_int_value();

        let rhs_plus_borrow = self
            .builder
            .build_int_add(rhs_limb, borrow, &format!("{label}_rhs_plus_borrow"))
            .expect("failed bigint sub abs rhs_plus_borrow");
        let enough = self
            .builder
            .build_int_compare(
                IntPredicate::UGE,
                lhs_limb,
                rhs_plus_borrow,
                &format!("{label}_enough"),
            )
            .expect("failed bigint sub abs enough compare");
        let no_borrow_block =
            self.context.append_basic_block(function, &format!("{label}_no_borrow"));
        let borrow_block =
            self.context.append_basic_block(function, &format!("{label}_borrow_block"));
        let merge = self.context.append_basic_block(function, &format!("{label}_merge"));
        self.builder
            .build_conditional_branch(enough, no_borrow_block, borrow_block)
            .expect("failed bigint sub abs enough branch");

        self.builder.position_at_end(no_borrow_block);
        let diff_no_borrow = self
            .builder
            .build_int_sub(lhs_limb, rhs_plus_borrow, &format!("{label}_diff_no_borrow"))
            .expect("failed bigint sub abs diff no borrow");
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed bigint sub abs no borrow jump");
        let no_borrow_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(borrow_block);
        let lhs_with_base = self
            .builder
            .build_int_add(
                lhs_limb,
                self.i64_type.const_int(1u64 << 32, false),
                &format!("{label}_lhs_with_base"),
            )
            .expect("failed bigint sub abs lhs_with_base");
        let diff_borrow = self
            .builder
            .build_int_sub(lhs_with_base, rhs_plus_borrow, &format!("{label}_diff_borrow"))
            .expect("failed bigint sub abs diff borrow");
        self.builder.build_unconditional_branch(merge).expect("failed bigint sub abs borrow jump");
        let borrow_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge);
        let diff_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_diff_phi"))
            .expect("failed bigint sub abs diff phi");
        let next_borrow_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_next_borrow_phi"))
            .expect("failed bigint sub abs next borrow phi");
        diff_phi.add_incoming(&[(&diff_no_borrow, no_borrow_end), (&diff_borrow, borrow_end)]);
        next_borrow_phi.add_incoming(&[
            (&self.i64_type.const_zero(), no_borrow_end),
            (&self.i64_type.const_int(1, false), borrow_end),
        ]);
        let out_limb = diff_phi.as_basic_value().into_int_value();
        let next_borrow = next_borrow_phi.as_basic_value().into_int_value();
        self.build_bigint_limb_store(result, idx, out_limb, &format!("{label}_store"));
        let next_idx = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), &format!("{label}_next_idx"))
            .expect("failed bigint sub abs next idx");
        self.builder
            .build_unconditional_branch(loop_block)
            .expect("failed bigint sub abs continue");
        let body_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next_idx, body_end)]);
        borrow_phi.add_incoming(&[(&next_borrow, body_end)]);

        self.builder.position_at_end(done_block);
        self.build_bigint_normalize(result, label);
        result
    }

    pub(super) fn build_bigint_mul_abs(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let lhs_len = self.build_bigint_len_load(lhs, &format!("{label}_lhs"));
        let rhs_len = self.build_bigint_len_load(rhs, &format!("{label}_rhs"));
        let cap = self
            .builder
            .build_int_add(lhs_len, rhs_len, &format!("{label}_cap"))
            .expect("failed bigint mul abs cap");
        let result = self.build_bigint_alloc(cap, &format!("{label}_alloc"));
        self.build_bigint_len_store(result, cap, label);

        let init_loop = self.context.append_basic_block(function, &format!("{label}_init_loop"));
        let init_body = self.context.append_basic_block(function, &format!("{label}_init_body"));
        let init_done = self.context.append_basic_block(function, &format!("{label}_init_done"));
        self.builder.build_unconditional_branch(init_loop).expect("failed bigint mul init jump");
        let init_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(init_loop);
        let init_idx_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_init_idx"))
            .expect("failed bigint mul init idx phi");
        init_idx_phi.add_incoming(&[(&self.i64_type.const_zero(), init_entry_end)]);
        let init_idx = init_idx_phi.as_basic_value().into_int_value();
        let init_more = self
            .builder
            .build_int_compare(IntPredicate::ULT, init_idx, cap, &format!("{label}_init_more"))
            .expect("failed bigint mul init compare");
        self.builder
            .build_conditional_branch(init_more, init_body, init_done)
            .expect("failed bigint mul init branch");

        self.builder.position_at_end(init_body);
        self.build_bigint_limb_store(
            result,
            init_idx,
            self.i64_type.const_zero(),
            &format!("{label}_init_store"),
        );
        let init_next = self
            .builder
            .build_int_add(
                init_idx,
                self.i64_type.const_int(1, false),
                &format!("{label}_init_next"),
            )
            .expect("failed bigint mul init next");
        self.builder.build_unconditional_branch(init_loop).expect("failed bigint mul init loop");
        let init_body_end = self.builder.get_insert_block().unwrap();
        init_idx_phi.add_incoming(&[(&init_next, init_body_end)]);

        self.builder.position_at_end(init_done);
        let outer_loop = self.context.append_basic_block(function, &format!("{label}_outer_loop"));
        let outer_body = self.context.append_basic_block(function, &format!("{label}_outer_body"));
        let outer_done = self.context.append_basic_block(function, &format!("{label}_outer_done"));
        self.builder.build_unconditional_branch(outer_loop).expect("failed bigint mul outer jump");
        let outer_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(outer_loop);
        let i_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_outer_i"))
            .expect("failed bigint mul outer i phi");
        i_phi.add_incoming(&[(&self.i64_type.const_zero(), outer_entry_end)]);
        let i = i_phi.as_basic_value().into_int_value();
        let outer_more = self
            .builder
            .build_int_compare(IntPredicate::ULT, i, lhs_len, &format!("{label}_outer_more"))
            .expect("failed bigint mul outer compare");
        self.builder
            .build_conditional_branch(outer_more, outer_body, outer_done)
            .expect("failed bigint mul outer branch");

        self.builder.position_at_end(outer_body);
        let lhs_limb = self.build_bigint_limb_load(lhs, i, &format!("{label}_lhs_limb"));
        let inner_loop = self.context.append_basic_block(function, &format!("{label}_inner_loop"));
        let inner_body = self.context.append_basic_block(function, &format!("{label}_inner_body"));
        let inner_done = self.context.append_basic_block(function, &format!("{label}_inner_done"));
        self.builder.build_unconditional_branch(inner_loop).expect("failed bigint mul inner jump");
        let inner_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(inner_loop);
        let j_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_inner_j"))
            .expect("failed bigint mul inner j phi");
        let carry_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_inner_carry"))
            .expect("failed bigint mul inner carry phi");
        j_phi.add_incoming(&[(&self.i64_type.const_zero(), inner_entry_end)]);
        carry_phi.add_incoming(&[(&self.i64_type.const_zero(), inner_entry_end)]);
        let j = j_phi.as_basic_value().into_int_value();
        let carry = carry_phi.as_basic_value().into_int_value();
        let inner_more = self
            .builder
            .build_int_compare(IntPredicate::ULT, j, rhs_len, &format!("{label}_inner_more"))
            .expect("failed bigint mul inner compare");
        self.builder
            .build_conditional_branch(inner_more, inner_body, inner_done)
            .expect("failed bigint mul inner branch");

        self.builder.position_at_end(inner_body);
        let rhs_limb = self.build_bigint_limb_load(rhs, j, &format!("{label}_rhs_limb"));
        let idx = self
            .builder
            .build_int_add(i, j, &format!("{label}_idx"))
            .expect("failed bigint mul idx");
        let existing = self.build_bigint_limb_load(result, idx, &format!("{label}_existing"));
        let prod = self
            .builder
            .build_int_mul(lhs_limb, rhs_limb, &format!("{label}_prod"))
            .expect("failed bigint mul prod");
        let tmp = self
            .builder
            .build_int_add(existing, prod, &format!("{label}_tmp"))
            .expect("failed bigint mul tmp");
        let total = self
            .builder
            .build_int_add(tmp, carry, &format!("{label}_total"))
            .expect("failed bigint mul total");
        let low = self
            .builder
            .build_and(total, self.i64_type.const_int(0xffff_ffff, false), &format!("{label}_low"))
            .expect("failed bigint mul low");
        self.build_bigint_limb_store(result, idx, low, &format!("{label}_store"));
        let next_carry = self
            .builder
            .build_right_shift(
                total,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_next_carry"),
            )
            .expect("failed bigint mul carry shift");
        let next_j = self
            .builder
            .build_int_add(j, self.i64_type.const_int(1, false), &format!("{label}_next_j"))
            .expect("failed bigint mul next j");
        self.builder.build_unconditional_branch(inner_loop).expect("failed bigint mul inner loop");
        let inner_body_end = self.builder.get_insert_block().unwrap();
        j_phi.add_incoming(&[(&next_j, inner_body_end)]);
        carry_phi.add_incoming(&[(&next_carry, inner_body_end)]);

        self.builder.position_at_end(inner_done);
        let carry_loop = self.context.append_basic_block(function, &format!("{label}_carry_loop"));
        let carry_body = self.context.append_basic_block(function, &format!("{label}_carry_body"));
        let carry_done = self.context.append_basic_block(function, &format!("{label}_carry_done"));
        let carry_start_idx = self
            .builder
            .build_int_add(i, rhs_len, &format!("{label}_carry_start_idx"))
            .expect("failed bigint mul carry start idx");
        self.builder.build_unconditional_branch(carry_loop).expect("failed bigint mul carry jump");
        let carry_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(carry_loop);
        let carry_idx_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_carry_idx"))
            .expect("failed bigint mul carry idx phi");
        let carry_val_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_carry_val"))
            .expect("failed bigint mul carry val phi");
        carry_idx_phi.add_incoming(&[(&carry_start_idx, carry_entry_end)]);
        carry_val_phi
            .add_incoming(&[(&carry_phi.as_basic_value().into_int_value(), carry_entry_end)]);
        let carry_idx = carry_idx_phi.as_basic_value().into_int_value();
        let carry_val = carry_val_phi.as_basic_value().into_int_value();
        let carry_more = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                carry_val,
                self.i64_type.const_zero(),
                &format!("{label}_carry_more"),
            )
            .expect("failed bigint mul carry more");
        self.builder
            .build_conditional_branch(carry_more, carry_body, carry_done)
            .expect("failed bigint mul carry branch");

        self.builder.position_at_end(carry_body);
        let existing =
            self.build_bigint_limb_load(result, carry_idx, &format!("{label}_carry_existing"));
        let total = self
            .builder
            .build_int_add(existing, carry_val, &format!("{label}_carry_total"))
            .expect("failed bigint mul carry total");
        let low = self
            .builder
            .build_and(
                total,
                self.i64_type.const_int(0xffff_ffff, false),
                &format!("{label}_carry_low"),
            )
            .expect("failed bigint mul carry low");
        self.build_bigint_limb_store(result, carry_idx, low, &format!("{label}_carry_store"));
        let next_carry = self
            .builder
            .build_right_shift(
                total,
                self.i64_type.const_int(32, false),
                false,
                &format!("{label}_carry_next_carry"),
            )
            .expect("failed bigint mul carry shift");
        let next_idx = self
            .builder
            .build_int_add(
                carry_idx,
                self.i64_type.const_int(1, false),
                &format!("{label}_carry_next_idx"),
            )
            .expect("failed bigint mul carry next idx");
        self.builder.build_unconditional_branch(carry_loop).expect("failed bigint mul carry loop");
        let carry_body_end = self.builder.get_insert_block().unwrap();
        carry_idx_phi.add_incoming(&[(&next_idx, carry_body_end)]);
        carry_val_phi.add_incoming(&[(&next_carry, carry_body_end)]);

        self.builder.position_at_end(carry_done);
        let next_i = self
            .builder
            .build_int_add(i, self.i64_type.const_int(1, false), &format!("{label}_next_i"))
            .expect("failed bigint mul next i");
        self.builder.build_unconditional_branch(outer_loop).expect("failed bigint mul outer loop");
        let outer_body_end = self.builder.get_insert_block().unwrap();
        i_phi.add_incoming(&[(&next_i, outer_body_end)]);

        self.builder.position_at_end(outer_done);
        self.build_bigint_normalize(result, &format!("{label}_norm"));
        result
    }

    pub(super) fn build_bigint_signed_addsub(
        &self,
        lhs: IntValue<'ctx>,
        lhs_sign: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        rhs_sign: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let merge = self.context.append_basic_block(function, &format!("{label}_merge"));
        let lhs_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                self.i64_type.const_zero(),
                &format!("{label}_lhs_zero"),
            )
            .expect("failed bigint signed lhs_zero");
        let lhs_zero_block =
            self.context.append_basic_block(function, &format!("{label}_lhs_zero_block"));
        let rhs_zero_check =
            self.context.append_basic_block(function, &format!("{label}_rhs_zero_check"));
        self.builder
            .build_conditional_branch(lhs_zero, lhs_zero_block, rhs_zero_check)
            .expect("failed bigint signed lhs_zero branch");

        self.builder.position_at_end(lhs_zero_block);
        self.builder.build_unconditional_branch(merge).expect("failed lhs_zero merge");
        let lhs_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_zero_check);
        let rhs_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rhs_sign,
                self.i64_type.const_zero(),
                &format!("{label}_rhs_zero"),
            )
            .expect("failed bigint signed rhs_zero");
        let rhs_zero_block =
            self.context.append_basic_block(function, &format!("{label}_rhs_zero_block"));
        let same_sign_block =
            self.context.append_basic_block(function, &format!("{label}_same_sign"));
        self.builder
            .build_conditional_branch(rhs_zero, rhs_zero_block, same_sign_block)
            .expect("failed bigint signed rhs_zero branch");

        self.builder.position_at_end(rhs_zero_block);
        self.builder.build_unconditional_branch(merge).expect("failed rhs_zero merge");
        let rhs_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(same_sign_block);
        let signs_equal = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                rhs_sign,
                &format!("{label}_signs_equal"),
            )
            .expect("failed bigint signed signs_equal");
        let add_block = self.context.append_basic_block(function, &format!("{label}_add_block"));
        let diff_sign_block =
            self.context.append_basic_block(function, &format!("{label}_diff_sign"));
        self.builder
            .build_conditional_branch(signs_equal, add_block, diff_sign_block)
            .expect("failed bigint signed sign branch");

        self.builder.position_at_end(add_block);
        let sum_ptr = self.build_bigint_add_abs(lhs, rhs, &format!("{label}_add_abs"));
        self.build_bigint_sign_store(sum_ptr, lhs_sign, &format!("{label}_sum_sign"));
        self.build_bigint_normalize(sum_ptr, &format!("{label}_sum_norm"));
        self.builder.build_unconditional_branch(merge).expect("failed add merge");
        let add_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(diff_sign_block);
        let cmp = self.build_bigint_cmp_abs(lhs, rhs, &format!("{label}_cmp_abs"));
        let cmp_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                cmp,
                self.i64_type.const_zero(),
                &format!("{label}_cmp_zero"),
            )
            .expect("failed bigint signed cmp_zero");
        let equal_block = self.context.append_basic_block(function, &format!("{label}_equal"));
        let non_zero_block =
            self.context.append_basic_block(function, &format!("{label}_non_zero"));
        self.builder
            .build_conditional_branch(cmp_zero, equal_block, non_zero_block)
            .expect("failed bigint signed cmp branch");

        self.builder.position_at_end(equal_block);
        let zero_ptr =
            self.build_bigint_alloc(self.i64_type.const_zero(), &format!("{label}_zero_alloc"));
        self.build_bigint_sign_store(
            zero_ptr,
            self.i64_type.const_zero(),
            &format!("{label}_zero_sign"),
        );
        self.build_bigint_len_store(
            zero_ptr,
            self.i64_type.const_zero(),
            &format!("{label}_zero_len"),
        );
        self.builder.build_unconditional_branch(merge).expect("failed equal merge");
        let equal_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(non_zero_block);
        let lhs_gt = self
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                cmp,
                self.i64_type.const_zero(),
                &format!("{label}_lhs_gt"),
            )
            .expect("failed bigint signed lhs_gt");
        let lhs_gt_block =
            self.context.append_basic_block(function, &format!("{label}_lhs_gt_block"));
        let rhs_gt_block =
            self.context.append_basic_block(function, &format!("{label}_rhs_gt_block"));
        self.builder
            .build_conditional_branch(lhs_gt, lhs_gt_block, rhs_gt_block)
            .expect("failed bigint signed lhs_gt branch");

        self.builder.position_at_end(lhs_gt_block);
        let lhs_diff = self.build_bigint_sub_abs(lhs, rhs, &format!("{label}_lhs_diff"));
        self.build_bigint_sign_store(lhs_diff, lhs_sign, &format!("{label}_lhs_diff_sign"));
        self.build_bigint_normalize(lhs_diff, &format!("{label}_lhs_diff_norm"));
        self.builder.build_unconditional_branch(merge).expect("failed lhs_gt merge");
        let lhs_gt_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rhs_gt_block);
        let rhs_diff = self.build_bigint_sub_abs(rhs, lhs, &format!("{label}_rhs_diff"));
        self.build_bigint_sign_store(rhs_diff, rhs_sign, &format!("{label}_rhs_diff_sign"));
        self.build_bigint_normalize(rhs_diff, &format!("{label}_rhs_diff_norm"));
        self.builder.build_unconditional_branch(merge).expect("failed rhs_gt merge");
        let rhs_gt_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge);
        let result_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_result_phi"))
            .expect("failed bigint signed result phi");
        result_phi.add_incoming(&[
            (&rhs, lhs_zero_end),
            (&lhs, rhs_zero_end),
            (&sum_ptr, add_end),
            (&zero_ptr, equal_end),
            (&lhs_diff, lhs_gt_end),
            (&rhs_diff, rhs_gt_end),
        ]);
        result_phi.as_basic_value().into_int_value()
    }

    pub(super) fn build_bigint_signed_compare(
        &self,
        lhs: IntValue<'ctx>,
        lhs_sign: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
        rhs_sign: IntValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let merge = self.context.append_basic_block(function, &format!("{label}_merge"));
        let signs_equal_block =
            self.context.append_basic_block(function, &format!("{label}_signs_equal"));
        let signs_diff_block =
            self.context.append_basic_block(function, &format!("{label}_signs_diff"));
        let signs_equal = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                rhs_sign,
                &format!("{label}_signs_equal_cmp"),
            )
            .expect("failed bigint signed compare signs_equal");
        self.builder
            .build_conditional_branch(signs_equal, signs_equal_block, signs_diff_block)
            .expect("failed bigint signed compare branch");

        self.builder.position_at_end(signs_diff_block);
        let lhs_gt = self
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                lhs_sign,
                rhs_sign,
                &format!("{label}_lhs_sign_gt"),
            )
            .expect("failed bigint signed compare lhs_gt");
        let diff_cmp = self
            .builder
            .build_select(
                lhs_gt,
                self.i64_type.const_int(1, true),
                self.i64_type.const_int((-1i64) as u64, true),
                &format!("{label}_diff_cmp"),
            )
            .expect("failed bigint signed compare diff select")
            .into_int_value();
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed bigint signed compare diff merge");
        let signs_diff_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(signs_equal_block);
        let sign_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                self.i64_type.const_zero(),
                &format!("{label}_sign_zero"),
            )
            .expect("failed bigint signed compare sign_zero");
        let zero_block = self.context.append_basic_block(function, &format!("{label}_zero"));
        let cmp_block = self.context.append_basic_block(function, &format!("{label}_cmp"));
        self.builder
            .build_conditional_branch(sign_zero, zero_block, cmp_block)
            .expect("failed bigint signed compare zero branch");

        self.builder.position_at_end(zero_block);
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed bigint signed compare zero merge");
        let zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(cmp_block);
        let cmp = self.build_bigint_cmp_abs(lhs, rhs, &format!("{label}_cmp_abs"));
        let sign_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                lhs_sign,
                self.i64_type.const_zero(),
                &format!("{label}_sign_negative"),
            )
            .expect("failed bigint signed compare sign_negative");
        let neg_block = self.context.append_basic_block(function, &format!("{label}_neg"));
        let pos_block = self.context.append_basic_block(function, &format!("{label}_pos"));
        self.builder
            .build_conditional_branch(sign_negative, neg_block, pos_block)
            .expect("failed bigint signed compare neg branch");

        self.builder.position_at_end(pos_block);
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed bigint signed compare pos merge");
        let pos_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(neg_block);
        let neg_cmp = self
            .builder
            .build_int_neg(cmp, &format!("{label}_neg_cmp"))
            .expect("failed bigint signed compare neg cmp");
        self.builder
            .build_unconditional_branch(merge)
            .expect("failed bigint signed compare neg merge");
        let neg_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge);
        let result_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_result_phi"))
            .expect("failed bigint signed compare result phi");
        result_phi.add_incoming(&[
            (&diff_cmp, signs_diff_end),
            (&self.i64_type.const_zero(), zero_end),
            (&cmp, pos_end),
            (&neg_cmp, neg_end),
        ]);
        result_phi.as_basic_value().into_int_value()
    }

    pub(super) fn bigint_header_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context.struct_type(
            &[
                self.i64_type.into(),
                self.i64_type.into(),
                self.i64_type.into(),
                self.context.ptr_type(Default::default()).into(),
            ],
            false,
        )
    }

    pub(super) fn define_pair_bigint_from_int(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);
        self.functions.insert("bigint_from_int".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let value = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let trap_block = self.context.append_basic_block(function, "trap");
        let raw = self.expect_tag_int(value, "bigint_from_int", trap_block);
        let zero_block = self.context.append_basic_block(function, "zero");
        let non_zero_block = self.context.append_basic_block(function, "non_zero");
        let merge_block = self.context.append_basic_block(function, "merge");
        let is_zero = self
            .builder
            .build_int_compare(IntPredicate::EQ, raw, self.i64_type.const_zero(), "is_zero")
            .expect("failed bigint_from_int zero compare");
        self.builder
            .build_conditional_branch(is_zero, zero_block, non_zero_block)
            .expect("failed bigint_from_int zero branch");

        self.builder.position_at_end(zero_block);
        let zero_ptr = self.build_bigint_alloc(self.i64_type.const_zero(), "bigint_zero");
        self.builder
            .build_unconditional_branch(merge_block)
            .expect("failed bigint_from_int zero merge");
        let zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(non_zero_block);
        let is_negative = self
            .builder
            .build_int_compare(IntPredicate::SLT, raw, self.i64_type.const_zero(), "is_negative")
            .expect("failed bigint_from_int neg compare");
        let neg_block = self.context.append_basic_block(function, "neg");
        let pos_block = self.context.append_basic_block(function, "pos");
        let sign_merge = self.context.append_basic_block(function, "sign_merge");
        self.builder
            .build_conditional_branch(is_negative, neg_block, pos_block)
            .expect("failed bigint_from_int sign branch");

        self.builder.position_at_end(neg_block);
        let neg_abs =
            self.builder.build_int_neg(raw, "neg_abs").expect("failed bigint_from_int neg abs");
        self.builder
            .build_unconditional_branch(sign_merge)
            .expect("failed bigint_from_int neg merge");
        let neg_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(pos_block);
        self.builder
            .build_unconditional_branch(sign_merge)
            .expect("failed bigint_from_int pos merge");
        let pos_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(sign_merge);
        let sign_phi = self
            .builder
            .build_phi(self.i64_type, "sign_phi")
            .expect("failed bigint_from_int sign phi");
        let abs_phi = self
            .builder
            .build_phi(self.i64_type, "abs_phi")
            .expect("failed bigint_from_int abs phi");
        sign_phi.add_incoming(&[
            (&self.i64_type.const_int((-1i64) as u64, true), neg_end),
            (&self.i64_type.const_int(1, true), pos_end),
        ]);
        abs_phi.add_incoming(&[(&neg_abs, neg_end), (&raw, pos_end)]);
        let sign = sign_phi.as_basic_value().into_int_value();
        let abs = abs_phi.as_basic_value().into_int_value();
        let high = self
            .builder
            .build_right_shift(abs, self.i64_type.const_int(32, false), false, "high")
            .expect("failed bigint_from_int high");
        let has_high = self
            .builder
            .build_int_compare(IntPredicate::NE, high, self.i64_type.const_zero(), "has_high")
            .expect("failed bigint_from_int has_high");
        let cap_merge = self.context.append_basic_block(function, "cap_merge");
        let high_block = self.context.append_basic_block(function, "high_block");
        let low_block = self.context.append_basic_block(function, "low_block");
        self.builder
            .build_conditional_branch(has_high, high_block, low_block)
            .expect("failed bigint_from_int cap branch");

        self.builder.position_at_end(high_block);
        self.builder
            .build_unconditional_branch(cap_merge)
            .expect("failed bigint_from_int high merge");
        let high_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(low_block);
        self.builder
            .build_unconditional_branch(cap_merge)
            .expect("failed bigint_from_int low merge");
        let low_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(cap_merge);
        let cap_phi = self
            .builder
            .build_phi(self.i64_type, "cap_phi")
            .expect("failed bigint_from_int cap phi");
        cap_phi.add_incoming(&[
            (&self.i64_type.const_int(2, false), high_end),
            (&self.i64_type.const_int(1, false), low_end),
        ]);
        let cap = cap_phi.as_basic_value().into_int_value();
        let ptr = self.build_bigint_alloc(cap, "bigint_from_int_alloc");
        self.build_bigint_sign_store(ptr, sign, "bigint_from_int_sign");
        self.build_bigint_len_store(ptr, cap, "bigint_from_int_len");
        let low = self
            .builder
            .build_and(abs, self.i64_type.const_int(0xffff_ffff, false), "low")
            .expect("failed bigint_from_int low");
        self.build_bigint_limb_store(ptr, self.i64_type.const_zero(), low, "bigint_from_int_low");
        let has_second = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                cap,
                self.i64_type.const_int(2, false),
                "has_second",
            )
            .expect("failed bigint_from_int has_second");
        let second_block = self.context.append_basic_block(function, "second");
        let second_done = self.context.append_basic_block(function, "second_done");
        self.builder
            .build_conditional_branch(has_second, second_block, second_done)
            .expect("failed bigint_from_int second branch");

        self.builder.position_at_end(second_block);
        self.build_bigint_limb_store(
            ptr,
            self.i64_type.const_int(1, false),
            high,
            "bigint_from_int_high",
        );
        self.builder
            .build_unconditional_branch(second_done)
            .expect("failed bigint_from_int second done");

        self.builder.position_at_end(second_done);
        self.build_bigint_normalize(ptr, "bigint_from_int_norm");
        self.builder.build_unconditional_branch(merge_block).expect("failed bigint_from_int merge");
        let non_zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_block);
        let result_phi = self
            .builder
            .build_phi(self.i64_type, "result_phi")
            .expect("failed bigint_from_int result phi");
        result_phi.add_incoming(&[(&zero_ptr, zero_end), (&ptr, non_zero_end)]);
        let result_ptr = result_phi.as_basic_value().into_int_value();
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                result_ptr,
                "bigint_from_int_result",
            )))
            .expect("failed bigint_from_int return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_bigint_add(&mut self, name: &str, symbol: &str) {
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
        self.functions.insert("bigint_add".to_string(), function);
        let entry = self.context.append_basic_block(function, "entry");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);
        let lhs = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let rhs = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_add_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);
        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_add_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);
        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_add_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_add_rhs");
        let result_ptr =
            self.build_bigint_signed_addsub(lhs_ptr, lhs_sign, rhs_ptr, rhs_sign, "bigint_add");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                result_ptr,
                "bigint_add_result",
            )))
            .expect("failed bigint_add return");
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_bigint_compare(&mut self, name: &str, symbol: &str) {
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
        self.functions.insert("bigint_compare".to_string(), function);
        let entry = self.context.append_basic_block(function, "entry");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);
        let lhs = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let rhs = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_cmp_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);
        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_cmp_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);
        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_cmp_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_cmp_rhs");
        let raw =
            self.build_bigint_signed_compare(lhs_ptr, lhs_sign, rhs_ptr, rhs_sign, "bigint_cmp");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                raw,
                "bigint_cmp_result",
            )))
            .expect("failed bigint_compare return");
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_bigint_subtract(&mut self, name: &str, symbol: &str) {
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
        self.functions.insert("bigint_subtract".to_string(), function);
        let entry = self.context.append_basic_block(function, "entry");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);
        let lhs = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let rhs = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_sub_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);
        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_sub_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);
        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_sub_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_sub_rhs");
        let neg_rhs_sign = self
            .builder
            .build_int_sub(self.i64_type.const_zero(), rhs_sign, "neg_rhs_sign")
            .expect("failed bigint_sub neg rhs sign");
        let result_ptr =
            self.build_bigint_signed_addsub(lhs_ptr, lhs_sign, rhs_ptr, neg_rhs_sign, "bigint_sub");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                result_ptr,
                "bigint_sub_result",
            )))
            .expect("failed bigint_sub return");
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_bigint_multiply(&mut self, name: &str, symbol: &str) {
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
        self.functions.insert("bigint_multiply".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let lhs = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let rhs = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };

        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_mul_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);

        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_mul_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);

        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_mul_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_mul_rhs");

        let lhs_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                self.i64_type.const_zero(),
                "bigint_mul_lhs_zero",
            )
            .expect("failed bigint_mul lhs zero compare");
        let rhs_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rhs_sign,
                self.i64_type.const_zero(),
                "bigint_mul_rhs_zero",
            )
            .expect("failed bigint_mul rhs zero compare");
        let is_zero = self
            .builder
            .build_or(lhs_zero, rhs_zero, "bigint_mul_is_zero")
            .expect("failed bigint_mul zero or");

        let zero_block = self.context.append_basic_block(function, "zero");
        let mul_block = self.context.append_basic_block(function, "mul");
        let merge_block = self.context.append_basic_block(function, "merge");
        self.builder
            .build_conditional_branch(is_zero, zero_block, mul_block)
            .expect("failed bigint_mul zero branch");

        self.builder.position_at_end(zero_block);
        let zero_ptr = self.build_bigint_alloc(self.i64_type.const_zero(), "bigint_mul_zero");
        self.build_bigint_sign_store(zero_ptr, self.i64_type.const_zero(), "bigint_mul_zero");
        self.build_bigint_len_store(zero_ptr, self.i64_type.const_zero(), "bigint_mul_zero");
        self.builder.build_unconditional_branch(merge_block).expect("failed bigint_mul zero merge");
        let zero_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(mul_block);
        let abs_ptr = self.build_bigint_mul_abs(lhs_ptr, rhs_ptr, "bigint_mul_abs");
        let same_sign = self
            .builder
            .build_int_compare(IntPredicate::EQ, lhs_sign, rhs_sign, "bigint_mul_same_sign")
            .expect("failed bigint_mul same sign compare");
        let result_sign = self
            .builder
            .build_select(
                same_sign,
                self.i64_type.const_int(1, true),
                self.i64_type.const_int((-1i64) as u64, true),
                "bigint_mul_result_sign",
            )
            .expect("failed bigint_mul sign select")
            .into_int_value();
        self.build_bigint_sign_store(abs_ptr, result_sign, "bigint_mul_sign");
        self.build_bigint_normalize(abs_ptr, "bigint_mul_norm");
        self.builder.build_unconditional_branch(merge_block).expect("failed bigint_mul mul merge");
        let mul_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_block);
        let result_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_mul_result_ptr")
            .expect("failed bigint_mul result phi");
        result_phi.add_incoming(&[(&zero_ptr, zero_end), (&abs_ptr, mul_end)]);
        let result_ptr = result_phi.as_basic_value().into_int_value();
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                result_ptr,
                "bigint_mul_result",
            )))
            .expect("failed bigint_mul return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_bigint_divide(&mut self, name: &str, symbol: &str) {
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
        self.functions.insert("bigint_divide".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let lhs = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let rhs = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };

        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_div_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);

        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_div_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);

        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_div_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_div_rhs");

        let rhs_is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rhs_sign,
                self.i64_type.const_zero(),
                "bigint_div_rhs_zero",
            )
            .expect("failed bigint_div rhs zero compare");
        let lhs_is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                self.i64_type.const_zero(),
                "bigint_div_lhs_zero",
            )
            .expect("failed bigint_div lhs zero compare");

        let zero_block = self.context.append_basic_block(function, "zero");
        let work_block = self.context.append_basic_block(function, "work");
        let init_block = self.context.append_basic_block(function, "init");
        self.builder
            .build_conditional_branch(rhs_is_zero, trap_block, work_block)
            .expect("failed bigint_div rhs zero branch");

        self.builder.position_at_end(work_block);
        let outer_loop = self.context.append_basic_block(function, "outer_loop");
        let outer_body = self.context.append_basic_block(function, "outer_body");
        let outer_done = self.context.append_basic_block(function, "outer_done");
        self.builder
            .build_conditional_branch(lhs_is_zero, zero_block, init_block)
            .expect("failed bigint_div lhs zero branch");

        self.builder.position_at_end(zero_block);
        let zero_ptr = self.build_bigint_zero("bigint_div_zero");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                zero_ptr,
                "bigint_div_zero_result",
            )))
            .expect("failed bigint_div zero return");

        self.builder.position_at_end(init_block);
        let quotient0 = self.build_bigint_zero("bigint_div_q0");
        self.builder.build_unconditional_branch(outer_loop).expect("failed bigint_div init jump");
        let init_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(outer_loop);
        let quotient_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_div_quotient")
            .expect("failed bigint_div quotient phi");
        let remainder_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_div_remainder")
            .expect("failed bigint_div remainder phi");
        quotient_phi.add_incoming(&[(&quotient0, init_end)]);
        remainder_phi.add_incoming(&[(&lhs_ptr, init_end)]);
        let quotient = quotient_phi.as_basic_value().into_int_value();
        let remainder = remainder_phi.as_basic_value().into_int_value();
        let cmp = self.build_bigint_cmp_abs(remainder, rhs_ptr, "bigint_div_outer_cmp");
        let has_more = self
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                cmp,
                self.i64_type.const_zero(),
                "bigint_div_has_more",
            )
            .expect("failed bigint_div outer cmp check");
        self.builder
            .build_conditional_branch(has_more, outer_body, outer_done)
            .expect("failed bigint_div outer branch");

        self.builder.position_at_end(outer_body);
        let inner_loop = self.context.append_basic_block(function, "inner_loop");
        let inner_body = self.context.append_basic_block(function, "inner_body");
        let inner_done = self.context.append_basic_block(function, "inner_done");
        let multiple0 = self.build_bigint_one("bigint_div_m1");
        self.builder.build_unconditional_branch(inner_loop).expect("failed bigint_div inner jump");
        let inner_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(inner_loop);
        let current_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_div_current")
            .expect("failed bigint_div current phi");
        let multiple_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_div_multiple")
            .expect("failed bigint_div multiple phi");
        current_phi.add_incoming(&[(&rhs_ptr, inner_entry_end)]);
        multiple_phi.add_incoming(&[(&multiple0, inner_entry_end)]);
        let current = current_phi.as_basic_value().into_int_value();
        let multiple = multiple_phi.as_basic_value().into_int_value();
        let doubled = self.build_bigint_add_abs(current, current, "bigint_div_doubled");
        let doubled_cmp = self.build_bigint_cmp_abs(doubled, remainder, "bigint_div_doubled_cmp");
        let can_double = self
            .builder
            .build_int_compare(
                IntPredicate::SLE,
                doubled_cmp,
                self.i64_type.const_zero(),
                "bigint_div_can_double",
            )
            .expect("failed bigint_div can_double");
        self.builder
            .build_conditional_branch(can_double, inner_body, inner_done)
            .expect("failed bigint_div inner branch");

        self.builder.position_at_end(inner_body);
        let doubled_multiple =
            self.build_bigint_add_abs(multiple, multiple, "bigint_div_doubled_multiple");
        self.builder.build_unconditional_branch(inner_loop).expect("failed bigint_div inner loop");
        let inner_body_end = self.builder.get_insert_block().unwrap();
        current_phi.add_incoming(&[(&doubled, inner_body_end)]);
        multiple_phi.add_incoming(&[(&doubled_multiple, inner_body_end)]);

        self.builder.position_at_end(inner_done);
        let best_current = current_phi.as_basic_value().into_int_value();
        let best_multiple = multiple_phi.as_basic_value().into_int_value();
        let next_remainder =
            self.build_bigint_sub_abs(remainder, best_current, "bigint_div_next_remainder");
        let next_quotient =
            self.build_bigint_add_abs(quotient, best_multiple, "bigint_div_next_quotient");
        self.builder
            .build_unconditional_branch(outer_loop)
            .expect("failed bigint_div outer continue");
        let inner_done_end = self.builder.get_insert_block().unwrap();
        quotient_phi.add_incoming(&[(&next_quotient, inner_done_end)]);
        remainder_phi.add_incoming(&[(&next_remainder, inner_done_end)]);

        self.builder.position_at_end(outer_done);
        let raw_quotient = quotient_phi.as_basic_value().into_int_value();
        let same_sign = self
            .builder
            .build_int_compare(IntPredicate::EQ, lhs_sign, rhs_sign, "bigint_div_same_sign")
            .expect("failed bigint_div same sign compare");
        let out_sign = self
            .builder
            .build_select(
                same_sign,
                self.i64_type.const_int(1, true),
                self.i64_type.const_int((-1i64) as u64, true),
                "bigint_div_out_sign",
            )
            .expect("failed bigint_div sign select")
            .into_int_value();
        self.build_bigint_sign_store(raw_quotient, out_sign, "bigint_div_sign");
        self.build_bigint_normalize(raw_quotient, "bigint_div_norm");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                raw_quotient,
                "bigint_div_result",
            )))
            .expect("failed bigint_div return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_bigint_modulo(&mut self, name: &str, symbol: &str) {
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
        self.functions.insert("bigint_modulo".to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let lhs = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let rhs = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };

        let lhs_ok = self.context.append_basic_block(function, "lhs_ok");
        let lhs_ptr =
            self.expect_tag_payload(lhs, TAG_BIGINT, "bigint_mod_lhs", lhs_ok, trap_block);
        self.builder.position_at_end(lhs_ok);

        let rhs_ok = self.context.append_basic_block(function, "rhs_ok");
        let rhs_ptr =
            self.expect_tag_payload(rhs, TAG_BIGINT, "bigint_mod_rhs", rhs_ok, trap_block);
        self.builder.position_at_end(rhs_ok);

        let lhs_sign = self.build_bigint_sign_load(lhs_ptr, "bigint_mod_lhs");
        let rhs_sign = self.build_bigint_sign_load(rhs_ptr, "bigint_mod_rhs");

        let rhs_is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                rhs_sign,
                self.i64_type.const_zero(),
                "bigint_mod_rhs_zero",
            )
            .expect("failed bigint_mod rhs zero compare");
        let lhs_is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                lhs_sign,
                self.i64_type.const_zero(),
                "bigint_mod_lhs_zero",
            )
            .expect("failed bigint_mod lhs zero compare");

        let zero_block = self.context.append_basic_block(function, "zero");
        let work_block = self.context.append_basic_block(function, "work");
        let init_block = self.context.append_basic_block(function, "init");
        self.builder
            .build_conditional_branch(rhs_is_zero, trap_block, work_block)
            .expect("failed bigint_mod rhs zero branch");

        self.builder.position_at_end(work_block);
        let outer_loop = self.context.append_basic_block(function, "outer_loop");
        let outer_body = self.context.append_basic_block(function, "outer_body");
        let outer_done = self.context.append_basic_block(function, "outer_done");
        self.builder
            .build_conditional_branch(lhs_is_zero, zero_block, init_block)
            .expect("failed bigint_mod lhs zero branch");

        self.builder.position_at_end(zero_block);
        let zero_ptr = self.build_bigint_zero("bigint_mod_zero");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                zero_ptr,
                "bigint_mod_zero_result",
            )))
            .expect("failed bigint_mod zero return");

        self.builder.position_at_end(init_block);
        self.builder.build_unconditional_branch(outer_loop).expect("failed bigint_mod init jump");
        let init_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(outer_loop);
        let remainder_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_mod_remainder")
            .expect("failed bigint_mod remainder phi");
        remainder_phi.add_incoming(&[(&lhs_ptr, init_end)]);
        let remainder = remainder_phi.as_basic_value().into_int_value();
        let cmp = self.build_bigint_cmp_abs(remainder, rhs_ptr, "bigint_mod_outer_cmp");
        let has_more = self
            .builder
            .build_int_compare(
                IntPredicate::SGE,
                cmp,
                self.i64_type.const_zero(),
                "bigint_mod_has_more",
            )
            .expect("failed bigint_mod outer cmp check");
        self.builder
            .build_conditional_branch(has_more, outer_body, outer_done)
            .expect("failed bigint_mod outer branch");

        self.builder.position_at_end(outer_body);
        let inner_loop = self.context.append_basic_block(function, "inner_loop");
        let inner_body = self.context.append_basic_block(function, "inner_body");
        let inner_done = self.context.append_basic_block(function, "inner_done");
        self.builder.build_unconditional_branch(inner_loop).expect("failed bigint_mod inner jump");
        let inner_entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(inner_loop);
        let current_phi = self
            .builder
            .build_phi(self.i64_type, "bigint_mod_current")
            .expect("failed bigint_mod current phi");
        current_phi.add_incoming(&[(&rhs_ptr, inner_entry_end)]);
        let current = current_phi.as_basic_value().into_int_value();
        let doubled = self.build_bigint_add_abs(current, current, "bigint_mod_doubled");
        let doubled_cmp = self.build_bigint_cmp_abs(doubled, remainder, "bigint_mod_doubled_cmp");
        let can_double = self
            .builder
            .build_int_compare(
                IntPredicate::SLE,
                doubled_cmp,
                self.i64_type.const_zero(),
                "bigint_mod_can_double",
            )
            .expect("failed bigint_mod can_double");
        self.builder
            .build_conditional_branch(can_double, inner_body, inner_done)
            .expect("failed bigint_mod inner branch");

        self.builder.position_at_end(inner_body);
        self.builder.build_unconditional_branch(inner_loop).expect("failed bigint_mod inner loop");
        let inner_body_end = self.builder.get_insert_block().unwrap();
        current_phi.add_incoming(&[(&doubled, inner_body_end)]);

        self.builder.position_at_end(inner_done);
        let best_current = current_phi.as_basic_value().into_int_value();
        let next_remainder =
            self.build_bigint_sub_abs(remainder, best_current, "bigint_mod_next_remainder");
        self.builder
            .build_unconditional_branch(outer_loop)
            .expect("failed bigint_mod outer continue");
        let inner_done_end = self.builder.get_insert_block().unwrap();
        remainder_phi.add_incoming(&[(&next_remainder, inner_done_end)]);

        self.builder.position_at_end(outer_done);
        let raw_remainder = remainder_phi.as_basic_value().into_int_value();
        self.build_bigint_sign_store(raw_remainder, lhs_sign, "bigint_mod_sign");
        self.build_bigint_normalize(raw_remainder, "bigint_mod_norm");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_BIGINT as u64, false),
                raw_remainder,
                "bigint_mod_result",
            )))
            .expect("failed bigint_mod return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }
}
