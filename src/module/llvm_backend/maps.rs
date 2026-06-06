use super::{CompiledValue, LlvmCompiler};
use crate::value::{
    MAP_CAP_OFFSET, MAP_ENTRY_HASH_OFFSET, MAP_ENTRY_KEY_OFFSET, MAP_ENTRY_OCCUPIED,
    MAP_ENTRY_SIZE, MAP_ENTRY_STATE_OFFSET, MAP_ENTRY_TOMBSTONE, MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
    MAP_ENTRY_VALUE_TAG_OFFSET, MAP_HEADER_SIZE, MAP_LEN_OFFSET, MAP_PTR_OFFSET, TAG_INT, TAG_MAP,
    TAG_STRING,
};
use inkwell::IntPredicate;
use inkwell::module::Linkage;

impl<'ctx> LlvmCompiler<'ctx> {
    pub(super) fn build_map_len_load(
        &self,
        map_payload: inkwell::values::IntValue<'ctx>,
        label: &str,
    ) -> inkwell::values::IntValue<'ctx> {
        self.builder
            .build_load(
                self.i64_type,
                self.builder
                    .build_int_to_ptr(
                        self.builder
                            .build_int_add(
                                map_payload,
                                self.i64_type.const_int(MAP_LEN_OFFSET as u64, false),
                                &format!("{label}_map_len_addr"),
                            )
                            .expect("failed map len addr"),
                        self.context.ptr_type(Default::default()),
                        &format!("{label}_map_len_ptr"),
                    )
                    .expect("failed map len ptr"),
                &format!("{label}_map_len"),
            )
            .expect("failed map len load")
            .into_int_value()
    }

    fn build_map_cap_load(
        &self,
        map_payload: inkwell::values::IntValue<'ctx>,
        label: &str,
    ) -> inkwell::values::IntValue<'ctx> {
        self.builder
            .build_load(
                self.i64_type,
                self.builder
                    .build_int_to_ptr(
                        self.builder
                            .build_int_add(
                                map_payload,
                                self.i64_type.const_int(MAP_CAP_OFFSET as u64, false),
                                &format!("{label}_map_cap_addr"),
                            )
                            .expect("failed map cap addr"),
                        self.context.ptr_type(Default::default()),
                        &format!("{label}_map_cap_ptr"),
                    )
                    .expect("failed map cap ptr"),
                &format!("{label}_map_cap"),
            )
            .expect("failed map cap load")
            .into_int_value()
    }

    fn build_map_ptr_load(
        &self,
        map_payload: inkwell::values::IntValue<'ctx>,
        label: &str,
    ) -> inkwell::values::IntValue<'ctx> {
        self.builder
            .build_load(
                self.i64_type,
                self.builder
                    .build_int_to_ptr(
                        self.builder
                            .build_int_add(
                                map_payload,
                                self.i64_type.const_int(MAP_PTR_OFFSET as u64, false),
                                &format!("{label}_map_ptr_addr"),
                            )
                            .expect("failed map ptr addr"),
                        self.context.ptr_type(Default::default()),
                        &format!("{label}_map_ptr_ptr"),
                    )
                    .expect("failed map ptr ptr"),
                &format!("{label}_map_ptr"),
            )
            .expect("failed map ptr load")
            .into_int_value()
    }

    fn build_map_len_store(
        &self,
        map_payload: inkwell::values::IntValue<'ctx>,
        value: inkwell::values::IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self
            .builder
            .build_int_to_ptr(
                self.builder
                    .build_int_add(
                        map_payload,
                        self.i64_type.const_int(MAP_LEN_OFFSET as u64, false),
                        &format!("{label}_map_len_store_addr"),
                    )
                    .expect("failed map len store addr"),
                self.context.ptr_type(Default::default()),
                &format!("{label}_map_len_store_ptr"),
            )
            .expect("failed map len store ptr");
        self.builder.build_store(ptr, value).expect("failed map len store");
    }

    fn build_map_cap_store(
        &self,
        map_payload: inkwell::values::IntValue<'ctx>,
        value: inkwell::values::IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self
            .builder
            .build_int_to_ptr(
                self.builder
                    .build_int_add(
                        map_payload,
                        self.i64_type.const_int(MAP_CAP_OFFSET as u64, false),
                        &format!("{label}_map_cap_store_addr"),
                    )
                    .expect("failed map cap store addr"),
                self.context.ptr_type(Default::default()),
                &format!("{label}_map_cap_store_ptr"),
            )
            .expect("failed map cap store ptr");
        self.builder.build_store(ptr, value).expect("failed map cap store");
    }

    fn build_map_ptr_store(
        &self,
        map_payload: inkwell::values::IntValue<'ctx>,
        value: inkwell::values::IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self
            .builder
            .build_int_to_ptr(
                self.builder
                    .build_int_add(
                        map_payload,
                        self.i64_type.const_int(MAP_PTR_OFFSET as u64, false),
                        &format!("{label}_map_ptr_store_addr"),
                    )
                    .expect("failed map ptr store addr"),
                self.context.ptr_type(Default::default()),
                &format!("{label}_map_ptr_store_ptr"),
            )
            .expect("failed map ptr store ptr");
        self.builder.build_store(ptr, value).expect("failed map ptr store");
    }

    fn build_map_entry_ptr(
        &self,
        entries_ptr: inkwell::values::IntValue<'ctx>,
        idx: inkwell::values::IntValue<'ctx>,
        label: &str,
    ) -> inkwell::values::IntValue<'ctx> {
        let off = self
            .builder
            .build_int_mul(
                idx,
                self.i64_type.const_int(MAP_ENTRY_SIZE as u64, false),
                &format!("{label}_entry_off"),
            )
            .expect("failed map entry off");
        self.builder
            .build_int_add(entries_ptr, off, &format!("{label}_entry_ptr"))
            .expect("failed map entry ptr")
    }

    fn build_map_entry_field_load(
        &self,
        entry_ptr: inkwell::values::IntValue<'ctx>,
        offset: i32,
        label: &str,
    ) -> inkwell::values::IntValue<'ctx> {
        self.builder
            .build_load(
                self.i64_type,
                self.builder
                    .build_int_to_ptr(
                        self.builder
                            .build_int_add(
                                entry_ptr,
                                self.i64_type.const_int(offset as u64, false),
                                &format!("{label}_field_addr"),
                            )
                            .expect("failed map field addr"),
                        self.context.ptr_type(Default::default()),
                        &format!("{label}_field_ptr"),
                    )
                    .expect("failed map field ptr"),
                &format!("{label}_field"),
            )
            .expect("failed map field load")
            .into_int_value()
    }

    fn build_map_entry_field_store(
        &self,
        entry_ptr: inkwell::values::IntValue<'ctx>,
        offset: i32,
        value: inkwell::values::IntValue<'ctx>,
        label: &str,
    ) {
        let ptr = self
            .builder
            .build_int_to_ptr(
                self.builder
                    .build_int_add(
                        entry_ptr,
                        self.i64_type.const_int(offset as u64, false),
                        &format!("{label}_store_addr"),
                    )
                    .expect("failed map field store addr"),
                self.context.ptr_type(Default::default()),
                &format!("{label}_store_ptr"),
            )
            .expect("failed map field store ptr");
        self.builder.build_store(ptr, value).expect("failed map field store");
    }

    fn build_map_next_index(
        &self,
        idx: inkwell::values::IntValue<'ctx>,
        cap: inkwell::values::IntValue<'ctx>,
        label: &str,
    ) -> inkwell::values::IntValue<'ctx> {
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), &format!("{label}_next"))
            .expect("failed map next index");
        let in_bounds = self
            .builder
            .build_int_compare(IntPredicate::ULT, next, cap, &format!("{label}_in_bounds"))
            .expect("failed map next bounds");
        self.builder
            .build_select(in_bounds, next, self.i64_type.const_zero(), &format!("{label}_wrapped"))
            .expect("failed map next select")
            .into_int_value()
    }

    fn build_string_hash_bytes(
        &self,
        header_ptr: inkwell::values::IntValue<'ctx>,
        label: &str,
    ) -> inkwell::values::IntValue<'ctx> {
        let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let len = self.build_string_len_load(header_ptr, &format!("{label}_len"));
        let data_ptr = self.build_string_ptr_load(header_ptr, &format!("{label}_data"));
        let data_base = self
            .builder
            .build_ptr_to_int(data_ptr, self.i64_type, &format!("{label}_base"))
            .expect("failed string hash base");
        let loop_block = self.context.append_basic_block(function, &format!("{label}_loop"));
        let body_block = self.context.append_basic_block(function, &format!("{label}_body"));
        let done_block = self.context.append_basic_block(function, &format!("{label}_done"));
        let offset_basis = self.i64_type.const_int(0xcbf29ce484222325u64, false);
        let prime = self.i64_type.const_int(0x100000001b3u64, false);
        self.builder.build_unconditional_branch(loop_block).expect("failed string hash loop jump");
        let entry_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_idx"))
            .expect("failed string hash idx phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), entry_end)]);
        let hash_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_hash"))
            .expect("failed string hash phi");
        hash_phi.add_incoming(&[(&offset_basis, entry_end)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let hash = hash_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, &format!("{label}_more"))
            .expect("failed string hash more");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed string hash branch");
        let loop_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(body_block);
        let byte_addr = self
            .builder
            .build_int_add(data_base, idx, &format!("{label}_byte_addr"))
            .expect("failed string hash addr");
        let byte_ptr = self
            .builder
            .build_int_to_ptr(
                byte_addr,
                self.context.ptr_type(Default::default()),
                &format!("{label}_byte_ptr"),
            )
            .expect("failed string hash ptr");
        let byte = self
            .builder
            .build_load(self.context.i8_type(), byte_ptr, &format!("{label}_byte"))
            .expect("failed string hash byte")
            .into_int_value();
        let byte64 = self
            .builder
            .build_int_z_extend(byte, self.i64_type, &format!("{label}_byte64"))
            .expect("failed string hash extend");
        let xored = self
            .builder
            .build_xor(hash, byte64, &format!("{label}_xored"))
            .expect("failed string hash xor");
        let next_hash = self
            .builder
            .build_int_mul(xored, prime, &format!("{label}_next_hash"))
            .expect("failed string hash mul");
        let next_idx = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), &format!("{label}_next_idx"))
            .expect("failed string hash next idx");
        self.builder.build_unconditional_branch(loop_block).expect("failed string hash loop");
        let body_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next_idx, body_end)]);
        hash_phi.add_incoming(&[(&next_hash, body_end)]);

        self.builder.position_at_end(done_block);
        let result_phi = self
            .builder
            .build_phi(self.i64_type, &format!("{label}_result"))
            .expect("failed string hash result phi");
        result_phi.add_incoming(&[(&hash, loop_end)]);
        result_phi.as_basic_value().into_int_value()
    }

    pub(super) fn define_pair_map_new(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(&[], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);
        let entry = self.context.append_basic_block(function, "entry");
        let loop_block = self.context.append_basic_block(function, "loop");
        let body_block = self.context.append_basic_block(function, "body");
        let done_block = self.context.append_basic_block(function, "done");
        self.builder.position_at_end(entry);

        let align = self.i64_type.const_int(8, false);
        let cap = self.i64_type.const_int(64, false);
        let bytes = self
            .builder
            .build_int_mul(
                cap,
                self.i64_type.const_int(MAP_ENTRY_SIZE as u64, false),
                "map_new_bytes",
            )
            .expect("failed map bytes");
        let alloc = self.require_func("__alloc");
        let entries_ptr = self.build_boxed_call(alloc, &[bytes, align], "map_new_entries");
        let header_size = self.i64_type.const_int(MAP_HEADER_SIZE as u64, false);
        let header_ptr = self.build_boxed_call(alloc, &[header_size, align], "map_new_header");
        self.build_map_len_store(header_ptr, self.i64_type.const_zero(), "map_new");
        self.build_map_entry_field_store(header_ptr, MAP_CAP_OFFSET, cap, "map_new_cap");
        self.build_map_entry_field_store(header_ptr, MAP_PTR_OFFSET, entries_ptr, "map_new_ptr");
        self.builder.build_unconditional_branch(loop_block).expect("failed map new loop branch");

        self.builder.position_at_end(loop_block);
        let idx_phi =
            self.builder.build_phi(self.i64_type, "map_new_idx").expect("failed map new phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), entry)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, cap, "map_new_more")
            .expect("failed map new more");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed map new branch");

        self.builder.position_at_end(body_block);
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, idx, "map_new");
        self.build_map_entry_field_store(
            entry_ptr,
            MAP_ENTRY_STATE_OFFSET,
            self.i64_type.const_zero(),
            "map_new_state",
        );
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "map_new_next")
            .expect("failed map new next");
        self.builder.build_unconditional_branch(loop_block).expect("failed map new loop");
        idx_phi.add_incoming(&[(&next, body_block)]);

        self.builder.position_at_end(done_block);
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_MAP as u64, false),
                header_ptr,
                "map_new_result",
            )))
            .expect("failed map new return");
    }

    pub(super) fn define_pair_map_grow(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        let zero_loop = self.context.append_basic_block(function, "zero_loop");
        let zero_body = self.context.append_basic_block(function, "zero_body");
        let zero_done = self.context.append_basic_block(function, "zero_done");
        let rehash_loop = self.context.append_basic_block(function, "rehash_loop");
        let rehash_body = self.context.append_basic_block(function, "rehash_body");
        let probe_loop = self.context.append_basic_block(function, "probe_loop");
        let probe_body = self.context.append_basic_block(function, "probe_body");
        let probe_store = self.context.append_basic_block(function, "probe_store");
        let probe_next = self.context.append_basic_block(function, "probe_next");
        let rehash_done = self.context.append_basic_block(function, "rehash_done");
        self.builder.position_at_end(entry);

        let map = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let map_payload = self.expect_tag_payload(map, TAG_MAP, "map_grow", ok_block, trap_block);

        self.builder.position_at_end(ok_block);
        let alloc = self.require_func("__alloc");
        let len = self.build_map_len_load(map_payload, "map_grow");
        let cap = self.build_map_cap_load(map_payload, "map_grow");
        let old_entries_ptr = self.build_map_ptr_load(map_payload, "map_grow");
        let new_cap = self
            .builder
            .build_int_mul(cap, self.i64_type.const_int(2, false), "map_grow_new_cap")
            .expect("failed map grow new cap");
        let bytes = self
            .builder
            .build_int_mul(
                new_cap,
                self.i64_type.const_int(MAP_ENTRY_SIZE as u64, false),
                "map_grow_bytes",
            )
            .expect("failed map grow bytes");
        let align = self.i64_type.const_int(8, false);
        let new_entries_ptr = self.build_boxed_call(alloc, &[bytes, align], "map_grow_entries");
        self.build_map_ptr_store(map_payload, new_entries_ptr, "map_grow");
        self.build_map_cap_store(map_payload, new_cap, "map_grow");
        self.build_map_len_store(map_payload, self.i64_type.const_zero(), "map_grow");
        self.builder.build_unconditional_branch(zero_loop).expect("failed map grow zero jump");
        let ok_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(zero_loop);
        let zero_idx_phi = self
            .builder
            .build_phi(self.i64_type, "map_grow_zero_idx")
            .expect("failed map grow zero idx phi");
        zero_idx_phi.add_incoming(&[(&self.i64_type.const_zero(), ok_end)]);
        let zero_idx = zero_idx_phi.as_basic_value().into_int_value();
        let zero_more = self
            .builder
            .build_int_compare(IntPredicate::ULT, zero_idx, new_cap, "map_grow_zero_more")
            .expect("failed map grow zero more");
        self.builder
            .build_conditional_branch(zero_more, zero_body, zero_done)
            .expect("failed map grow zero branch");

        self.builder.position_at_end(zero_body);
        let zero_entry_ptr = self.build_map_entry_ptr(new_entries_ptr, zero_idx, "map_grow_zero");
        self.build_map_entry_field_store(
            zero_entry_ptr,
            MAP_ENTRY_STATE_OFFSET,
            self.i64_type.const_zero(),
            "map_grow_zero_state",
        );
        let zero_next = self
            .builder
            .build_int_add(zero_idx, self.i64_type.const_int(1, false), "map_grow_zero_next")
            .expect("failed map grow zero next");
        self.builder.build_unconditional_branch(zero_loop).expect("failed map grow zero loop");
        let zero_body_end = self.builder.get_insert_block().unwrap();
        zero_idx_phi.add_incoming(&[(&zero_next, zero_body_end)]);

        self.builder.position_at_end(zero_done);
        self.builder.build_unconditional_branch(rehash_loop).expect("failed map grow rehash jump");
        let zero_done_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(rehash_loop);
        let rehash_idx_phi = self
            .builder
            .build_phi(self.i64_type, "map_grow_rehash_idx")
            .expect("failed map grow rehash idx phi");
        rehash_idx_phi.add_incoming(&[(&self.i64_type.const_zero(), zero_done_end)]);
        let rehash_idx = rehash_idx_phi.as_basic_value().into_int_value();
        let rehash_more = self
            .builder
            .build_int_compare(IntPredicate::ULT, rehash_idx, cap, "map_grow_rehash_more")
            .expect("failed map grow rehash more");
        self.builder
            .build_conditional_branch(rehash_more, rehash_body, rehash_done)
            .expect("failed map grow rehash branch");

        self.builder.position_at_end(rehash_body);
        let old_entry_ptr = self.build_map_entry_ptr(old_entries_ptr, rehash_idx, "map_grow_old");
        let old_state = self.build_map_entry_field_load(
            old_entry_ptr,
            MAP_ENTRY_STATE_OFFSET,
            "map_grow_state",
        );
        let old_occupied = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                old_state,
                self.i64_type.const_int(MAP_ENTRY_OCCUPIED as u64, false),
                "map_grow_occupied",
            )
            .expect("failed map grow occupied");
        let insert_block = self.context.append_basic_block(function, "map_grow_insert");
        let next_block = self.context.append_basic_block(function, "map_grow_next");
        self.builder
            .build_conditional_branch(old_occupied, insert_block, next_block)
            .expect("failed map grow occupied branch");

        self.builder.position_at_end(insert_block);
        let old_hash =
            self.build_map_entry_field_load(old_entry_ptr, MAP_ENTRY_HASH_OFFSET, "map_grow_hash");
        let old_key =
            self.build_map_entry_field_load(old_entry_ptr, MAP_ENTRY_KEY_OFFSET, "map_grow_key");
        let old_value_tag = self.build_map_entry_field_load(
            old_entry_ptr,
            MAP_ENTRY_VALUE_TAG_OFFSET,
            "map_grow_value_tag",
        );
        let old_value_payload = self.build_map_entry_field_load(
            old_entry_ptr,
            MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            "map_grow_value_payload",
        );
        let start_idx = self
            .builder
            .build_int_unsigned_rem(old_hash, new_cap, "map_grow_start_idx")
            .expect("failed map grow start idx");
        let insert_end = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(probe_loop).expect("failed map grow probe jump");

        self.builder.position_at_end(probe_loop);
        let probe_idx_phi = self
            .builder
            .build_phi(self.i64_type, "map_grow_probe_idx")
            .expect("failed map grow probe idx phi");
        probe_idx_phi.add_incoming(&[(&start_idx, insert_end)]);
        let probe_idx = probe_idx_phi.as_basic_value().into_int_value();
        self.builder
            .build_unconditional_branch(probe_body)
            .expect("failed map grow probe body jump");

        self.builder.position_at_end(probe_body);
        let probe_entry_ptr =
            self.build_map_entry_ptr(new_entries_ptr, probe_idx, "map_grow_probe");
        let probe_state = self.build_map_entry_field_load(
            probe_entry_ptr,
            MAP_ENTRY_STATE_OFFSET,
            "map_grow_probe_state",
        );
        let probe_empty = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                probe_state,
                self.i64_type.const_zero(),
                "map_grow_probe_empty",
            )
            .expect("failed map grow probe empty");
        self.builder
            .build_conditional_branch(probe_empty, probe_store, probe_next)
            .expect("failed map grow probe branch");

        self.builder.position_at_end(probe_store);
        self.build_map_entry_field_store(
            probe_entry_ptr,
            MAP_ENTRY_HASH_OFFSET,
            old_hash,
            "map_grow_store_hash",
        );
        self.build_map_entry_field_store(
            probe_entry_ptr,
            MAP_ENTRY_KEY_OFFSET,
            old_key,
            "map_grow_store_key",
        );
        self.build_map_entry_field_store(
            probe_entry_ptr,
            MAP_ENTRY_VALUE_TAG_OFFSET,
            old_value_tag,
            "map_grow_store_tag",
        );
        self.build_map_entry_field_store(
            probe_entry_ptr,
            MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            old_value_payload,
            "map_grow_store_payload",
        );
        self.build_map_entry_field_store(
            probe_entry_ptr,
            MAP_ENTRY_STATE_OFFSET,
            self.i64_type.const_int(MAP_ENTRY_OCCUPIED as u64, false),
            "map_grow_store_state",
        );
        self.builder.build_unconditional_branch(next_block).expect("failed map grow stored jump");

        self.builder.position_at_end(probe_next);
        let probe_next_idx = self.build_map_next_index(probe_idx, new_cap, "map_grow_probe");
        self.builder.build_unconditional_branch(probe_loop).expect("failed map grow probe loop");
        let probe_next_end = self.builder.get_insert_block().unwrap();
        probe_idx_phi.add_incoming(&[(&probe_next_idx, probe_next_end)]);

        self.builder.position_at_end(next_block);
        let rehash_next = self
            .builder
            .build_int_add(rehash_idx, self.i64_type.const_int(1, false), "map_grow_rehash_next")
            .expect("failed map grow rehash next");
        self.builder.build_unconditional_branch(rehash_loop).expect("failed map grow rehash loop");
        let next_end = self.builder.get_insert_block().unwrap();
        rehash_idx_phi.add_incoming(&[(&rehash_next, next_end)]);

        self.builder.position_at_end(rehash_done);
        self.build_map_len_store(map_payload, len, "map_grow_restore_len");
        self.builder
            .build_return(Some(&self.make_pair_value(map.tag, map.payload, "map_grow_result")))
            .expect("failed map grow return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_map_len(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);
        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);
        let map = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let map_payload = self.expect_tag_payload(map, TAG_MAP, "map_len", ok_block, trap_block);
        self.builder.position_at_end(ok_block);
        let len = self.build_map_len_load(map_payload, "map_len");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                len,
                "map_len_result",
            )))
            .expect("failed map len return");
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_map_has(&mut self, name: &str, symbol: &str) {
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
        let key_ok_block = self.context.append_basic_block(function, "key_ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        let loop_block = self.context.append_basic_block(function, "loop");
        let body_block = self.context.append_basic_block(function, "body");
        let found_block = self.context.append_basic_block(function, "found");
        let done_block = self.context.append_basic_block(function, "done");
        self.builder.position_at_end(entry);
        let map = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let key = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let map_payload =
            self.expect_tag_payload(map, TAG_MAP, "map_has_map", ok_block, trap_block);
        self.builder.position_at_end(ok_block);
        let key_ptr =
            self.expect_tag_payload(key, TAG_STRING, "map_has_key", key_ok_block, trap_block);
        self.builder.position_at_end(key_ok_block);
        let cap = self.build_map_cap_load(map_payload, "map_has");
        let hash = self.build_string_hash_bytes(key_ptr, "map_has_hash");
        let start_idx = self
            .builder
            .build_int_unsigned_rem(hash, cap, "map_has_start_idx")
            .expect("failed map has start idx");
        let hash_done = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(loop_block).expect("failed map has hash jump");

        self.builder.position_at_end(loop_block);
        let idx_phi =
            self.builder.build_phi(self.i64_type, "map_has_idx").expect("failed map has idx phi");
        let probes_phi = self
            .builder
            .build_phi(self.i64_type, "map_has_probes")
            .expect("failed map has probes phi");
        idx_phi.add_incoming(&[(&start_idx, hash_done)]);
        probes_phi.add_incoming(&[(&self.i64_type.const_zero(), hash_done)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let probes = probes_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, probes, cap, "map_has_more")
            .expect("failed map has more");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed map has branch");

        self.builder.position_at_end(body_block);
        let entries_ptr = self.build_map_ptr_load(map_payload, "map_has");
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, idx, "map_has");
        let state =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_STATE_OFFSET, "map_has_state");
        let empty = self
            .builder
            .build_int_compare(IntPredicate::EQ, state, self.i64_type.const_zero(), "map_has_empty")
            .expect("failed map has empty");
        let cmp_block = self.context.append_basic_block(function, "map_has_cmp");
        let next_block = self.context.append_basic_block(function, "map_has_next");
        self.builder
            .build_conditional_branch(empty, done_block, cmp_block)
            .expect("failed map has empty branch");

        self.builder.position_at_end(cmp_block);
        let occupied = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.i64_type.const_int(MAP_ENTRY_OCCUPIED as u64, false),
                "map_has_occupied",
            )
            .expect("failed map has occupied");
        let maybe_match_block = self.context.append_basic_block(function, "map_has_maybe_match");
        self.builder
            .build_conditional_branch(occupied, maybe_match_block, next_block)
            .expect("failed map has occupied branch");

        self.builder.position_at_end(maybe_match_block);
        let stored_hash =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_HASH_OFFSET, "map_has_hash");
        let hash_equal = self
            .builder
            .build_int_compare(IntPredicate::EQ, stored_hash, hash, "map_has_hash_equal")
            .expect("failed map has hash equal");
        let stored_key =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_KEY_OFFSET, "map_has_key");
        let key_equal = self.build_string_eq_bytes(stored_key, key_ptr, "map_has_equal");
        let key_equal_bool = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                key_equal,
                self.i64_type.const_zero(),
                "map_has_key_equal_bool",
            )
            .expect("failed map has key equal bool");
        let both = self
            .builder
            .build_and(hash_equal, key_equal_bool, "map_has_both")
            .expect("failed map has both");
        self.builder
            .build_conditional_branch(both, found_block, next_block)
            .expect("failed map has both branch");

        self.builder.position_at_end(found_block);
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                self.i64_type.const_int(1, false),
                "map_has_found_result",
            )))
            .expect("failed map has found return");

        self.builder.position_at_end(next_block);
        let next_idx = self.build_map_next_index(idx, cap, "map_has");
        let next_probes = self
            .builder
            .build_int_add(probes, self.i64_type.const_int(1, false), "map_has_next_probes")
            .expect("failed map has next probes");
        self.builder.build_unconditional_branch(loop_block).expect("failed map has loop");
        idx_phi.add_incoming(&[(&next_idx, next_block)]);
        probes_phi.add_incoming(&[(&next_probes, next_block)]);

        self.builder.position_at_end(done_block);
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                self.i64_type.const_zero(),
                "map_has_result",
            )))
            .expect("failed map has return");
        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_map_get(&mut self, name: &str, symbol: &str) {
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
        let key_ok_block = self.context.append_basic_block(function, "key_ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        let loop_block = self.context.append_basic_block(function, "loop");
        let body_block = self.context.append_basic_block(function, "body");
        let found_block = self.context.append_basic_block(function, "found");
        self.builder.position_at_end(entry);
        let map = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let key = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let map_payload =
            self.expect_tag_payload(map, TAG_MAP, "map_get_map", ok_block, trap_block);
        self.builder.position_at_end(ok_block);
        let key_ptr =
            self.expect_tag_payload(key, TAG_STRING, "map_get_key", key_ok_block, trap_block);
        self.builder.position_at_end(key_ok_block);
        let cap = self.build_map_cap_load(map_payload, "map_get");
        let hash = self.build_string_hash_bytes(key_ptr, "map_get_hash");
        let start_idx = self
            .builder
            .build_int_unsigned_rem(hash, cap, "map_get_start_idx")
            .expect("failed map get start idx");
        let hash_done = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(loop_block).expect("failed map get hash jump");

        self.builder.position_at_end(loop_block);
        let idx_phi =
            self.builder.build_phi(self.i64_type, "map_get_idx").expect("failed map get idx phi");
        let probes_phi = self
            .builder
            .build_phi(self.i64_type, "map_get_probes")
            .expect("failed map get probes phi");
        idx_phi.add_incoming(&[(&start_idx, hash_done)]);
        probes_phi.add_incoming(&[(&self.i64_type.const_zero(), hash_done)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let probes = probes_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, probes, cap, "map_get_more")
            .expect("failed map get more");
        self.builder
            .build_conditional_branch(more, body_block, trap_block)
            .expect("failed map get branch");

        self.builder.position_at_end(body_block);
        let entries_ptr = self.build_map_ptr_load(map_payload, "map_get");
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, idx, "map_get");
        let state =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_STATE_OFFSET, "map_get_state");
        let empty = self
            .builder
            .build_int_compare(IntPredicate::EQ, state, self.i64_type.const_zero(), "map_get_empty")
            .expect("failed map get empty");
        let cmp_block = self.context.append_basic_block(function, "map_get_cmp");
        let next_block = self.context.append_basic_block(function, "map_get_next");
        self.builder
            .build_conditional_branch(empty, trap_block, cmp_block)
            .expect("failed map get empty branch");

        self.builder.position_at_end(cmp_block);
        let occupied = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.i64_type.const_int(MAP_ENTRY_OCCUPIED as u64, false),
                "map_get_occupied",
            )
            .expect("failed map get occupied");
        let maybe_match_block = self.context.append_basic_block(function, "map_get_maybe_match");
        self.builder
            .build_conditional_branch(occupied, maybe_match_block, next_block)
            .expect("failed map get occupied branch");

        self.builder.position_at_end(maybe_match_block);
        let stored_hash =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_HASH_OFFSET, "map_get_hash");
        let hash_equal = self
            .builder
            .build_int_compare(IntPredicate::EQ, stored_hash, hash, "map_get_hash_equal")
            .expect("failed map get hash equal");
        let stored_key =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_KEY_OFFSET, "map_get_key");
        let key_equal = self.build_string_eq_bytes(stored_key, key_ptr, "map_get_equal");
        let key_equal_bool = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                key_equal,
                self.i64_type.const_zero(),
                "map_get_key_equal_bool",
            )
            .expect("failed map get key equal bool");
        let both = self
            .builder
            .build_and(hash_equal, key_equal_bool, "map_get_both")
            .expect("failed map get both");
        self.builder
            .build_conditional_branch(both, found_block, next_block)
            .expect("failed map get both branch");

        self.builder.position_at_end(found_block);
        let value_tag = self.build_map_entry_field_load(
            entry_ptr,
            MAP_ENTRY_VALUE_TAG_OFFSET,
            "map_get_value_tag",
        );
        let value_payload = self.build_map_entry_field_load(
            entry_ptr,
            MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            "map_get_value_payload",
        );
        self.builder
            .build_return(Some(&self.make_pair_value(value_tag, value_payload, "map_get_result")))
            .expect("failed map get return");

        self.builder.position_at_end(next_block);
        let next = self.build_map_next_index(idx, cap, "map_get");
        let next_probes = self
            .builder
            .build_int_add(probes, self.i64_type.const_int(1, false), "map_get_next_probes")
            .expect("failed map get next probes");
        self.builder.build_unconditional_branch(loop_block).expect("failed map get loop");
        idx_phi.add_incoming(&[(&next, next_block)]);
        probes_phi.add_incoming(&[(&next_probes, next_block)]);

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_map_set(&mut self, name: &str, symbol: &str) {
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
        let key_ok_block = self.context.append_basic_block(function, "key_ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        let loop_block = self.context.append_basic_block(function, "loop");
        let body_block = self.context.append_basic_block(function, "body");
        let grow_block = self.context.append_basic_block(function, "map_set_grow");
        let insert_block = self.context.append_basic_block(function, "insert");
        self.builder.position_at_end(entry);
        let map = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let key = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let value = CompiledValue {
            tag: function.get_nth_param(4).unwrap().into_int_value(),
            payload: function.get_nth_param(5).unwrap().into_int_value(),
        };
        let map_payload =
            self.expect_tag_payload(map, TAG_MAP, "map_set_map", ok_block, trap_block);
        self.builder.position_at_end(ok_block);
        let key_ptr =
            self.expect_tag_payload(key, TAG_STRING, "map_set_key", key_ok_block, trap_block);
        self.builder.position_at_end(key_ok_block);
        let cap = self.build_map_cap_load(map_payload, "map_set");
        let hash = self.build_string_hash_bytes(key_ptr, "map_set_hash");
        let start_idx = self
            .builder
            .build_int_unsigned_rem(hash, cap, "map_set_start_idx")
            .expect("failed map set start idx");
        let no_tombstone = self.i64_type.const_all_ones();
        let hash_done = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(loop_block).expect("failed map set hash jump");

        self.builder.position_at_end(loop_block);
        let idx_phi =
            self.builder.build_phi(self.i64_type, "map_set_idx").expect("failed map set idx phi");
        let probes_phi = self
            .builder
            .build_phi(self.i64_type, "map_set_probes")
            .expect("failed map set probes phi");
        let first_tombstone_phi = self
            .builder
            .build_phi(self.i64_type, "map_set_first_tombstone")
            .expect("failed map set tombstone phi");
        idx_phi.add_incoming(&[(&start_idx, hash_done)]);
        probes_phi.add_incoming(&[(&self.i64_type.const_zero(), hash_done)]);
        first_tombstone_phi.add_incoming(&[(&no_tombstone, hash_done)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let probes = probes_phi.as_basic_value().into_int_value();
        let first_tombstone = first_tombstone_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, probes, cap, "map_set_more")
            .expect("failed map set more");
        self.builder
            .build_conditional_branch(more, body_block, grow_block)
            .expect("failed map set loop branch");

        self.builder.position_at_end(body_block);
        let entries_ptr = self.build_map_ptr_load(map_payload, "map_set");
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, idx, "map_set");
        let state =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_STATE_OFFSET, "map_set_state");
        let empty = self
            .builder
            .build_int_compare(IntPredicate::EQ, state, self.i64_type.const_zero(), "map_set_empty")
            .expect("failed map set empty");
        let cmp_block = self.context.append_basic_block(function, "map_set_cmp");
        self.builder
            .build_conditional_branch(empty, insert_block, cmp_block)
            .expect("failed map set empty branch");

        self.builder.position_at_end(cmp_block);
        let occupied = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.i64_type.const_int(MAP_ENTRY_OCCUPIED as u64, false),
                "map_set_occupied",
            )
            .expect("failed map set occupied");
        let maybe_match_block = self.context.append_basic_block(function, "map_set_maybe_match");
        let tombstone_block = self.context.append_basic_block(function, "map_set_tombstone");
        self.builder
            .build_conditional_branch(occupied, maybe_match_block, tombstone_block)
            .expect("failed map set occupied branch");

        self.builder.position_at_end(maybe_match_block);
        let stored_hash =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_HASH_OFFSET, "map_set_hash");
        let hash_equal = self
            .builder
            .build_int_compare(IntPredicate::EQ, stored_hash, hash, "map_set_hash_equal")
            .expect("failed map set hash equal");
        let stored_key =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_KEY_OFFSET, "map_set_key");
        let equal = self.build_string_eq_bytes(stored_key, key_ptr, "map_set_equal");
        let equal_bool = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                equal,
                self.i64_type.const_zero(),
                "map_set_equal_bool",
            )
            .expect("failed map set equal bool");
        let both = self
            .builder
            .build_and(hash_equal, equal_bool, "map_set_both")
            .expect("failed map set both");
        let update_block = self.context.append_basic_block(function, "map_set_update");
        let next_block = self.context.append_basic_block(function, "map_set_next");
        self.builder
            .build_conditional_branch(both, update_block, next_block)
            .expect("failed map set match branch");
        let maybe_match_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(update_block);
        self.build_map_entry_field_store(
            entry_ptr,
            MAP_ENTRY_VALUE_TAG_OFFSET,
            value.tag,
            "map_set_update_tag",
        );
        self.build_map_entry_field_store(
            entry_ptr,
            MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            value.payload,
            "map_set_update_payload",
        );
        self.builder
            .build_return(Some(&self.make_pair_value(map.tag, map.payload, "map_set_result")))
            .expect("failed map set update return");

        self.builder.position_at_end(tombstone_block);
        let is_tombstone = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.i64_type.const_int(MAP_ENTRY_TOMBSTONE as u64, false),
                "map_set_is_tombstone",
            )
            .expect("failed map set tombstone");
        let need_tombstone = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                first_tombstone,
                no_tombstone,
                "map_set_need_tombstone",
            )
            .expect("failed map set need tombstone");
        let record_tombstone = self
            .builder
            .build_and(is_tombstone, need_tombstone, "map_set_record_tombstone")
            .expect("failed map set record tombstone");
        let next_tombstone = self
            .builder
            .build_select(record_tombstone, idx, first_tombstone, "map_set_next_tombstone")
            .expect("failed map set next tombstone")
            .into_int_value();
        self.builder.build_unconditional_branch(next_block).expect("failed map set tombstone jump");
        let tombstone_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(next_block);
        let carried_tombstone_phi = self
            .builder
            .build_phi(self.i64_type, "map_set_carried_tombstone")
            .expect("failed map set carried tombstone phi");
        carried_tombstone_phi.add_incoming(&[(&first_tombstone, maybe_match_end)]);
        carried_tombstone_phi.add_incoming(&[(&next_tombstone, tombstone_end)]);
        let carried_tombstone = carried_tombstone_phi.as_basic_value().into_int_value();
        let next = self.build_map_next_index(idx, cap, "map_set");
        let next_probes = self
            .builder
            .build_int_add(probes, self.i64_type.const_int(1, false), "map_set_next_probes")
            .expect("failed map set next probes");
        self.builder.build_unconditional_branch(loop_block).expect("failed map set loop");
        let next_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next, next_end)]);
        probes_phi.add_incoming(&[(&next_probes, next_end)]);
        first_tombstone_phi.add_incoming(&[(&carried_tombstone, next_end)]);

        self.builder.position_at_end(insert_block);
        let use_tombstone = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                first_tombstone,
                no_tombstone,
                "map_set_use_tombstone",
            )
            .expect("failed map set use tombstone");
        let len = self.build_map_len_load(map_payload, "map_set_insert");
        let has_room = self
            .builder
            .build_int_compare(IntPredicate::ULT, len, cap, "map_set_has_room")
            .expect("failed map set has room");
        let allow_insert = self
            .builder
            .build_or(use_tombstone, has_room, "map_set_allow_insert")
            .expect("failed map set allow insert");
        let do_insert_block = self.context.append_basic_block(function, "map_set_do_insert");
        self.builder
            .build_conditional_branch(allow_insert, do_insert_block, trap_block)
            .expect("failed map set insert branch");

        self.builder.position_at_end(do_insert_block);
        let insert_idx = self
            .builder
            .build_select(use_tombstone, first_tombstone, idx, "map_set_insert_idx")
            .expect("failed map set insert idx")
            .into_int_value();
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, insert_idx, "map_set_insert");
        self.build_map_entry_field_store(
            entry_ptr,
            MAP_ENTRY_HASH_OFFSET,
            hash,
            "map_set_insert_hash",
        );
        self.build_map_entry_field_store(
            entry_ptr,
            MAP_ENTRY_KEY_OFFSET,
            key_ptr,
            "map_set_insert_key",
        );
        self.build_map_entry_field_store(
            entry_ptr,
            MAP_ENTRY_VALUE_TAG_OFFSET,
            value.tag,
            "map_set_insert_tag",
        );
        self.build_map_entry_field_store(
            entry_ptr,
            MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            value.payload,
            "map_set_insert_payload",
        );
        self.build_map_entry_field_store(
            entry_ptr,
            MAP_ENTRY_STATE_OFFSET,
            self.i64_type.const_int(MAP_ENTRY_OCCUPIED as u64, false),
            "map_set_insert_state",
        );
        let new_len = self
            .builder
            .build_int_add(len, self.i64_type.const_int(1, false), "map_set_new_len")
            .expect("failed map set new len");
        self.build_map_len_store(map_payload, new_len, "map_set");
        self.builder
            .build_return(Some(&self.make_pair_value(
                map.tag,
                map.payload,
                "map_set_insert_result",
            )))
            .expect("failed map set insert return");

        self.builder.position_at_end(grow_block);
        let grow_func = self.require_func("__rt_map_grow");
        let grown_map = self.build_internal_call(grow_func, &[map], "map_set_grow_call");
        let retry =
            self.build_internal_call(function, &[grown_map, key, value], "map_set_grow_retry");
        self.builder
            .build_return(Some(&self.make_pair_value(
                retry.tag,
                retry.payload,
                "map_set_grow_retry_result",
            )))
            .expect("failed map set grow-only return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_map_delete(&mut self, name: &str, symbol: &str) {
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
        let key_ok_block = self.context.append_basic_block(function, "key_ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        let loop_block = self.context.append_basic_block(function, "loop");
        let body_block = self.context.append_basic_block(function, "body");
        let found_block = self.context.append_basic_block(function, "found");
        self.builder.position_at_end(entry);
        let map = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let key = CompiledValue {
            tag: function.get_nth_param(2).unwrap().into_int_value(),
            payload: function.get_nth_param(3).unwrap().into_int_value(),
        };
        let map_payload =
            self.expect_tag_payload(map, TAG_MAP, "map_delete_map", ok_block, trap_block);
        self.builder.position_at_end(ok_block);
        let key_ptr =
            self.expect_tag_payload(key, TAG_STRING, "map_delete_key", key_ok_block, trap_block);
        self.builder.position_at_end(key_ok_block);
        let cap = self.build_map_cap_load(map_payload, "map_delete");
        let hash = self.build_string_hash_bytes(key_ptr, "map_delete_hash");
        let start_idx = self
            .builder
            .build_int_unsigned_rem(hash, cap, "map_delete_start_idx")
            .expect("failed map delete start idx");
        let hash_done = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(loop_block).expect("failed map delete hash jump");

        self.builder.position_at_end(loop_block);
        let idx_phi = self
            .builder
            .build_phi(self.i64_type, "map_delete_idx")
            .expect("failed map delete idx phi");
        let probes_phi = self
            .builder
            .build_phi(self.i64_type, "map_delete_probes")
            .expect("failed map delete probes phi");
        idx_phi.add_incoming(&[(&start_idx, hash_done)]);
        probes_phi.add_incoming(&[(&self.i64_type.const_zero(), hash_done)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let probes = probes_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, probes, cap, "map_delete_more")
            .expect("failed map delete more");
        self.builder
            .build_conditional_branch(more, body_block, trap_block)
            .expect("failed map delete branch");

        self.builder.position_at_end(body_block);
        let entries_ptr = self.build_map_ptr_load(map_payload, "map_delete");
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, idx, "map_delete");
        let state =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_STATE_OFFSET, "map_delete_state");
        let empty = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.i64_type.const_zero(),
                "map_delete_empty",
            )
            .expect("failed map delete empty");
        let cmp_block = self.context.append_basic_block(function, "map_delete_cmp");
        self.builder
            .build_conditional_branch(empty, trap_block, cmp_block)
            .expect("failed map delete empty branch");

        self.builder.position_at_end(cmp_block);
        let occupied = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.i64_type.const_int(MAP_ENTRY_OCCUPIED as u64, false),
                "map_delete_occupied",
            )
            .expect("failed map delete occupied");
        let maybe_match_block = self.context.append_basic_block(function, "map_delete_maybe_match");
        let next_block = self.context.append_basic_block(function, "map_delete_next");
        self.builder
            .build_conditional_branch(occupied, maybe_match_block, next_block)
            .expect("failed map delete occupied branch");

        self.builder.position_at_end(maybe_match_block);
        let stored_hash =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_HASH_OFFSET, "map_delete_hash");
        let hash_equal = self
            .builder
            .build_int_compare(IntPredicate::EQ, stored_hash, hash, "map_delete_hash_equal")
            .expect("failed map delete hash equal");
        let stored_key =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_KEY_OFFSET, "map_delete_key");
        let equal = self.build_string_eq_bytes(stored_key, key_ptr, "map_delete_equal");
        let equal_bool = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                equal,
                self.i64_type.const_zero(),
                "map_delete_equal_bool",
            )
            .expect("failed map delete equal bool");
        let both = self
            .builder
            .build_and(hash_equal, equal_bool, "map_delete_both")
            .expect("failed map delete both");
        self.builder
            .build_conditional_branch(both, found_block, next_block)
            .expect("failed map delete match branch");

        self.builder.position_at_end(found_block);
        let removed_tag = self.build_map_entry_field_load(
            entry_ptr,
            MAP_ENTRY_VALUE_TAG_OFFSET,
            "map_delete_removed_tag",
        );
        let removed_payload = self.build_map_entry_field_load(
            entry_ptr,
            MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            "map_delete_removed_payload",
        );
        self.build_map_entry_field_store(
            entry_ptr,
            MAP_ENTRY_STATE_OFFSET,
            self.i64_type.const_int(MAP_ENTRY_TOMBSTONE as u64, false),
            "map_delete_tombstone",
        );
        let len = self.build_map_len_load(map_payload, "map_delete_len");
        let new_len = self
            .builder
            .build_int_sub(len, self.i64_type.const_int(1, false), "map_delete_new_len")
            .expect("failed map delete new len");
        self.build_map_len_store(map_payload, new_len, "map_delete");
        self.builder
            .build_return(Some(&self.make_pair_value(
                removed_tag,
                removed_payload,
                "map_delete_result",
            )))
            .expect("failed map delete return");

        self.builder.position_at_end(next_block);
        let next = self.build_map_next_index(idx, cap, "map_delete");
        let next_probes = self
            .builder
            .build_int_add(probes, self.i64_type.const_int(1, false), "map_delete_next_probes")
            .expect("failed map delete next probes");
        self.builder.build_unconditional_branch(loop_block).expect("failed map delete loop");
        let next_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next, next_end)]);
        probes_phi.add_incoming(&[(&next_probes, next_end)]);

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    pub(super) fn define_pair_map_keys(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
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
        let map = CompiledValue {
            tag: function.get_first_param().unwrap().into_int_value(),
            payload: function.get_nth_param(1).unwrap().into_int_value(),
        };
        let map_payload = self.expect_tag_payload(map, TAG_MAP, "map_keys", ok_block, trap_block);

        self.builder.position_at_end(ok_block);
        let list =
            self.build_internal_call(self.require_func("__rt_list_new"), &[], "map_keys_new_list");
        let list_tag = list.tag;
        let list_payload = list.payload;
        let entries_ptr = self.build_map_ptr_load(map_payload, "map_keys");
        let cap = self.build_map_cap_load(map_payload, "map_keys");
        self.builder.build_unconditional_branch(loop_block).expect("failed map keys loop jump");
        let ok_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let idx_phi =
            self.builder.build_phi(self.i64_type, "map_keys_idx").expect("failed map keys phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), ok_end)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, cap, "map_keys_more")
            .expect("failed map keys more");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed map keys branch");

        self.builder.position_at_end(body_block);
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, idx, "map_keys");
        let state =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_STATE_OFFSET, "map_keys_state");
        let occupied = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.i64_type.const_int(MAP_ENTRY_OCCUPIED as u64, false),
                "map_keys_occupied",
            )
            .expect("failed map keys occupied");
        let push_block = self.context.append_basic_block(function, "map_keys_push");
        let next_block = self.context.append_basic_block(function, "map_keys_next");
        self.builder
            .build_conditional_branch(occupied, push_block, next_block)
            .expect("failed map keys occupied branch");

        self.builder.position_at_end(push_block);
        let key_ptr =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_KEY_OFFSET, "map_keys_key");
        let string_value = CompiledValue {
            tag: self.i64_type.const_int(TAG_STRING as u64, false),
            payload: key_ptr,
        };
        let _push = self.build_internal_call(
            self.require_func("__rt_list_push"),
            &[CompiledValue { tag: list_tag, payload: list_payload }, string_value],
            "map_keys_push",
        );
        self.builder.build_unconditional_branch(next_block).expect("failed map keys push jump");

        self.builder.position_at_end(next_block);
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "map_keys_next")
            .expect("failed map keys next");
        self.builder.build_unconditional_branch(loop_block).expect("failed map keys loop");
        let next_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next, next_end)]);

        self.builder.position_at_end(done_block);
        self.builder
            .build_return(Some(&self.make_pair_value(list_tag, list_payload, "map_keys_result")))
            .expect("failed map keys return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }
}
