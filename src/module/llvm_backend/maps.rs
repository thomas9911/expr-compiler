use super::{CompiledValue, LlvmCompiler};
use crate::value::{
    MAP_CAP_OFFSET, MAP_ENTRY_KEY_OFFSET, MAP_ENTRY_OCCUPIED, MAP_ENTRY_SIZE,
    MAP_ENTRY_STATE_OFFSET, MAP_ENTRY_VALUE_PAYLOAD_OFFSET, MAP_ENTRY_VALUE_TAG_OFFSET,
    MAP_HEADER_SIZE, MAP_LEN_OFFSET, MAP_PTR_OFFSET, TAG_INT, TAG_MAP, TAG_STRING,
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
            self.expect_tag_payload(key, TAG_STRING, "map_has_key", loop_block, trap_block);

        self.builder.position_at_end(loop_block);
        let idx_phi =
            self.builder.build_phi(self.i64_type, "map_has_idx").expect("failed map has phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), ok_block)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let len = self.build_map_len_load(map_payload, "map_has");
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, "map_has_more")
            .expect("failed map has more");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed map has branch");

        self.builder.position_at_end(body_block);
        let entries_ptr = self.build_map_ptr_load(map_payload, "map_has");
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, idx, "map_has");
        let state =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_STATE_OFFSET, "map_has_state");
        let occupied = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.i64_type.const_int(MAP_ENTRY_OCCUPIED as u64, false),
                "map_has_occupied",
            )
            .expect("failed map has occupied");
        let cmp_block = self.context.append_basic_block(function, "map_has_cmp");
        let next_block = self.context.append_basic_block(function, "map_has_next");
        self.builder
            .build_conditional_branch(occupied, cmp_block, next_block)
            .expect("failed map has occupied branch");

        self.builder.position_at_end(cmp_block);
        let stored_key =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_KEY_OFFSET, "map_has_key");
        let equal = self.build_string_eq_bytes(stored_key, key_ptr, "map_has_equal");
        let equal_bool = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                equal,
                self.i64_type.const_zero(),
                "map_has_equal_bool",
            )
            .expect("failed map has equal bool");
        self.builder
            .build_conditional_branch(equal_bool, found_block, next_block)
            .expect("failed map has equal branch");

        self.builder.position_at_end(found_block);
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                self.i64_type.const_int(1, false),
                "map_has_found_result",
            )))
            .expect("failed map has found return");

        self.builder.position_at_end(next_block);
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "map_has_next_idx")
            .expect("failed map has next");
        self.builder.build_unconditional_branch(loop_block).expect("failed map has loop");
        idx_phi.add_incoming(&[(&next, next_block)]);

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
            self.expect_tag_payload(key, TAG_STRING, "map_get_key", loop_block, trap_block);

        self.builder.position_at_end(loop_block);
        let idx_phi =
            self.builder.build_phi(self.i64_type, "map_get_idx").expect("failed map get phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), ok_block)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let len = self.build_map_len_load(map_payload, "map_get");
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, "map_get_more")
            .expect("failed map get more");
        self.builder
            .build_conditional_branch(more, body_block, trap_block)
            .expect("failed map get branch");

        self.builder.position_at_end(body_block);
        let entries_ptr = self.build_map_ptr_load(map_payload, "map_get");
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, idx, "map_get");
        let state =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_STATE_OFFSET, "map_get_state");
        let occupied = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.i64_type.const_int(MAP_ENTRY_OCCUPIED as u64, false),
                "map_get_occupied",
            )
            .expect("failed map get occupied");
        let cmp_block = self.context.append_basic_block(function, "map_get_cmp");
        let next_block = self.context.append_basic_block(function, "map_get_next");
        self.builder
            .build_conditional_branch(occupied, cmp_block, next_block)
            .expect("failed map get occupied branch");

        self.builder.position_at_end(cmp_block);
        let stored_key =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_KEY_OFFSET, "map_get_key");
        let equal = self.build_string_eq_bytes(stored_key, key_ptr, "map_get_equal");
        let equal_bool = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                equal,
                self.i64_type.const_zero(),
                "map_get_equal_bool",
            )
            .expect("failed map get equal bool");
        self.builder
            .build_conditional_branch(equal_bool, found_block, next_block)
            .expect("failed map get equal branch");

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
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "map_get_next_idx")
            .expect("failed map get next");
        self.builder.build_unconditional_branch(loop_block).expect("failed map get loop");
        idx_phi.add_incoming(&[(&next, next_block)]);

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
        let trap_block = self.context.append_basic_block(function, "trap");
        let loop_block = self.context.append_basic_block(function, "loop");
        let body_block = self.context.append_basic_block(function, "body");
        let insert_block = self.context.append_basic_block(function, "insert");
        let full_block = self.context.append_basic_block(function, "full");
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
            self.expect_tag_payload(key, TAG_STRING, "map_set_key", loop_block, trap_block);

        self.builder.position_at_end(loop_block);
        let idx_phi =
            self.builder.build_phi(self.i64_type, "map_set_idx").expect("failed map set phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), ok_block)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let len = self.build_map_len_load(map_payload, "map_set");
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, "map_set_more")
            .expect("failed map set more");
        self.builder
            .build_conditional_branch(more, body_block, insert_block)
            .expect("failed map set branch");

        self.builder.position_at_end(body_block);
        let entries_ptr = self.build_map_ptr_load(map_payload, "map_set");
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, idx, "map_set");
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
        let update_block = self.context.append_basic_block(function, "map_set_update");
        let next_block = self.context.append_basic_block(function, "map_set_next");
        self.builder
            .build_conditional_branch(equal_bool, update_block, next_block)
            .expect("failed map set equal branch");

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

        self.builder.position_at_end(next_block);
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "map_set_next_idx")
            .expect("failed map set next");
        self.builder.build_unconditional_branch(loop_block).expect("failed map set loop");
        idx_phi.add_incoming(&[(&next, next_block)]);

        self.builder.position_at_end(insert_block);
        let cap = self.build_map_cap_load(map_payload, "map_set");
        let has_room = self
            .builder
            .build_int_compare(IntPredicate::ULT, len, cap, "map_set_has_room")
            .expect("failed map set has room");
        self.builder
            .build_conditional_branch(has_room, full_block, trap_block)
            .expect("failed map set insert branch");

        self.builder.position_at_end(full_block);
        let entries_ptr = self.build_map_ptr_load(map_payload, "map_set_insert");
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, len, "map_set_insert");
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
            self.expect_tag_payload(key, TAG_STRING, "map_delete_key", loop_block, trap_block);

        self.builder.position_at_end(loop_block);
        let idx_phi =
            self.builder.build_phi(self.i64_type, "map_delete_idx").expect("failed map delete phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), ok_block)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let len = self.build_map_len_load(map_payload, "map_delete");
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, "map_delete_more")
            .expect("failed map delete more");
        self.builder
            .build_conditional_branch(more, body_block, trap_block)
            .expect("failed map delete branch");

        self.builder.position_at_end(body_block);
        let entries_ptr = self.build_map_ptr_load(map_payload, "map_delete");
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, idx, "map_delete");
        let state =
            self.build_map_entry_field_load(entry_ptr, MAP_ENTRY_STATE_OFFSET, "map_delete_state");
        let occupied = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                state,
                self.i64_type.const_int(MAP_ENTRY_OCCUPIED as u64, false),
                "map_delete_occupied",
            )
            .expect("failed map delete occupied");
        let cmp_block = self.context.append_basic_block(function, "map_delete_cmp");
        let next_block = self.context.append_basic_block(function, "map_delete_next");
        self.builder
            .build_conditional_branch(occupied, cmp_block, next_block)
            .expect("failed map delete occupied branch");

        self.builder.position_at_end(cmp_block);
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
        self.builder
            .build_conditional_branch(equal_bool, found_block, next_block)
            .expect("failed map delete equal branch");

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
        let shift_loop = self.context.append_basic_block(function, "map_delete_shift_loop");
        let shift_body = self.context.append_basic_block(function, "map_delete_shift_body");
        let done_block = self.context.append_basic_block(function, "map_delete_done");
        let start = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "map_delete_shift_start")
            .expect("failed map delete shift start");
        self.builder.build_unconditional_branch(shift_loop).expect("failed map delete shift jump");
        let found_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(shift_loop);
        let cur_phi = self
            .builder
            .build_phi(self.i64_type, "map_delete_shift_idx")
            .expect("failed map delete shift phi");
        cur_phi.add_incoming(&[(&start, found_end)]);
        let cur = cur_phi.as_basic_value().into_int_value();
        let shift_more = self
            .builder
            .build_int_compare(IntPredicate::ULT, cur, len, "map_delete_shift_more")
            .expect("failed map delete shift more");
        self.builder
            .build_conditional_branch(shift_more, shift_body, done_block)
            .expect("failed map delete shift branch");

        self.builder.position_at_end(shift_body);
        let src_ptr = self.build_map_entry_ptr(entries_ptr, cur, "map_delete_shift_src");
        let dst_index = self
            .builder
            .build_int_sub(cur, self.i64_type.const_int(1, false), "map_delete_shift_dst_index")
            .expect("failed map delete shift dst index");
        let dst_ptr = self.build_map_entry_ptr(entries_ptr, dst_index, "map_delete_shift_dst");
        let moved_hash = self.build_map_entry_field_load(src_ptr, 0, "map_delete_shift_hash");
        let moved_key =
            self.build_map_entry_field_load(src_ptr, MAP_ENTRY_KEY_OFFSET, "map_delete_shift_key");
        let moved_tag = self.build_map_entry_field_load(
            src_ptr,
            MAP_ENTRY_VALUE_TAG_OFFSET,
            "map_delete_shift_tag",
        );
        let moved_payload = self.build_map_entry_field_load(
            src_ptr,
            MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            "map_delete_shift_payload",
        );
        let moved_state = self.build_map_entry_field_load(
            src_ptr,
            MAP_ENTRY_STATE_OFFSET,
            "map_delete_shift_state",
        );
        self.build_map_entry_field_store(dst_ptr, 0, moved_hash, "map_delete_shift_store_hash");
        self.build_map_entry_field_store(
            dst_ptr,
            MAP_ENTRY_KEY_OFFSET,
            moved_key,
            "map_delete_shift_store_key",
        );
        self.build_map_entry_field_store(
            dst_ptr,
            MAP_ENTRY_VALUE_TAG_OFFSET,
            moved_tag,
            "map_delete_shift_store_tag",
        );
        self.build_map_entry_field_store(
            dst_ptr,
            MAP_ENTRY_VALUE_PAYLOAD_OFFSET,
            moved_payload,
            "map_delete_shift_store_payload",
        );
        self.build_map_entry_field_store(
            dst_ptr,
            MAP_ENTRY_STATE_OFFSET,
            moved_state,
            "map_delete_shift_store_state",
        );
        let next = self
            .builder
            .build_int_add(cur, self.i64_type.const_int(1, false), "map_delete_shift_next")
            .expect("failed map delete shift next");
        self.builder.build_unconditional_branch(shift_loop).expect("failed map delete shift loop");
        let shift_body_end = self.builder.get_insert_block().unwrap();
        cur_phi.add_incoming(&[(&next, shift_body_end)]);

        self.builder.position_at_end(done_block);
        let new_len = self
            .builder
            .build_int_sub(len, self.i64_type.const_int(1, false), "map_delete_new_len")
            .expect("failed map delete new len");
        self.build_map_len_store(map_payload, new_len, "map_delete");
        let last_ptr = self.build_map_entry_ptr(entries_ptr, new_len, "map_delete_last");
        self.build_map_entry_field_store(
            last_ptr,
            MAP_ENTRY_STATE_OFFSET,
            self.i64_type.const_zero(),
            "map_delete_last_state",
        );
        self.builder
            .build_return(Some(&self.make_pair_value(
                removed_tag,
                removed_payload,
                "map_delete_result",
            )))
            .expect("failed map delete return");

        self.builder.position_at_end(next_block);
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "map_delete_next_idx")
            .expect("failed map delete next");
        self.builder.build_unconditional_branch(loop_block).expect("failed map delete loop");
        let next_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next, next_end)]);

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
        let len = self.build_map_len_load(map_payload, "map_keys");
        self.builder.build_unconditional_branch(loop_block).expect("failed map keys loop jump");
        let ok_end = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(loop_block);
        let idx_phi =
            self.builder.build_phi(self.i64_type, "map_keys_idx").expect("failed map keys phi");
        idx_phi.add_incoming(&[(&self.i64_type.const_zero(), ok_end)]);
        let idx = idx_phi.as_basic_value().into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, "map_keys_more")
            .expect("failed map keys more");
        self.builder
            .build_conditional_branch(more, body_block, done_block)
            .expect("failed map keys branch");

        self.builder.position_at_end(body_block);
        let entry_ptr = self.build_map_entry_ptr(entries_ptr, idx, "map_keys");
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
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "map_keys_next")
            .expect("failed map keys next");
        self.builder.build_unconditional_branch(loop_block).expect("failed map keys loop");
        let body_end = self.builder.get_insert_block().unwrap();
        idx_phi.add_incoming(&[(&next, body_end)]);

        self.builder.position_at_end(done_block);
        self.builder
            .build_return(Some(&self.make_pair_value(list_tag, list_payload, "map_keys_result")))
            .expect("failed map keys return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }
}
