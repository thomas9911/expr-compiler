use super::*;

impl<'ctx> LlvmCompiler<'ctx> {
    pub(super) fn pair_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context.struct_type(&[self.i64_type.into(), self.i64_type.into()], false)
    }

    pub(super) fn make_pair_value(
        &self,
        tag: IntValue<'ctx>,
        payload: IntValue<'ctx>,
        label: &str,
    ) -> inkwell::values::StructValue<'ctx> {
        let pair = self.pair_type().get_undef();
        let pair = self
            .builder
            .build_insert_value(pair, tag, 0, &format!("{label}_tag_insert"))
            .expect("failed to insert tag")
            .into_struct_value();
        self.builder
            .build_insert_value(pair, payload, 1, &format!("{label}_payload_insert"))
            .expect("failed to insert payload")
            .into_struct_value()
    }

    pub(super) fn int_value(&self, raw: IntValue<'ctx>) -> CompiledValue<'ctx> {
        CompiledValue { tag: self.i64_type.const_int(TAG_INT as u64, false), payload: raw }
    }

    pub(super) fn load_compiled_value(
        &self,
        ptr: PointerValue<'ctx>,
        label: &str,
    ) -> CompiledValue<'ctx> {
        let pair = self
            .builder
            .build_load(self.pair_type(), ptr, label)
            .expect("failed to load pair")
            .into_struct_value();
        let tag = self
            .builder
            .build_extract_value(pair, 0, &format!("{label}_tag"))
            .expect("failed to extract loaded tag")
            .into_int_value();
        let payload = self
            .builder
            .build_extract_value(pair, 1, &format!("{label}_payload"))
            .expect("failed to extract loaded payload")
            .into_int_value();
        CompiledValue { tag, payload }
    }

    pub(super) fn load_value_from_env(
        &self,
        env_ptr: IntValue<'ctx>,
        slot: usize,
        label: &str,
    ) -> CompiledValue<'ctx> {
        let env_raw = self
            .builder
            .build_int_to_ptr(
                env_ptr,
                self.context.ptr_type(Default::default()),
                &format!("{label}_env_ptr"),
            )
            .expect("failed to convert env ptr");
        let index = self.i64_type.const_int(slot as u64, false);
        self.build_list_value_load_from_data_ptr(env_raw, index, label)
    }

    pub(super) fn allocate_closure_for_function(
        &self,
        name: &str,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        current_env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let captures = self
            .closure_metadata
            .get(name)
            .map(|metadata| metadata.captures.as_slice())
            .unwrap_or(&[]);
        let alloc = self.require_func("__alloc");
        let env_raw = if captures.is_empty() {
            self.i64_type.const_zero()
        } else {
            let env_bytes =
                self.i64_type.const_int((captures.len() as i64 * VALUE_SIZE) as u64, false);
            let align = self.i64_type.const_int(std::mem::align_of::<i64>() as u64, false);
            let env_ptr = self.build_boxed_call(alloc, &[env_bytes, align], "closure_env_alloc");
            let env_data_ptr = self
                .builder
                .build_int_to_ptr(
                    env_ptr,
                    self.context.ptr_type(Default::default()),
                    "closure_env_data_ptr",
                )
                .expect("failed to convert closure env ptr");
            for (index, capture_name) in captures.iter().enumerate() {
                let value = self.resolve_named_value(
                    capture_name,
                    vars,
                    capture_slots,
                    current_env_ptr,
                    function,
                );
                self.build_list_value_store_from_data_ptr(
                    env_data_ptr,
                    self.i64_type.const_int(index as u64, false),
                    value,
                    &format!("closure_capture_{index}"),
                );
            }
            env_ptr
        };

        let closure_size = self.i64_type.const_int(CLOSURE_SIZE as u64, false);
        let closure_align = self.i64_type.const_int(std::mem::align_of::<i64>() as u64, false);
        let closure_ptr =
            self.build_boxed_call(alloc, &[closure_size, closure_align], "closure_alloc");
        let closure_raw_ptr = self
            .builder
            .build_int_to_ptr(
                closure_ptr,
                self.context.ptr_type(Default::default()),
                "closure_raw_ptr",
            )
            .expect("failed to convert closure ptr");
        let ordinal_ptr = self
            .builder
            .build_struct_gep(self.closure_type(), closure_raw_ptr, 0, "closure_ordinal_ptr")
            .expect("failed to build closure ordinal ptr");
        let env_ptr_ptr = self
            .builder
            .build_struct_gep(self.closure_type(), closure_raw_ptr, 1, "closure_env_ptr_ptr")
            .expect("failed to build closure env ptr ptr");
        let ordinal = *self.function_ordinals.get(name).unwrap_or_else(|| {
            panic!("internal compiler error: validated function reference '{name}' has no ordinal")
        });
        self.builder
            .build_store(ordinal_ptr, self.i64_type.const_int(ordinal as u64, true))
            .expect("failed to store closure ordinal");
        self.builder.build_store(env_ptr_ptr, env_raw).expect("failed to store closure env ptr");
        CompiledValue {
            tag: self.i64_type.const_int(TAG_FUNCTION as u64, false),
            payload: closure_ptr,
        }
    }

    pub(super) fn resolve_named_value(
        &self,
        name: &str,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        if let Some(ptr) = vars.get(name) {
            self.load_compiled_value(*ptr, name)
        } else if let Some(&slot) = capture_slots.get(name) {
            self.load_value_from_env(env_ptr, slot, name)
        } else if self.function_ordinals.contains_key(name) {
            self.allocate_closure_for_function(name, vars, capture_slots, env_ptr, function)
        } else {
            unreachable!(
                "undefined variable should have been rejected before llvm codegen: {name}"
            );
        }
    }

    pub(super) fn box_compiled_value(
        &self,
        value: CompiledValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let box_fn = self.require_func("__box_value");
        self.build_boxed_call(box_fn, &[value.tag, value.payload], label)
    }

    pub(super) fn unbox_handle(&self, handle: IntValue<'ctx>, label: &str) -> CompiledValue<'ctx> {
        let ptr = self
            .builder
            .build_int_to_ptr(
                handle,
                self.context.ptr_type(Default::default()),
                &format!("{label}_ptr"),
            )
            .expect("failed to convert handle to pointer");
        let tag = self
            .builder
            .build_int_z_extend(
                self.build_value_tag_load(ptr, label),
                self.i64_type,
                &format!("{label}_tag_i64"),
            )
            .expect("failed to extend unboxed tag");
        let payload = self.build_value_payload_load(ptr, label);
        CompiledValue { tag, payload }
    }

    pub(super) fn value_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context.struct_type(
            &[
                self.context.i8_type().into(),
                self.context.i8_type().array_type(7).into(),
                self.i64_type.into(),
            ],
            false,
        )
    }

    pub(super) fn closure_type(&self) -> inkwell::types::StructType<'ctx> {
        self.context.struct_type(&[self.i64_type.into(), self.i64_type.into()], false)
    }

    pub(super) fn build_value_tag_load(
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

    pub(super) fn build_value_payload_load(
        &self,
        value_ptr: PointerValue<'ctx>,
        label: &str,
    ) -> IntValue<'ctx> {
        let payload_ptr = self
            .builder
            .build_struct_gep(self.value_type(), value_ptr, 2, &format!("{label}_payload_ptr"))
            .expect("failed to build value payload gep");
        self.builder
            .build_load(self.i64_type, payload_ptr, &format!("{label}_payload"))
            .expect("failed to load value payload")
            .into_int_value()
    }
}
