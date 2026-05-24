use super::*;
#[cfg(feature = "wasi")]
use inkwell::attributes::AttributeLoc;

impl<'ctx> LlvmCompiler<'ctx> {
    #[cfg(feature = "wasi")]
    pub(super) fn declare_wasi_preview1_import(&mut self, name: &str, import_name: &str) {
        let i32_type = self.context.i32_type();
        let function = match import_name {
            "fd_write" => self.module.add_function(
                name,
                i32_type.fn_type(
                    &[i32_type.into(), i32_type.into(), i32_type.into(), i32_type.into()],
                    false,
                ),
                None,
            ),
            "args_sizes_get" => self.module.add_function(
                name,
                i32_type.fn_type(
                    &[
                        self.context.ptr_type(Default::default()).into(),
                        self.context.ptr_type(Default::default()).into(),
                    ],
                    false,
                ),
                None,
            ),
            "args_get" => self.module.add_function(
                name,
                i32_type.fn_type(
                    &[
                        self.context.ptr_type(Default::default()).into(),
                        self.context.ptr_type(Default::default()).into(),
                    ],
                    false,
                ),
                None,
            ),
            "proc_exit" => self.module.add_function(
                name,
                self.context.void_type().fn_type(&[i32_type.into()], false),
                None,
            ),
            other => panic!("unsupported WASI Preview 1 import: {other}"),
        };

        let import_module =
            self.context.create_string_attribute("wasm-import-module", "wasi_snapshot_preview1");
        let import_name_attr =
            self.context.create_string_attribute("wasm-import-name", import_name);
        function.add_attribute(AttributeLoc::Function, import_module);
        function.add_attribute(AttributeLoc::Function, import_name_attr);
        self.functions.insert(name.to_string(), function);
    }

    #[cfg(feature = "wasi")]
    pub(super) fn define_wasi_preview1_command_start_wrapper(&self) {
        let _ = self
            .functions
            .get("main")
            .copied()
            .expect("missing main function for wasi command wrapper");
        let int_wrapper_name = int_result_symbol_name("main", LlvmOutputMode::WasiPreview1Command);
        let int_wrapper = self
            .module
            .get_function(&int_wrapper_name)
            .unwrap_or_else(|| panic!("missing int-result wrapper: {int_wrapper_name}"));
        let function = self.module.add_function(
            "_start",
            self.context.void_type().fn_type(&[], false),
            Some(Linkage::External),
        );
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let exit_code_i64 = self
            .builder
            .build_call(int_wrapper, &[], "wasi_main_exit_code")
            .expect("failed to call int-result wrapper")
            .try_as_basic_value()
            .unwrap_basic()
            .into_int_value();
        let exit_code = self
            .builder
            .build_int_truncate(exit_code_i64, self.context.i32_type(), "wasi_main_exit_code_i32")
            .expect("failed to truncate exit code");
        let proc_exit = self.require_func("__wasi_proc_exit");
        self.builder
            .build_call(proc_exit, &[exit_code.into()], "wasi_proc_exit")
            .expect("failed to call proc_exit");
        self.builder.build_return(None).expect("failed to return from _start");
    }
    #[cfg(feature = "wasi")]
    pub(super) fn define_wasi_preview1_main_int_result_wrapper(&self, func_def: &FunctionDefAst) {
        assert!(
            func_def.inputs.len() <= 1,
            "wasi command main function supports at most one argument"
        );

        let symbol = int_result_symbol_name(&func_def.name, LlvmOutputMode::WasiPreview1Command);
        let function = self.module.add_function(
            &symbol,
            self.i64_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        let internal = self.require_func(&func_def.name);
        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let args_value = if func_def.inputs.len() == 1 {
            Some(self.build_wasi_preview1_args_list(function))
        } else {
            None
        };
        let call_args = args_value.as_ref().map_or_else(Vec::new, |value| vec![*value]);
        let value = self.build_user_call(
            internal,
            self.i64_type.const_zero(),
            &call_args,
            "wasi_main_value",
        );
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                value.tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "wasi_main_is_int",
            )
            .expect("failed to compare wasi main result tag");
        self.builder
            .build_conditional_branch(is_int, ok_block, trap_block)
            .expect("failed to branch on wasi main result tag");

        self.builder.position_at_end(ok_block);
        self.builder
            .build_return(Some(&value.payload))
            .expect("failed to build wasi main int-result return");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    #[cfg(feature = "wasi")]
    pub(super) fn build_wasi_errno_check(
        &self,
        errno: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
        label: &str,
    ) {
        let ok_block = self.context.append_basic_block(function, &format!("{label}_ok"));
        let trap_block = self.context.append_basic_block(function, &format!("{label}_trap"));
        let is_ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                errno,
                self.context.i32_type().const_zero(),
                &format!("{label}_is_ok"),
            )
            .expect("failed to compare wasi errno");
        self.builder
            .build_conditional_branch(is_ok, ok_block, trap_block)
            .expect("failed to branch on wasi errno");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
        self.builder.position_at_end(ok_block);
    }
    #[cfg(feature = "wasi")]
    pub(super) fn build_wasi_preview1_args_list(
        &self,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(Default::default());
        let alloc = self.require_func("__alloc");
        let args_sizes_get = self.require_func("__wasi_args_sizes_get");
        let args_get = self.require_func("__wasi_args_get");
        let list_new = self.require_func("__rt_list_new");
        let list_push = self.require_func("__rt_list_push");

        let argc_ptr = self
            .builder
            .build_alloca(i32_type, "wasi_argc_ptr")
            .expect("failed to allocate wasi argc ptr");
        let buf_size_ptr = self
            .builder
            .build_alloca(i32_type, "wasi_buf_size_ptr")
            .expect("failed to allocate wasi buf size ptr");
        let errno = self
            .builder
            .build_call(
                args_sizes_get,
                &[argc_ptr.into(), buf_size_ptr.into()],
                "wasi_args_sizes_get",
            )
            .expect("failed to call wasi args_sizes_get")
            .try_as_basic_value()
            .unwrap_basic()
            .into_int_value();
        self.build_wasi_errno_check(errno, function, "wasi_args_sizes_get");

        let argc32 = self
            .builder
            .build_load(i32_type, argc_ptr, "wasi_argc")
            .expect("failed to load wasi argc")
            .into_int_value();
        let buf_size32 = self
            .builder
            .build_load(i32_type, buf_size_ptr, "wasi_buf_size")
            .expect("failed to load wasi buf size")
            .into_int_value();
        let argc64 = self
            .builder
            .build_int_z_extend(argc32, self.i64_type, "wasi_argc64")
            .expect("failed to zext wasi argc");
        let buf_size64 = self
            .builder
            .build_int_z_extend(buf_size32, self.i64_type, "wasi_buf_size64")
            .expect("failed to zext wasi buf size");
        let argv_ptr_bytes = self
            .builder
            .build_int_mul(argc64, self.i64_type.const_int(4, false), "wasi_argv_ptr_bytes")
            .expect("failed to compute wasi argv ptr bytes");
        let argv_raw = self.build_boxed_call(
            alloc,
            &[argv_ptr_bytes, self.i64_type.const_int(4, false)],
            "wasi_argv_alloc",
        );
        let argv_buf_raw = self.build_boxed_call(
            alloc,
            &[buf_size64, self.i64_type.const_int(1, false)],
            "wasi_argv_buf_alloc",
        );
        let argv_ptr = self
            .builder
            .build_int_to_ptr(argv_raw, ptr_type, "wasi_argv_ptr")
            .expect("failed to convert wasi argv ptr");
        let argv_buf_ptr = self
            .builder
            .build_int_to_ptr(argv_buf_raw, ptr_type, "wasi_argv_buf_ptr")
            .expect("failed to convert wasi argv buf ptr");
        let errno = self
            .builder
            .build_call(args_get, &[argv_ptr.into(), argv_buf_ptr.into()], "wasi_args_get")
            .expect("failed to call wasi args_get")
            .try_as_basic_value()
            .unwrap_basic()
            .into_int_value();
        self.build_wasi_errno_check(errno, function, "wasi_args_get");

        let list = self.build_internal_call(list_new, &[], "wasi_args_list");
        let index_ptr = self
            .builder
            .build_alloca(i32_type, "wasi_args_index")
            .expect("failed to allocate wasi args index");
        self.builder
            .build_store(index_ptr, i32_type.const_int(1, false))
            .expect("failed to initialize wasi args index");

        let loop_check = self.context.append_basic_block(function, "wasi_args_loop_check");
        let loop_body = self.context.append_basic_block(function, "wasi_args_loop_body");
        let loop_done = self.context.append_basic_block(function, "wasi_args_loop_done");
        self.builder
            .build_unconditional_branch(loop_check)
            .expect("failed to branch to wasi args loop");

        self.builder.position_at_end(loop_check);
        let index32 = self
            .builder
            .build_load(i32_type, index_ptr, "wasi_args_index_load")
            .expect("failed to load wasi args index")
            .into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, index32, argc32, "wasi_args_more")
            .expect("failed to compare wasi args index");
        self.builder
            .build_conditional_branch(more, loop_body, loop_done)
            .expect("failed to branch wasi args loop");

        self.builder.position_at_end(loop_body);
        let argv_entry_ptr = unsafe {
            self.builder
                .build_gep(ptr_type, argv_ptr, &[index32], "wasi_argv_entry_ptr")
                .expect("failed to build wasi argv entry gep")
        };
        let arg_ptr = self
            .builder
            .build_load(ptr_type, argv_entry_ptr, "wasi_arg_ptr")
            .expect("failed to load wasi arg ptr")
            .into_pointer_value();

        let len_ptr = self
            .builder
            .build_alloca(i32_type, "wasi_arg_len_ptr")
            .expect("failed to allocate wasi arg len ptr");
        self.builder
            .build_store(len_ptr, i32_type.const_zero())
            .expect("failed to init wasi arg len");
        let len_check = self.context.append_basic_block(function, "wasi_arg_len_check");
        let len_body = self.context.append_basic_block(function, "wasi_arg_len_body");
        let len_done = self.context.append_basic_block(function, "wasi_arg_len_done");
        self.builder
            .build_unconditional_branch(len_check)
            .expect("failed to branch to wasi arg len loop");

        self.builder.position_at_end(len_check);
        let len32 = self
            .builder
            .build_load(i32_type, len_ptr, "wasi_arg_len")
            .expect("failed to load wasi arg len")
            .into_int_value();
        let byte_ptr = unsafe {
            self.builder
                .build_gep(self.context.i8_type(), arg_ptr, &[len32], "wasi_arg_byte_ptr")
                .expect("failed to build wasi arg byte ptr")
        };
        let byte = self
            .builder
            .build_load(self.context.i8_type(), byte_ptr, "wasi_arg_byte")
            .expect("failed to load wasi arg byte")
            .into_int_value();
        let is_zero = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                byte,
                self.context.i8_type().const_zero(),
                "wasi_arg_byte_is_zero",
            )
            .expect("failed to compare wasi arg byte");
        self.builder
            .build_conditional_branch(is_zero, len_done, len_body)
            .expect("failed to branch wasi arg len loop");

        self.builder.position_at_end(len_body);
        let next_len = self
            .builder
            .build_int_add(len32, i32_type.const_int(1, false), "wasi_arg_len_next")
            .expect("failed to increment wasi arg len");
        self.builder.build_store(len_ptr, next_len).expect("failed to store wasi arg len next");
        self.builder
            .build_unconditional_branch(len_check)
            .expect("failed to branch back to wasi arg len check");

        self.builder.position_at_end(len_done);
        let len32 = self
            .builder
            .build_load(i32_type, len_ptr, "wasi_arg_len_final")
            .expect("failed to reload wasi arg len")
            .into_int_value();
        let len64 = self
            .builder
            .build_int_z_extend(len32, self.i64_type, "wasi_arg_len64")
            .expect("failed to zext wasi arg len");
        let data_raw = self.build_boxed_call(
            alloc,
            &[len64, self.i64_type.const_int(1, false)],
            "wasi_arg_data_alloc",
        );
        let data_ptr = self
            .builder
            .build_int_to_ptr(data_raw, ptr_type, "wasi_arg_data_ptr")
            .expect("failed to convert wasi arg data ptr");
        self.build_copy_bytes_loop(arg_ptr, data_ptr, len64, function, "wasi_arg_copy");
        let string = self.build_string_header_from_parts(data_ptr, len64, "wasi_arg_string");
        let list_value = CompiledValue { tag: list.tag, payload: list.payload };
        let _ = self.build_internal_call(list_push, &[list_value, string], "wasi_args_push");

        let next_index = self
            .builder
            .build_int_add(index32, i32_type.const_int(1, false), "wasi_args_index_next")
            .expect("failed to increment wasi args index");
        self.builder.build_store(index_ptr, next_index).expect("failed to store wasi args index");
        self.builder
            .build_unconditional_branch(loop_check)
            .expect("failed to branch back to wasi args loop");

        self.builder.position_at_end(loop_done);
        list
    }

    #[cfg(feature = "wasi")]
    pub(super) fn define_wasi_preview1_print_runtime(&mut self) {
        let ptr_type = self.context.ptr_type(Default::default());
        let void_type = self.context.void_type();

        let write_bytes = self.module.add_function(
            "llvm_wasi_write_bytes",
            void_type.fn_type(&[ptr_type.into(), self.context.i32_type().into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert("__wasi_write_bytes".to_string(), write_bytes);

        let write_i64 = self.module.add_function(
            "llvm_wasi_write_i64",
            void_type.fn_type(&[self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert("__wasi_write_i64".to_string(), write_i64);

        let write_value = self.module.add_function(
            "llvm_wasi_write_value",
            void_type.fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert("__wasi_write_value".to_string(), write_value);

        let write_bigint = self.module.add_function(
            "llvm_wasi_write_bigint",
            void_type.fn_type(&[self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert("__wasi_write_bigint".to_string(), write_bigint);

        let write_list = self.module.add_function(
            "llvm_wasi_write_list",
            void_type.fn_type(&[self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert("__wasi_write_list".to_string(), write_list);

        let write_string = self.module.add_function(
            "llvm_wasi_write_string",
            void_type.fn_type(&[self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert("__wasi_write_string".to_string(), write_string);

        self.define_wasi_write_bytes_body(write_bytes);
        self.define_wasi_write_i64_body(write_i64);
        self.define_wasi_write_value_body(write_value);
        self.define_wasi_write_bigint_body(write_bigint);
        self.define_wasi_write_list_body(write_list);
        self.define_wasi_write_string_body(write_string);
        self.define_wasi_preview1_pair_print_wrapper("__rt_print", "llvm_rt_print");
        self.define_wasi_preview1_pair_print_wrapper("__rt_list_print", "llvm_rt_list_print");
    }
    #[cfg(feature = "wasi")]
    pub(super) fn define_wasi_preview1_pair_print_wrapper(&mut self, name: &str, symbol: &str) {
        let function = self.module.add_function(
            symbol,
            self.pair_type().fn_type(&[self.i64_type.into(), self.i64_type.into()], false),
            Some(Linkage::Private),
        );
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let tag = function.get_first_param().unwrap().into_int_value();
        let payload = function.get_nth_param(1).unwrap().into_int_value();
        let write_value = self.require_func("__wasi_write_value");
        self.builder
            .build_call(write_value, &[tag.into(), payload.into()], "wasi_write_value")
            .expect("failed to call preview1 write_value");
        self.build_wasi_write_const("__wasi_newline", b"\n", "preview1_newline");
        self.builder
            .build_return(Some(&self.make_pair_value(
                self.i64_type.const_int(TAG_INT as u64, false),
                self.i64_type.const_zero(),
                &format!("{symbol}_result"),
            )))
            .expect("failed to return preview1 print wrapper");
    }

    #[cfg(feature = "wasi")]
    pub(super) fn define_wasi_write_bytes_body(&self, function: FunctionValue<'ctx>) {
        let entry = self.context.append_basic_block(function, "entry");
        let ok_block = self.context.append_basic_block(function, "ok");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let ptr = function.get_first_param().unwrap().into_pointer_value();
        let len = function.get_nth_param(1).unwrap().into_int_value();
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(Default::default());
        let iovec_type = self.context.struct_type(&[ptr_type.into(), i32_type.into()], false);
        let iovec_ptr = self
            .builder
            .build_alloca(iovec_type, "wasi_iovec")
            .expect("failed to allocate wasi iovec");
        let nwritten_ptr = self
            .builder
            .build_alloca(i32_type, "wasi_nwritten")
            .expect("failed to allocate wasi nwritten");
        let buf_ptr = self
            .builder
            .build_struct_gep(iovec_type, iovec_ptr, 0, "wasi_iovec_buf")
            .expect("failed to build iovec buf gep");
        let len_ptr = self
            .builder
            .build_struct_gep(iovec_type, iovec_ptr, 1, "wasi_iovec_len")
            .expect("failed to build iovec len gep");
        self.builder.build_store(buf_ptr, ptr).expect("failed to store iovec buf");
        self.builder.build_store(len_ptr, len).expect("failed to store iovec len");

        let fd_write = self.require_func("__wasi_fd_write");
        let iovec_raw = self
            .builder
            .build_ptr_to_int(iovec_ptr, i32_type, "wasi_iovec_raw")
            .expect("failed to convert iovec ptr");
        let nwritten_raw = self
            .builder
            .build_ptr_to_int(nwritten_ptr, i32_type, "wasi_nwritten_raw")
            .expect("failed to convert nwritten ptr");
        let status = self
            .builder
            .build_call(
                fd_write,
                &[
                    i32_type.const_int(1, false).into(),
                    iovec_raw.into(),
                    i32_type.const_int(1, false).into(),
                    nwritten_raw.into(),
                ],
                "wasi_fd_write",
            )
            .expect("failed to call fd_write")
            .try_as_basic_value()
            .unwrap_basic()
            .into_int_value();
        let success = self
            .builder
            .build_int_compare(IntPredicate::EQ, status, i32_type.const_zero(), "wasi_fd_write_ok")
            .expect("failed to compare fd_write status");
        self.builder
            .build_conditional_branch(success, ok_block, trap_block)
            .expect("failed to branch on fd_write status");

        self.builder.position_at_end(ok_block);
        self.builder.build_return(None).expect("failed to return from write_bytes");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    #[cfg(feature = "wasi")]
    pub(super) fn define_wasi_write_i64_body(&self, function: FunctionValue<'ctx>) {
        let entry = self.context.append_basic_block(function, "entry");
        let zero_block = self.context.append_basic_block(function, "zero");
        let non_zero_block = self.context.append_basic_block(function, "non_zero");
        let sign_check_block = self.context.append_basic_block(function, "sign_check");
        let loop_block = self.context.append_basic_block(function, "loop");
        let done_block = self.context.append_basic_block(function, "done");
        self.builder.position_at_end(entry);

        let value = function.get_first_param().unwrap().into_int_value();
        let zero = self.i64_type.const_zero();
        let is_zero = self
            .builder
            .build_int_compare(IntPredicate::EQ, value, zero, "wasi_i64_is_zero")
            .expect("failed to compare i64 zero");
        self.builder
            .build_conditional_branch(is_zero, zero_block, non_zero_block)
            .expect("failed to branch on i64 zero");

        self.builder.position_at_end(zero_block);
        self.build_wasi_write_const("__wasi_digit_zero", b"0", "wasi_zero");
        self.builder.build_return(None).expect("failed to return from zero i64 writer");

        self.builder.position_at_end(non_zero_block);
        let buffer_type = self.context.i8_type().array_type(32);
        let buffer = self
            .builder
            .build_alloca(buffer_type, "wasi_i64_buffer")
            .expect("failed to allocate i64 buffer");
        let idx_ptr = self
            .builder
            .build_alloca(self.context.i32_type(), "wasi_i64_idx")
            .expect("failed to allocate i64 idx");
        let current_ptr = self
            .builder
            .build_alloca(self.i64_type, "wasi_i64_current")
            .expect("failed to allocate i64 current");
        self.builder
            .build_store(idx_ptr, self.context.i32_type().const_int(32, false))
            .expect("failed to store initial i64 idx");
        self.builder
            .build_unconditional_branch(sign_check_block)
            .expect("failed to branch to sign check");

        self.builder.position_at_end(sign_check_block);
        let is_negative = self
            .builder
            .build_int_compare(IntPredicate::SLT, value, zero, "wasi_i64_is_negative")
            .expect("failed to compare i64 sign");
        let neg_block = self.context.append_basic_block(function, "negative");
        let pos_block = self.context.append_basic_block(function, "positive");
        self.builder
            .build_conditional_branch(is_negative, neg_block, pos_block)
            .expect("failed to branch on i64 sign");

        self.builder.position_at_end(neg_block);
        self.build_wasi_write_const("__wasi_minus", b"-", "wasi_minus");
        let magnitude = self
            .builder
            .build_int_sub(zero, value, "wasi_i64_magnitude")
            .expect("failed to compute i64 magnitude");
        self.builder
            .build_store(current_ptr, magnitude)
            .expect("failed to store negative magnitude");
        self.builder.build_unconditional_branch(loop_block).expect("failed to branch to i64 loop");

        self.builder.position_at_end(pos_block);
        self.builder.build_store(current_ptr, value).expect("failed to store positive i64");
        self.builder.build_unconditional_branch(loop_block).expect("failed to branch to i64 loop");

        self.builder.position_at_end(loop_block);
        let current = self
            .builder
            .build_load(self.i64_type, current_ptr, "wasi_i64_current_load")
            .expect("failed to load current i64")
            .into_int_value();
        let quotient = self
            .builder
            .build_int_unsigned_div(
                current,
                self.i64_type.const_int(10, false),
                "wasi_i64_quotient",
            )
            .expect("failed to divide current i64");
        let remainder = self
            .builder
            .build_int_unsigned_rem(
                current,
                self.i64_type.const_int(10, false),
                "wasi_i64_remainder",
            )
            .expect("failed to mod current i64");
        let idx = self
            .builder
            .build_load(self.context.i32_type(), idx_ptr, "wasi_i64_idx_load")
            .expect("failed to load i64 idx")
            .into_int_value();
        let next_idx = self
            .builder
            .build_int_sub(idx, self.context.i32_type().const_int(1, false), "wasi_i64_next_idx")
            .expect("failed to decrement i64 idx");
        self.builder.build_store(idx_ptr, next_idx).expect("failed to store next i64 idx");
        let digit = self
            .builder
            .build_int_add(
                self.builder
                    .build_int_truncate(remainder, self.context.i8_type(), "wasi_i64_digit_raw")
                    .expect("failed to truncate digit"),
                self.context.i8_type().const_int(b'0' as u64, false),
                "wasi_i64_digit",
            )
            .expect("failed to build digit");
        let zero32 = self.context.i32_type().const_zero();
        let digit_ptr = unsafe {
            self.builder
                .build_gep(buffer_type, buffer, &[zero32, next_idx], "wasi_i64_digit_ptr")
                .expect("failed to build digit ptr")
        };
        self.builder.build_store(digit_ptr, digit).expect("failed to store digit");
        self.builder.build_store(current_ptr, quotient).expect("failed to store quotient");
        let more = self
            .builder
            .build_int_compare(IntPredicate::NE, quotient, zero, "wasi_i64_more_digits")
            .expect("failed to compare quotient");
        self.builder
            .build_conditional_branch(more, loop_block, done_block)
            .expect("failed to branch in i64 loop");

        self.builder.position_at_end(done_block);
        let final_idx = self
            .builder
            .build_load(self.context.i32_type(), idx_ptr, "wasi_i64_final_idx")
            .expect("failed to load final i64 idx")
            .into_int_value();
        let start_ptr = unsafe {
            self.builder
                .build_gep(buffer_type, buffer, &[zero32, final_idx], "wasi_i64_start_ptr")
                .expect("failed to build start ptr")
        };
        let len = self
            .builder
            .build_int_sub(self.context.i32_type().const_int(32, false), final_idx, "wasi_i64_len")
            .expect("failed to compute i64 len");
        let write_bytes = self.require_func("__wasi_write_bytes");
        self.builder
            .build_call(write_bytes, &[start_ptr.into(), len.into()], "wasi_write_digits")
            .expect("failed to write digit buffer");
        self.builder.build_return(None).expect("failed to return from i64 writer");
    }

    #[cfg(feature = "wasi")]
    pub(super) fn define_wasi_write_value_body(&self, function: FunctionValue<'ctx>) {
        let entry = self.context.append_basic_block(function, "entry");
        let int_block = self.context.append_basic_block(function, "int");
        let list_block = self.context.append_basic_block(function, "list");
        let bigint_block = self.context.append_basic_block(function, "bigint");
        let string_block = self.context.append_basic_block(function, "string");
        let trap_block = self.context.append_basic_block(function, "trap");
        self.builder.position_at_end(entry);

        let tag = function.get_first_param().unwrap().into_int_value();
        let payload = function.get_nth_param(1).unwrap().into_int_value();
        let is_int = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                self.i64_type.const_int(TAG_INT as u64, false),
                "wasi_value_is_int",
            )
            .expect("failed to compare value tag int");
        let tag_dispatch = self.context.append_basic_block(function, "dispatch");
        self.builder
            .build_conditional_branch(is_int, int_block, tag_dispatch)
            .expect("failed to branch on int tag");

        self.builder.position_at_end(tag_dispatch);
        let is_list = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                self.i64_type.const_int(TAG_LIST as u64, false),
                "wasi_value_is_list",
            )
            .expect("failed to compare value tag list");
        let bigint_tag = self.i64_type.const_int(TAG_BIGINT as u64, false);
        let is_bigint = self
            .builder
            .build_int_compare(IntPredicate::EQ, tag, bigint_tag, "wasi_value_is_bigint")
            .expect("failed to compare value tag bigint");
        let string_tag = self.i64_type.const_int(TAG_STRING as u64, false);
        let is_string = self
            .builder
            .build_int_compare(IntPredicate::EQ, tag, string_tag, "wasi_value_is_string")
            .expect("failed to compare value tag string");
        let after_list = self.context.append_basic_block(function, "after_list");
        self.builder
            .build_conditional_branch(is_list, list_block, after_list)
            .expect("failed to branch on list tag");

        self.builder.position_at_end(after_list);
        let after_bigint = self.context.append_basic_block(function, "after_bigint");
        self.builder
            .build_conditional_branch(is_bigint, bigint_block, after_bigint)
            .expect("failed to branch on bigint tag");

        self.builder.position_at_end(after_bigint);
        self.builder
            .build_conditional_branch(is_string, string_block, trap_block)
            .expect("failed to branch on string tag");

        self.builder.position_at_end(int_block);
        let write_i64 = self.require_func("__wasi_write_i64");
        self.builder
            .build_call(write_i64, &[payload.into()], "wasi_write_i64")
            .expect("failed to call write_i64");
        self.builder.build_return(None).expect("failed to return from value int writer");

        self.builder.position_at_end(list_block);
        let write_list = self.require_func("__wasi_write_list");
        self.builder
            .build_call(write_list, &[payload.into()], "wasi_write_list")
            .expect("failed to call write_list");
        self.builder.build_return(None).expect("failed to return from value list writer");

        self.builder.position_at_end(bigint_block);
        let write_bigint = self.require_func("__wasi_write_bigint");
        self.builder
            .build_call(write_bigint, &[payload.into()], "wasi_write_bigint")
            .expect("failed to call write_bigint");
        self.builder.build_return(None).expect("failed to return from value bigint writer");

        self.builder.position_at_end(string_block);
        let write_string = self.require_func("__wasi_write_string");
        self.builder
            .build_call(write_string, &[payload.into()], "wasi_write_string")
            .expect("failed to call write_string");
        self.builder.build_return(None).expect("failed to return from value string writer");

        self.builder.position_at_end(trap_block);
        self.build_trap_and_unreachable();
    }

    #[cfg(feature = "wasi")]
    pub(super) fn define_wasi_write_list_body(&self, function: FunctionValue<'ctx>) {
        let entry = self.context.append_basic_block(function, "entry");
        let loop_check = self.context.append_basic_block(function, "loop_check");
        let loop_body = self.context.append_basic_block(function, "loop_body");
        let separator_block = self.context.append_basic_block(function, "separator");
        let element_block = self.context.append_basic_block(function, "element");
        let done_block = self.context.append_basic_block(function, "done");
        self.builder.position_at_end(entry);

        let payload = function.get_first_param().unwrap().into_int_value();
        self.build_wasi_write_const("__wasi_list_open", b"[", "wasi_list_open");
        let len = self.build_list_len_load(payload, "wasi_list_len");
        let idx_ptr = self
            .builder
            .build_alloca(self.i64_type, "wasi_list_idx")
            .expect("failed to allocate list idx");
        self.builder
            .build_store(idx_ptr, self.i64_type.const_zero())
            .expect("failed to init list idx");
        self.builder.build_unconditional_branch(loop_check).expect("failed to branch to list loop");

        self.builder.position_at_end(loop_check);
        let idx = self
            .builder
            .build_load(self.i64_type, idx_ptr, "wasi_list_idx_load")
            .expect("failed to load list idx")
            .into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::ULT, idx, len, "wasi_list_more")
            .expect("failed to compare list idx");
        self.builder
            .build_conditional_branch(more, loop_body, done_block)
            .expect("failed to branch on list idx");

        self.builder.position_at_end(loop_body);
        let is_first = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                idx,
                self.i64_type.const_zero(),
                "wasi_list_is_first",
            )
            .expect("failed to compare first list element");
        self.builder
            .build_conditional_branch(is_first, element_block, separator_block)
            .expect("failed to branch on first list element");

        self.builder.position_at_end(separator_block);
        self.build_wasi_write_const("__wasi_list_separator", b", ", "wasi_list_sep");
        self.builder
            .build_unconditional_branch(element_block)
            .expect("failed to branch to list element");

        self.builder.position_at_end(element_block);
        let value = self.build_list_value_load(payload, idx, "wasi_list_value");
        let write_value = self.require_func("__wasi_write_value");
        self.builder
            .build_call(
                write_value,
                &[value.tag.into(), value.payload.into()],
                "wasi_list_write_value",
            )
            .expect("failed to write list element");
        let next = self
            .builder
            .build_int_add(idx, self.i64_type.const_int(1, false), "wasi_list_next")
            .expect("failed to increment list idx");
        self.builder.build_store(idx_ptr, next).expect("failed to store next list idx");
        self.builder.build_unconditional_branch(loop_check).expect("failed to loop over list");

        self.builder.position_at_end(done_block);
        self.build_wasi_write_const("__wasi_list_close", b"]", "wasi_list_close");
        self.builder.build_return(None).expect("failed to return from list writer");
    }

    #[cfg(feature = "wasi")]
    pub(super) fn define_wasi_write_string_body(&self, function: FunctionValue<'ctx>) {
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let payload = function.get_first_param().unwrap().into_int_value();
        let len = self.build_string_len_load(payload, "wasi_string");
        let len32 = self
            .builder
            .build_int_truncate(len, self.context.i32_type(), "wasi_string_len32")
            .expect("failed to truncate string len");
        let data_ptr = self.build_string_ptr_load(payload, "wasi_string");
        let write_bytes = self.require_func("__wasi_write_bytes");
        self.builder
            .build_call(write_bytes, &[data_ptr.into(), len32.into()], "wasi_write_string_bytes")
            .expect("failed to write string bytes");
        self.builder.build_return(None).expect("failed to return from string writer");
    }

    #[cfg(feature = "wasi")]
    pub(super) fn define_wasi_write_bigint_body(&self, function: FunctionValue<'ctx>) {
        let entry = self.context.append_basic_block(function, "entry");
        let zero_block = self.context.append_basic_block(function, "zero");
        let non_zero_block = self.context.append_basic_block(function, "non_zero");
        let sign_block = self.context.append_basic_block(function, "sign");
        let digit_loop_check = self.context.append_basic_block(function, "digit_loop_check");
        let digit_loop_body = self.context.append_basic_block(function, "digit_loop_body");
        let digit_loop_done = self.context.append_basic_block(function, "digit_loop_done");
        let limb_loop_check = self.context.append_basic_block(function, "limb_loop_check");
        let limb_loop_body = self.context.append_basic_block(function, "limb_loop_body");
        let limb_loop_done = self.context.append_basic_block(function, "limb_loop_done");
        let write_block = self.context.append_basic_block(function, "write");
        self.builder.position_at_end(entry);

        let payload = function.get_first_param().unwrap().into_int_value();
        let sign = self.build_bigint_sign_load(payload, "wasi_bigint_sign");
        let len = self.build_bigint_len_load(payload, "wasi_bigint_len");
        let is_zero_sign = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sign,
                self.i64_type.const_zero(),
                "wasi_bigint_sign_zero",
            )
            .expect("failed to compare bigint sign to zero");
        let is_zero_len = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                len,
                self.i64_type.const_zero(),
                "wasi_bigint_len_zero",
            )
            .expect("failed to compare bigint len to zero");
        let is_zero = self
            .builder
            .build_or(is_zero_sign, is_zero_len, "wasi_bigint_is_zero")
            .expect("failed to combine bigint zero checks");
        self.builder
            .build_conditional_branch(is_zero, zero_block, non_zero_block)
            .expect("failed to branch on bigint zero");

        self.builder.position_at_end(zero_block);
        self.build_wasi_write_const("__wasi_bigint_zero", b"0", "wasi_bigint_zero");
        self.builder.build_return(None).expect("failed to return from zero bigint writer");

        self.builder.position_at_end(non_zero_block);
        let alloc = self.require_func("__alloc");
        let temp_payload = self.build_bigint_alloc(len, "wasi_bigint_temp");
        self.build_bigint_sign_store(temp_payload, sign, "wasi_bigint_temp_sign");
        self.build_bigint_len_store(temp_payload, len, "wasi_bigint_temp_len");

        let copy_idx_ptr = self
            .builder
            .build_alloca(self.i64_type, "wasi_bigint_copy_idx")
            .expect("failed to allocate bigint copy idx");
        let rem_ptr = self
            .builder
            .build_alloca(self.i64_type, "wasi_bigint_rem")
            .expect("failed to allocate bigint rem");
        let digit_cap = self
            .builder
            .build_int_add(
                self.builder
                    .build_int_mul(len, self.i64_type.const_int(10, false), "wasi_bigint_digit_mul")
                    .expect("failed to compute bigint digit cap mul"),
                self.i64_type.const_int(1, false),
                "wasi_bigint_digit_cap",
            )
            .expect("failed to compute bigint digit cap");
        let digit_buf_raw = self.build_boxed_call(
            alloc,
            &[digit_cap, self.i64_type.const_int(1, false)],
            "wasi_bigint_digit_buf",
        );
        let digit_pos_ptr = self
            .builder
            .build_alloca(self.i64_type, "wasi_bigint_digit_pos")
            .expect("failed to allocate bigint digit pos");
        self.builder
            .build_store(copy_idx_ptr, self.i64_type.const_zero())
            .expect("failed to init bigint copy idx");
        self.builder
            .build_store(digit_pos_ptr, digit_cap)
            .expect("failed to init bigint digit pos");

        let copy_loop_check = self.context.append_basic_block(function, "copy_loop_check");
        let copy_loop_body = self.context.append_basic_block(function, "copy_loop_body");
        let copy_loop_done = self.context.append_basic_block(function, "copy_loop_done");
        self.builder
            .build_unconditional_branch(copy_loop_check)
            .expect("failed to branch to bigint copy loop");

        self.builder.position_at_end(copy_loop_check);
        let copy_idx = self
            .builder
            .build_load(self.i64_type, copy_idx_ptr, "wasi_bigint_copy_idx_load")
            .expect("failed to load bigint copy idx")
            .into_int_value();
        let copy_more = self
            .builder
            .build_int_compare(IntPredicate::ULT, copy_idx, len, "wasi_bigint_copy_more")
            .expect("failed to compare bigint copy idx");
        self.builder
            .build_conditional_branch(copy_more, copy_loop_body, copy_loop_done)
            .expect("failed to branch bigint copy loop");

        self.builder.position_at_end(copy_loop_body);
        let copied_limb = self.build_bigint_limb_load(payload, copy_idx, "wasi_bigint_copy_src");
        self.build_bigint_limb_store(temp_payload, copy_idx, copied_limb, "wasi_bigint_copy_dst");
        let copy_next = self
            .builder
            .build_int_add(copy_idx, self.i64_type.const_int(1, false), "wasi_bigint_copy_next")
            .expect("failed to increment bigint copy idx");
        self.builder
            .build_store(copy_idx_ptr, copy_next)
            .expect("failed to store bigint copy next");
        self.builder
            .build_unconditional_branch(copy_loop_check)
            .expect("failed to loop bigint copy");

        self.builder.position_at_end(copy_loop_done);
        self.builder
            .build_unconditional_branch(sign_block)
            .expect("failed to branch to bigint sign block");

        self.builder.position_at_end(sign_block);
        let is_negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                sign,
                self.i64_type.const_zero(),
                "wasi_bigint_is_negative",
            )
            .expect("failed to compare bigint sign");
        let neg_block = self.context.append_basic_block(function, "negative");
        let pos_block = self.context.append_basic_block(function, "positive");
        self.builder
            .build_conditional_branch(is_negative, neg_block, pos_block)
            .expect("failed to branch on bigint sign");

        self.builder.position_at_end(neg_block);
        self.build_wasi_write_const("__wasi_minus", b"-", "wasi_bigint_minus");
        self.builder
            .build_unconditional_branch(pos_block)
            .expect("failed to branch after bigint minus");

        self.builder.position_at_end(pos_block);
        self.builder
            .build_unconditional_branch(digit_loop_check)
            .expect("failed to branch to bigint digit loop");

        self.builder.position_at_end(digit_loop_check);
        let temp_len = self.build_bigint_len_load(temp_payload, "wasi_bigint_temp_len");
        let has_digits = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                temp_len,
                self.i64_type.const_zero(),
                "wasi_bigint_has_digits",
            )
            .expect("failed to compare bigint temp len");
        self.builder
            .build_conditional_branch(has_digits, digit_loop_body, digit_loop_done)
            .expect("failed to branch bigint digit loop");

        self.builder.position_at_end(digit_loop_body);
        self.builder
            .build_store(rem_ptr, self.i64_type.const_zero())
            .expect("failed to reset bigint remainder");
        self.builder
            .build_store(copy_idx_ptr, temp_len)
            .expect("failed to init bigint limb loop idx");
        self.builder
            .build_unconditional_branch(limb_loop_check)
            .expect("failed to branch to bigint limb loop");

        self.builder.position_at_end(limb_loop_check);
        let limb_remaining = self
            .builder
            .build_load(self.i64_type, copy_idx_ptr, "wasi_bigint_limb_remaining")
            .expect("failed to load bigint limb remaining")
            .into_int_value();
        let limb_more = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                limb_remaining,
                self.i64_type.const_zero(),
                "wasi_bigint_limb_more",
            )
            .expect("failed to compare bigint limb remaining");
        self.builder
            .build_conditional_branch(limb_more, limb_loop_body, limb_loop_done)
            .expect("failed to branch bigint limb loop");

        self.builder.position_at_end(limb_loop_body);
        let limb_idx = self
            .builder
            .build_int_sub(
                limb_remaining,
                self.i64_type.const_int(1, false),
                "wasi_bigint_limb_idx",
            )
            .expect("failed to decrement bigint limb idx");
        let limb = self.build_bigint_limb_load(temp_payload, limb_idx, "wasi_bigint_div_limb");
        let remainder = self
            .builder
            .build_load(self.i64_type, rem_ptr, "wasi_bigint_rem_load")
            .expect("failed to load bigint remainder")
            .into_int_value();
        let high = self
            .builder
            .build_left_shift(remainder, self.i64_type.const_int(32, false), "wasi_bigint_cur_high")
            .expect("failed to shift bigint remainder");
        let current = self
            .builder
            .build_int_add(high, limb, "wasi_bigint_cur")
            .expect("failed to build bigint current");
        let quotient = self
            .builder
            .build_int_unsigned_div(
                current,
                self.i64_type.const_int(10, false),
                "wasi_bigint_quotient",
            )
            .expect("failed to divide bigint current");
        let next_remainder = self
            .builder
            .build_int_unsigned_rem(
                current,
                self.i64_type.const_int(10, false),
                "wasi_bigint_remainder",
            )
            .expect("failed to mod bigint current");
        self.build_bigint_limb_store(temp_payload, limb_idx, quotient, "wasi_bigint_quot_store");
        self.builder
            .build_store(rem_ptr, next_remainder)
            .expect("failed to store bigint remainder");
        self.builder
            .build_store(copy_idx_ptr, limb_idx)
            .expect("failed to store next bigint limb idx");
        self.builder
            .build_unconditional_branch(limb_loop_check)
            .expect("failed to loop bigint limb division");

        self.builder.position_at_end(limb_loop_done);
        self.build_bigint_normalize(temp_payload, "wasi_bigint_norm");
        let digit_pos = self
            .builder
            .build_load(self.i64_type, digit_pos_ptr, "wasi_bigint_digit_pos_load")
            .expect("failed to load bigint digit pos")
            .into_int_value();
        let next_digit_pos = self
            .builder
            .build_int_sub(
                digit_pos,
                self.i64_type.const_int(1, false),
                "wasi_bigint_next_digit_pos",
            )
            .expect("failed to decrement bigint digit pos");
        self.builder
            .build_store(digit_pos_ptr, next_digit_pos)
            .expect("failed to store bigint next digit pos");
        let digit_addr = self
            .builder
            .build_int_add(digit_buf_raw, next_digit_pos, "wasi_bigint_digit_addr")
            .expect("failed to compute bigint digit addr");
        let digit_ptr = self
            .builder
            .build_int_to_ptr(
                digit_addr,
                self.context.ptr_type(Default::default()),
                "wasi_bigint_digit_ptr",
            )
            .expect("failed to convert bigint digit ptr");
        let digit = self
            .builder
            .build_int_add(
                self.builder
                    .build_int_truncate(
                        self.builder
                            .build_load(self.i64_type, rem_ptr, "wasi_bigint_digit_rem")
                            .expect("failed to reload bigint remainder")
                            .into_int_value(),
                        self.context.i8_type(),
                        "wasi_bigint_digit_raw",
                    )
                    .expect("failed to truncate bigint digit"),
                self.context.i8_type().const_int(b'0' as u64, false),
                "wasi_bigint_digit",
            )
            .expect("failed to build bigint digit");
        self.builder.build_store(digit_ptr, digit).expect("failed to store bigint digit");
        self.builder
            .build_unconditional_branch(digit_loop_check)
            .expect("failed to loop bigint digits");

        self.builder.position_at_end(digit_loop_done);
        self.builder
            .build_unconditional_branch(write_block)
            .expect("failed to branch to bigint write");

        self.builder.position_at_end(write_block);
        let final_digit_pos = self
            .builder
            .build_load(self.i64_type, digit_pos_ptr, "wasi_bigint_final_digit_pos")
            .expect("failed to load final bigint digit pos")
            .into_int_value();
        let write_addr = self
            .builder
            .build_int_add(digit_buf_raw, final_digit_pos, "wasi_bigint_write_addr")
            .expect("failed to compute bigint write addr");
        let write_ptr = self
            .builder
            .build_int_to_ptr(
                write_addr,
                self.context.ptr_type(Default::default()),
                "wasi_bigint_write_ptr",
            )
            .expect("failed to convert bigint write ptr");
        let remaining_len = self
            .builder
            .build_int_sub(digit_cap, final_digit_pos, "wasi_bigint_write_len")
            .expect("failed to compute bigint write len");
        let write_bytes = self.require_func("__wasi_write_bytes");
        let write_len_i32 = self
            .builder
            .build_int_truncate(remaining_len, self.context.i32_type(), "wasi_bigint_write_len_i32")
            .expect("failed to truncate bigint write len");
        self.builder
            .build_call(
                write_bytes,
                &[write_ptr.into(), write_len_i32.into()],
                "wasi_bigint_write_bytes",
            )
            .expect("failed to write bigint bytes");
        self.builder.build_return(None).expect("failed to return from bigint writer");
    }
}
