use super::*;

impl<'ctx> LlvmCompiler<'ctx> {
    pub(super) fn compile_ast(
        &self,
        ast: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        match ast {
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
            Ast::ListLiteral(items) => {
                self.compile_list_literal_ast(items, vars, capture_slots, env_ptr, function)
            }
            Ast::Index { collection, index } => {
                self.compile_index_ast(collection, index, vars, capture_slots, env_ptr, function)
            }
            Ast::IndexAssign { collection, index, value } => self.compile_index_assign_ast(
                collection,
                index,
                value,
                vars,
                capture_slots,
                env_ptr,
                function,
            ),
            Ast::Expression(ExpressionAst { function: name, args }) => {
                self.compile_expression_ast(name, args, vars, capture_slots, env_ptr, function)
            }
            Ast::Block(block) => {
                self.compile_block_ast(block, vars, capture_slots, env_ptr, function)
            }
            Ast::Variable(name) => {
                self.resolve_named_value(name, vars, capture_slots, env_ptr, function)
            }
            Ast::Assign { name, value } => {
                self.compile_assign_ast(name, value, vars, capture_slots, env_ptr, function)
            }
            Ast::If { condition, then, else_ } => {
                self.compile_if_ast(condition, then, else_, vars, capture_slots, env_ptr, function)
            }
            Ast::FunctionDef(_) => unimplemented!("nested function definitions"),
        }
    }

    fn compile_list_literal_ast(
        &self,
        items: &[Ast],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let list = self.build_internal_call(self.require_func("__rt_list_new"), &[], "list_new");
        for item in items {
            let value = self.compile_ast(item, vars, capture_slots, env_ptr, function);
            let _ = self.build_internal_call(
                self.require_func("__rt_list_push"),
                &[list, value],
                "list_push",
            );
        }
        list
    }

    fn compile_index_ast(
        &self,
        collection: &Ast,
        index: &Ast,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let collection = self.compile_ast(collection, vars, capture_slots, env_ptr, function);
        let index = self.compile_ast(index, vars, capture_slots, env_ptr, function);
        self.build_internal_call(
            self.require_func("__rt_list_get"),
            &[collection, index],
            "list_get",
        )
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
    ) -> CompiledValue<'ctx> {
        let collection = self.compile_ast(collection, vars, capture_slots, env_ptr, function);
        let index = self.compile_ast(index, vars, capture_slots, env_ptr, function);
        let value = self.compile_ast(value, vars, capture_slots, env_ptr, function);
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
    ) -> CompiledValue<'ctx> {
        if name == "not" {
            assert_eq!(args.len(), 1, "{name} expects 1 argument");
            return self.compile_logical_not(&args[0], vars, capture_slots, env_ptr, function);
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
            );
        }
        if name == "list_map" {
            return self.compile_list_map(args, vars, capture_slots, env_ptr, function);
        }
        if name == "list_filter" {
            return self.compile_list_filter(args, vars, capture_slots, env_ptr, function);
        }
        if name == "list_range" {
            return self.compile_list_range(args, vars, capture_slots, env_ptr, function);
        }
        if let Some(value) =
            self.compile_string_expression_ast(name, args, vars, capture_slots, env_ptr, function)
        {
            return value;
        }

        let compiled = args
            .iter()
            .map(|arg| self.compile_ast(arg, vars, capture_slots, env_ptr, function))
            .collect::<Vec<_>>();
        if name.is_empty() {
            return compiled[0];
        }
        self.compile_named_expression_ast(name, &compiled, vars, capture_slots, env_ptr, function)
    }

    fn compile_string_expression_ast(
        &self,
        name: &str,
        args: &[Ast],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> Option<CompiledValue<'ctx>> {
        match name {
            "bytes_len" => {
                assert_eq!(args.len(), 1, "bytes_len expects 1 argument");
                let value = self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                let trap_block = self.context.append_basic_block(function, "bytes_len_trap");
                let ok_block = self.context.append_basic_block(function, "bytes_len_ok");
                let raw =
                    self.expect_tag_payload(value, TAG_STRING, "bytes_len", ok_block, trap_block);
                self.builder.position_at_end(trap_block);
                self.build_trap_and_unreachable();
                self.builder.position_at_end(ok_block);
                let len = self.build_string_len_load(raw, "bytes_len");
                Some(self.int_value(len))
            }
            "bytes_get" => {
                assert_eq!(args.len(), 2, "bytes_get expects 2 arguments");
                let string_value =
                    self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                let index_value =
                    self.compile_ast(&args[1], vars, capture_slots, env_ptr, function);
                Some(self.build_bytes_get(string_value, index_value, function))
            }
            "bytes_pop" => {
                assert_eq!(args.len(), 1, "bytes_pop expects 1 argument");
                let string_value =
                    self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                Some(self.build_bytes_pop(string_value, function))
            }
            "bytes_push" => {
                assert_eq!(args.len(), 2, "bytes_push expects 2 arguments");
                let string_value =
                    self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                let byte_value = self.compile_ast(&args[1], vars, capture_slots, env_ptr, function);
                Some(self.build_bytes_push(string_value, byte_value, function))
            }
            "bytes_insert" => {
                assert_eq!(args.len(), 3, "bytes_insert expects 3 arguments");
                let string_value =
                    self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                let index_value =
                    self.compile_ast(&args[1], vars, capture_slots, env_ptr, function);
                let byte_value = self.compile_ast(&args[2], vars, capture_slots, env_ptr, function);
                Some(self.build_bytes_insert(string_value, index_value, byte_value, function))
            }
            "bytes_remove" => {
                assert_eq!(args.len(), 2, "bytes_remove expects 2 arguments");
                let string_value =
                    self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                let index_value =
                    self.compile_ast(&args[1], vars, capture_slots, env_ptr, function);
                Some(self.build_bytes_remove(string_value, index_value, function))
            }
            "bytes_set" => {
                assert_eq!(args.len(), 3, "bytes_set expects 3 arguments");
                let string_value =
                    self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                let index_value =
                    self.compile_ast(&args[1], vars, capture_slots, env_ptr, function);
                let byte_value = self.compile_ast(&args[2], vars, capture_slots, env_ptr, function);
                Some(self.build_bytes_set(string_value, index_value, byte_value, function))
            }
            "bytes_slice" => {
                assert_eq!(args.len(), 3, "bytes_slice expects 3 arguments");
                let string_value =
                    self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                let start_value =
                    self.compile_ast(&args[1], vars, capture_slots, env_ptr, function);
                let end_value = self.compile_ast(&args[2], vars, capture_slots, env_ptr, function);
                Some(self.build_bytes_slice(string_value, start_value, end_value, function))
            }
            "string_chars" => {
                assert_eq!(args.len(), 1, "string_chars expects 1 argument");
                let string_value =
                    self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                Some(self.build_string_chars(string_value, function))
            }
            "string_iter_done" => {
                assert_eq!(args.len(), 1, "string_iter_done expects 1 argument");
                let iter_value = self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                Some(self.build_string_iter_done(iter_value, function))
            }
            "string_iter_next" => {
                assert_eq!(args.len(), 1, "string_iter_next expects 1 argument");
                let iter_value = self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                Some(self.build_string_iter_next(iter_value, function))
            }
            "string_copy" => {
                assert_eq!(args.len(), 1, "string_copy expects 1 argument");
                let string_value =
                    self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                Some(self.build_string_copy(string_value, function))
            }
            "string_concat" => {
                assert_eq!(args.len(), 2, "string_concat expects 2 arguments");
                let lhs = self.compile_ast(&args[0], vars, capture_slots, env_ptr, function);
                let rhs = self.compile_ast(&args[1], vars, capture_slots, env_ptr, function);
                Some(self.build_string_concat(lhs, rhs, function))
            }
            _ => None,
        }
    }

    fn compile_named_expression_ast(
        &self,
        name: &str,
        compiled: &[CompiledValue<'ctx>],
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        match name {
            "add" => self.build_internal_call(
                self.require_func("__op_add"),
                &[compiled[0], compiled[1]],
                "add",
            ),
            "subtract" => self.build_internal_call(
                self.require_func("__op_subtract"),
                &[compiled[0], compiled[1]],
                "subtract",
            ),
            "multiply" => self.build_internal_call(
                self.require_func("__op_multiply"),
                &[compiled[0], compiled[1]],
                "multiply",
            ),
            "divide" => self.build_internal_call(
                self.require_func("__op_divide"),
                &[compiled[0], compiled[1]],
                "divide",
            ),
            "modulo" => self.build_internal_call(
                self.require_func("__op_modulo"),
                &[compiled[0], compiled[1]],
                "modulo",
            ),
            "gt" => self.build_internal_call(
                self.require_func("__op_gt"),
                &[compiled[0], compiled[1]],
                "gt",
            ),
            "lt" => self.build_internal_call(
                self.require_func("__op_lt"),
                &[compiled[0], compiled[1]],
                "lt",
            ),
            "gte" => self.build_internal_call(
                self.require_func("__op_gte"),
                &[compiled[0], compiled[1]],
                "gte",
            ),
            "lte" => self.build_internal_call(
                self.require_func("__op_lte"),
                &[compiled[0], compiled[1]],
                "lte",
            ),
            "eq" => self.build_internal_call(
                self.require_func("__op_eq"),
                &[compiled[0], compiled[1]],
                "eq",
            ),
            "ne" => self.build_internal_call(
                self.require_func("__op_ne"),
                &[compiled[0], compiled[1]],
                "ne",
            ),
            "bigint_add" | "bigint_subtract" | "bigint_multiply" | "bigint_divide"
            | "bigint_modulo" | "bigint_compare" => {
                self.compile_bigint_builtin(name, &compiled, function)
            }
            "print" => {
                self.build_internal_call(self.require_func("__rt_print"), &compiled, "print")
            }
            "list_new" => {
                self.build_internal_call(self.require_func("__rt_list_new"), &compiled, "list_new")
            }
            "list_push" => self.build_internal_call(
                self.require_func("__rt_list_push"),
                &compiled,
                "list_push",
            ),
            "list_insert" => self.build_internal_call(
                self.require_func("__rt_list_insert"),
                &compiled,
                "list_insert",
            ),
            "list_len" => {
                self.build_internal_call(self.require_func("__rt_list_len"), &compiled, "list_len")
            }
            "list_get" => {
                self.build_internal_call(self.require_func("__rt_list_get"), &compiled, "list_get")
            }
            "list_set" => {
                self.build_internal_call(self.require_func("__rt_list_set"), &compiled, "list_set")
            }
            "list_swap" => self.build_internal_call(
                self.require_func("__rt_list_swap"),
                &compiled,
                "list_swap",
            ),
            "list_pop" => {
                self.build_internal_call(self.require_func("__rt_list_pop"), &compiled, "list_pop")
            }
            "list_delete" => self.build_internal_call(
                self.require_func("__rt_list_delete"),
                &compiled,
                "list_delete",
            ),
            "list_copy" => self.build_internal_call(
                self.require_func("__rt_list_copy"),
                &compiled,
                "list_copy",
            ),
            other => {
                if vars.contains_key(other) || capture_slots.contains_key(other) {
                    let callee =
                        self.resolve_named_value(other, vars, capture_slots, env_ptr, function);
                    return self.apply_function_value(callee, &compiled, function, other);
                }
                if self.function_ordinals.contains_key(other) {
                    return self.build_user_call(
                        self.require_func(other),
                        self.i64_type.const_zero(),
                        &compiled,
                        other,
                    );
                }
                let callee = self.require_func(other);
                self.build_internal_call(callee, &compiled, other)
            }
        }
    }

    fn compile_block_ast(
        &self,
        block: &crate::parser::BlockAst,
        vars: &HashMap<String, PointerValue<'ctx>>,
        capture_slots: &HashMap<String, usize>,
        env_ptr: IntValue<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> CompiledValue<'ctx> {
        let mut last = None;
        for line in &block.lines {
            last = Some(self.compile_ast(line, vars, capture_slots, env_ptr, function));
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
    ) -> CompiledValue<'ctx> {
        let value = self.compile_ast(value, vars, capture_slots, env_ptr, function);
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
    ) -> CompiledValue<'ctx> {
        let cond_value = self.compile_ast(condition, vars, capture_slots, env_ptr, function);
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
            then_value = self.compile_ast(line, vars, capture_slots, env_ptr, function);
        }
        self.builder.build_unconditional_branch(merge_block).expect("failed to branch from then");
        let then_end = self.builder.get_insert_block().expect("then block should exist");

        self.builder.position_at_end(else_block);
        let mut else_value = self.int_value(self.i64_type.const_zero());
        if let Some(else_block_ast) = else_ {
            for line in &else_block_ast.lines {
                else_value = self.compile_ast(line, vars, capture_slots, env_ptr, function);
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
