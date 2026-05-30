use crate::parser::{Ast, BlockAst, ExpressionAst, FunctionDefAst, LiteralAst};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Int,
    BigInt,
    String,
    List,
    Function,
    StringIter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct KindSet(u8);

impl KindSet {
    const INT_BIT: u8 = 1 << 0;
    const BIGINT_BIT: u8 = 1 << 1;
    const STRING_BIT: u8 = 1 << 2;
    const LIST_BIT: u8 = 1 << 3;
    const FUNCTION_BIT: u8 = 1 << 4;
    const STRING_ITER_BIT: u8 = 1 << 5;
    const ALL_BITS: u8 = Self::INT_BIT
        | Self::BIGINT_BIT
        | Self::STRING_BIT
        | Self::LIST_BIT
        | Self::FUNCTION_BIT
        | Self::STRING_ITER_BIT;

    pub const fn empty() -> Self {
        Self(0)
    }

    pub const fn any() -> Self {
        Self(Self::ALL_BITS)
    }

    pub const fn int() -> Self {
        Self(Self::INT_BIT)
    }

    pub const fn bigint() -> Self {
        Self(Self::BIGINT_BIT)
    }

    pub const fn string() -> Self {
        Self(Self::STRING_BIT)
    }

    pub const fn list() -> Self {
        Self(Self::LIST_BIT)
    }

    pub const fn function() -> Self {
        Self(Self::FUNCTION_BIT)
    }

    pub const fn string_iter() -> Self {
        Self(Self::STRING_ITER_BIT)
    }

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub fn contains(self, kind: ValueKind) -> bool {
        let bit = match kind {
            ValueKind::Int => Self::INT_BIT,
            ValueKind::BigInt => Self::BIGINT_BIT,
            ValueKind::String => Self::STRING_BIT,
            ValueKind::List => Self::LIST_BIT,
            ValueKind::Function => Self::FUNCTION_BIT,
            ValueKind::StringIter => Self::STRING_ITER_BIT,
        };
        self.0 & bit != 0
    }

    pub fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub fn is_any(self) -> bool {
        self.0 == Self::ALL_BITS
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueShape {
    slots: Vec<KindSet>,
}

impl ValueShape {
    pub fn scalar(kinds: KindSet) -> Self {
        Self { slots: vec![kinds] }
    }

    pub fn unknown_scalar() -> Self {
        Self::scalar(KindSet::any())
    }

    pub fn unknown_with_arity(arity: usize) -> Self {
        Self { slots: vec![KindSet::empty(); arity] }
    }

    pub fn from_slots(slots: Vec<KindSet>) -> Self {
        Self { slots }
    }

    pub fn arity(&self) -> usize {
        self.slots.len()
    }

    pub fn slot(&self, index: usize) -> Option<KindSet> {
        self.slots.get(index).copied()
    }

    pub fn slots(&self) -> &[KindSet] {
        &self.slots
    }

    pub fn scalar_slot(&self) -> KindSet {
        if self.arity() == 1 { self.slot(0).unwrap_or(KindSet::empty()) } else { KindSet::empty() }
    }

    pub fn union(&self, other: &Self) -> Self {
        assert_eq!(self.arity(), other.arity(), "cannot union shapes of different arity");
        Self::from_slots(
            self.slots
                .iter()
                .zip(other.slots.iter())
                .map(|(lhs, rhs)| lhs.union(*rhs))
                .collect(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionValueKindAnalysis {
    pub inputs: Vec<KindSet>,
    pub variables: HashMap<String, KindSet>,
    pub returns: ValueShape,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleValueKindAnalysis {
    pub functions: HashMap<String, FunctionValueKindAnalysis>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FunctionKindSummary {
    inputs: Vec<KindSet>,
    returns: ValueShape,
}

#[derive(Debug)]
struct InferFunctionResult {
    variables: HashMap<String, KindSet>,
    returns: ValueShape,
    calls: HashMap<String, Vec<KindSet>>,
}

pub fn analyze_module_value_kinds(
    functions: &[FunctionDefAst],
    function_return_arities: &HashMap<String, usize>,
) -> ModuleValueKindAnalysis {
    let mut summaries = function_return_arities
        .iter()
        .map(|(name, arity)| {
            let input_len = functions
                .iter()
                .find(|func| func.name == *name)
                .map(|func| func.inputs.len())
                .unwrap_or(0);
            (
                name.clone(),
                FunctionKindSummary {
                    inputs: vec![KindSet::empty(); input_len],
                    returns: ValueShape::unknown_with_arity(*arity),
                },
            )
        })
        .collect::<HashMap<_, _>>();

    for _ in 0..functions.len().max(1) {
        let mut changed = false;
        let mut incoming = HashMap::<String, Vec<KindSet>>::new();
        for func in functions {
            let inferred = infer_function(func, &summaries);
            for (callee, args) in inferred.calls {
                merge_input_kinds(&mut incoming, &callee, &args);
            }
            let current = summaries.get(&func.name).cloned().unwrap_or_else(|| FunctionKindSummary {
                inputs: vec![KindSet::empty(); func.inputs.len()],
                returns: ValueShape::unknown_with_arity(inferred.returns.arity()),
            });
            let merged_returns = current.returns.union(&inferred.returns);
            if merged_returns != current.returns {
                summaries.insert(
                    func.name.clone(),
                    FunctionKindSummary { inputs: current.inputs.clone(), returns: merged_returns },
                );
                changed = true;
            }
        }
        for func in functions {
            if let Some(args) = incoming.get(&func.name) {
                let current = summaries.get(&func.name).cloned().unwrap_or_else(|| FunctionKindSummary {
                    inputs: vec![KindSet::empty(); func.inputs.len()],
                    returns: ValueShape::unknown_with_arity(*function_return_arities.get(&func.name).unwrap_or(&1)),
                });
                let merged_inputs = current
                    .inputs
                    .iter()
                    .zip(args.iter())
                    .map(|(lhs, rhs)| lhs.union(*rhs))
                    .collect::<Vec<_>>();
                if merged_inputs != current.inputs {
                    summaries.insert(
                        func.name.clone(),
                        FunctionKindSummary { inputs: merged_inputs, returns: current.returns },
                    );
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }

    let analyses = functions
        .iter()
        .map(|func| {
            let inferred = infer_function(func, &summaries);
            let inputs = summaries
                .get(&func.name)
                .map(|summary| summary.inputs.clone())
                .unwrap_or_else(|| vec![KindSet::empty(); func.inputs.len()]);
            (
                func.name.clone(),
                FunctionValueKindAnalysis { inputs, variables: inferred.variables, returns: inferred.returns },
            )
        })
        .collect::<HashMap<_, _>>();

    ModuleValueKindAnalysis { functions: analyses }
}

fn infer_function(
    function: &FunctionDefAst,
    summaries: &HashMap<String, FunctionKindSummary>,
) -> InferFunctionResult {
    let summary_inputs = summaries.get(&function.name).map(|summary| summary.inputs.as_slice()).unwrap_or(&[]);
    let mut env = function
        .inputs
        .iter()
        .enumerate()
        .map(|(index, name)| {
            let kinds = summary_inputs
                .get(index)
                .copied()
                .filter(|kinds| !kinds.is_empty())
                .unwrap_or_else(|| if function.name == "main" { KindSet::any() } else { KindSet::empty() });
            (name.clone(), kinds)
        })
        .collect::<HashMap<_, _>>();
    let mut calls = HashMap::new();
    let returns = infer_block(&function.block, &mut env, summaries, &mut calls);
    InferFunctionResult { variables: env, returns, calls }
}

fn infer_block(
    block: &BlockAst,
    env: &mut HashMap<String, KindSet>,
    summaries: &HashMap<String, FunctionKindSummary>,
    calls: &mut HashMap<String, Vec<KindSet>>,
) -> ValueShape {
    let mut result = ValueShape::unknown_scalar();
    for line in &block.lines {
        result = infer_ast(line, env, summaries, calls);
    }
    result
}

fn infer_ast(
    ast: &Ast,
    env: &mut HashMap<String, KindSet>,
    summaries: &HashMap<String, FunctionKindSummary>,
    calls: &mut HashMap<String, Vec<KindSet>>,
) -> ValueShape {
    match ast {
        Ast::Block(block) => infer_block(block, env, summaries, calls),
        Ast::FunctionDef(_) => ValueShape::unknown_scalar(),
        Ast::Lambda { .. } | Ast::FunctionRef(_) => ValueShape::scalar(KindSet::function()),
        Ast::Expression(expr) => infer_expression(expr, env, summaries, calls),
        Ast::MultiValue(values) => {
            ValueShape::from_slots(
                values.iter().map(|value| infer_ast(value, env, summaries, calls).scalar_slot()).collect(),
            )
        }
        Ast::Literal(lit) => infer_literal(lit),
        Ast::ListLiteral(values) => {
            for value in values {
                let _ = infer_ast(value, env, summaries, calls);
            }
            ValueShape::scalar(KindSet::list())
        }
        Ast::Index { collection, index, .. } => {
            let collection_kinds = infer_ast(collection, env, summaries, calls).scalar_slot();
            let _ = infer_ast(index, env, summaries, calls);
            ValueShape::scalar(index_result_kinds(collection_kinds))
        }
        Ast::IndexAssign { collection, index, value, .. } => {
            let collection_kinds = infer_ast(collection, env, summaries, calls).scalar_slot();
            let _ = infer_ast(index, env, summaries, calls);
            let value_kinds = infer_ast(value, env, summaries, calls).scalar_slot();
            if collection_kinds.contains(ValueKind::String) {
                ValueShape::scalar(KindSet::int())
            } else if collection_kinds.contains(ValueKind::List) {
                ValueShape::scalar(value_kinds)
            } else {
                ValueShape::scalar(KindSet::any())
            }
        }
        Ast::Variable(name) => {
            ValueShape::scalar(env.get(name.as_str()).copied().unwrap_or_else(KindSet::any))
        }
        Ast::Assign { name, value, .. } => {
            let kinds = infer_ast(value, env, summaries, calls).scalar_slot();
            env.insert(name.clone(), kinds);
            ValueShape::scalar(kinds)
        }
        Ast::MultiAssign { names, value, .. } => {
            let shape = infer_ast(value, env, summaries, calls);
            for (index, name) in names.iter().enumerate() {
                env.insert(name.clone(), shape.slot(index).unwrap_or_else(KindSet::any));
            }
            ValueShape::scalar(KindSet::any())
        }
        Ast::If { condition, then, else_, .. } => {
            let _ = infer_ast(condition, env, summaries, calls);
            let mut then_env = env.clone();
            let then_shape = infer_block(then, &mut then_env, summaries, calls);
            let mut else_env = env.clone();
            let else_shape = if let Some(else_block) = else_ {
                infer_block(else_block, &mut else_env, summaries, calls)
            } else {
                ValueShape::unknown_scalar()
            };
            merge_envs(env, then_env, else_env);
            if then_shape.arity() == else_shape.arity() {
                then_shape.union(&else_shape)
            } else {
                ValueShape::unknown_scalar()
            }
        }
    }
}

fn infer_expression(
    expr: &ExpressionAst,
    env: &mut HashMap<String, KindSet>,
    summaries: &HashMap<String, FunctionKindSummary>,
    calls: &mut HashMap<String, Vec<KindSet>>,
) -> ValueShape {
    if expr.function.is_empty() {
        return expr
            .args
            .first()
            .map(|arg| infer_ast(arg, env, summaries, calls))
            .unwrap_or_else(ValueShape::unknown_scalar);
    }

    let arg_shapes =
        expr.args.iter().map(|arg| infer_ast(arg, env, summaries, calls)).collect::<Vec<_>>();
    if let Some(summary) = summaries.get(&expr.function) {
        merge_input_kinds(
            calls,
            &expr.function,
            &arg_shapes.iter().map(ValueShape::scalar_slot).collect::<Vec<_>>(),
        );
        return summary.returns.clone();
    }
    builtin_shape(&expr.function, &arg_shapes)
}

fn infer_literal(literal: &LiteralAst) -> ValueShape {
    match literal {
        LiteralAst::Integer(_) => ValueShape::scalar(KindSet::int()),
        LiteralAst::BigInt(_) => ValueShape::scalar(KindSet::bigint()),
        LiteralAst::String(_) => ValueShape::scalar(KindSet::string()),
    }
}

fn builtin_shape(name: &str, args: &[ValueShape]) -> ValueShape {
    match name {
        "add" | "subtract" | "multiply" | "divide" | "modulo" => {
            let lhs = args.first().map(ValueShape::scalar_slot).unwrap_or_else(KindSet::any);
            let rhs = args.get(1).map(ValueShape::scalar_slot).unwrap_or_else(KindSet::any);
            ValueShape::scalar(numeric_result_kinds(lhs, rhs))
        }
        "gt" | "lt" | "gte" | "lte" | "eq" | "ne" | "and" | "or" | "not" => {
            ValueShape::scalar(KindSet::int())
        }
        "print" | "list_print" => ValueShape::scalar(KindSet::int()),
        "bigint_from_int" | "bigint_add" | "bigint_subtract" | "bigint_multiply" | "bigint_divide"
        | "bigint_modulo" => ValueShape::scalar(KindSet::bigint()),
        "bigint_compare" => ValueShape::scalar(KindSet::int()),
        "string_concat" | "bytes_slice" | "string_copy" | "string_repeat" | "string_reverse" => {
            ValueShape::scalar(KindSet::string())
        }
        "bytes_len" | "bytes_get" | "bytes_pop" | "string_iter_done" | "string_iter_next"
        | "string_first" | "string_last" | "string_len" | "string_is_empty" | "string_is_not_empty"
        | "string_starts_with" | "string_ends_with" | "string_contains" | "string_is_ascii"
        | "string_all" | "string_any" | "string_is_integer" => ValueShape::scalar(KindSet::int()),
        "string_chars" => ValueShape::scalar(KindSet::string_iter()),
        "list_new" | "list_range" | "list_copy" | "list_map" | "list_filter" => {
            ValueShape::scalar(KindSet::list())
        }
        "list_len" => ValueShape::scalar(KindSet::int()),
        "list_get" | "list_pop" | "list_delete" => ValueShape::scalar(KindSet::any()),
        "list_push" | "list_insert" | "list_set" | "list_swap" => ValueShape::scalar(KindSet::int()),
        "string_try_parse_integer" => {
            ValueShape::from_slots(vec![KindSet::int(), KindSet::int(), KindSet::string()])
        }
        "string_try_parse_bigint" => {
            ValueShape::from_slots(vec![KindSet::int(), KindSet::bigint(), KindSet::string()])
        }
        "string_try_first" | "string_try_last" | "bytes_try_get" | "string_try_pop" => {
            ValueShape::from_slots(vec![KindSet::int(), KindSet::int(), KindSet::string()])
        }
        _ => ValueShape::unknown_scalar(),
    }
}

fn numeric_result_kinds(lhs: KindSet, rhs: KindSet) -> KindSet {
    if lhs.is_empty() || rhs.is_empty() {
        return KindSet::empty();
    }
    let can_int = lhs.contains(ValueKind::Int) && rhs.contains(ValueKind::Int);
    let can_bigint = (lhs.contains(ValueKind::BigInt)
        && (rhs.contains(ValueKind::Int) || rhs.contains(ValueKind::BigInt)))
        || (rhs.contains(ValueKind::BigInt)
            && (lhs.contains(ValueKind::Int) || lhs.contains(ValueKind::BigInt)));

    match (can_int, can_bigint) {
        (true, true) => KindSet::int().union(KindSet::bigint()),
        (true, false) => KindSet::int(),
        (false, true) => KindSet::bigint(),
        (false, false) => KindSet::any(),
    }
}

fn index_result_kinds(collection: KindSet) -> KindSet {
    let mut result = KindSet::empty();
    if collection.contains(ValueKind::String) {
        result = result.union(KindSet::int());
    }
    if collection.contains(ValueKind::List) {
        result = result.union(KindSet::any());
    }
    if result.is_empty() { KindSet::any() } else { result }
}

fn merge_envs(
    env: &mut HashMap<String, KindSet>,
    then_env: HashMap<String, KindSet>,
    else_env: HashMap<String, KindSet>,
) {
    let names = env
        .keys()
        .chain(then_env.keys())
        .chain(else_env.keys())
        .cloned()
        .collect::<HashSet<_>>();
    for name in names {
        let base = env.get(&name).copied().unwrap_or_else(KindSet::empty);
        let then_value = then_env.get(&name).copied().unwrap_or(base);
        let else_value = else_env.get(&name).copied().unwrap_or(base);
        env.insert(name, then_value.union(else_value));
    }
}

fn merge_input_kinds(
    calls: &mut HashMap<String, Vec<KindSet>>,
    callee: &str,
    args: &[KindSet],
) {
    let entry = calls
        .entry(callee.to_string())
        .or_insert_with(|| vec![KindSet::empty(); args.len()]);
    if entry.len() < args.len() {
        entry.resize(args.len(), KindSet::empty());
    }
    for (slot, arg) in entry.iter_mut().zip(args.iter()) {
        *slot = slot.union(*arg);
    }
}

#[cfg(test)]
mod tests {
    use super::{KindSet, ValueKind};
    use crate::module::Module;

    #[test]
    fn analyze_value_kinds_tracks_try_parse_integer_slots() {
        let src = "fn main() do\n    ok, value, err = string_try_parse_integer(\"12\")\n    print(ok)\n    print(value)\n    print(err)\n    0\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("ok").copied(), Some(KindSet::int()));
        assert_eq!(main.variables.get("value").copied(), Some(KindSet::int()));
        assert_eq!(main.variables.get("err").copied(), Some(KindSet::string()));
        assert_eq!(main.returns.slot(0), Some(KindSet::int()));
    }

    #[test]
    fn analyze_value_kinds_tracks_try_parse_bigint_slots() {
        let src = "fn main() do\n    ok, value, err = string_try_parse_bigint(\"12\")\n    print(ok)\n    print(value)\n    print(err)\n    0\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("ok").copied(), Some(KindSet::int()));
        assert_eq!(main.variables.get("value").copied(), Some(KindSet::bigint()));
        assert_eq!(main.variables.get("err").copied(), Some(KindSet::string()));
    }

    #[test]
    fn analyze_value_kinds_tracks_recursive_multi_return_function() {
        let src = "fn parse_digits(s, index, value) do\n    if index == bytes_len(s) do\n        true, value, \"\"\n    else\n        parse_digits(s, index + 1, value * 10 + bytes_get(s, index) - 48)\n    end\nend\n\nfn main() do\n    ok, value, err = parse_digits(\"12\", 0, 0)\n    print(ok)\n    print(value)\n    print(err)\n    0\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let parse_digits = analysis.functions.get("parse_digits").expect("analysis missing");
        assert_eq!(parse_digits.returns.slot(0), Some(KindSet::int()));
        assert_eq!(parse_digits.returns.slot(1), Some(KindSet::int()));
        assert_eq!(parse_digits.returns.slot(2), Some(KindSet::string()));
    }

    #[test]
    fn analyze_value_kinds_tracks_calculator_bigint_variables() {
        let src = std::fs::read_to_string("examples/calculator.expr").expect("calculator example should load");
        let module = Module::try_from_source(&src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("lhs_ok").copied(), Some(KindSet::int()));
        assert_eq!(main.variables.get("lhs").copied(), Some(KindSet::bigint()));
        assert_eq!(main.variables.get("lhs_err").copied(), Some(KindSet::string()));
        assert_eq!(main.variables.get("rhs_ok").copied(), Some(KindSet::int()));
        assert_eq!(main.variables.get("rhs").copied(), Some(KindSet::bigint()));
        assert_eq!(main.variables.get("rhs_err").copied(), Some(KindSet::string()));
        let apply = analysis.functions.get("apply_and_print").expect("apply_and_print missing");
        assert_eq!(apply.variables.get("lhs").copied(), Some(KindSet::bigint()));
        assert_eq!(apply.variables.get("rhs").copied(), Some(KindSet::bigint()));
        assert!(apply.returns.slot(0).expect("return slot missing").contains(ValueKind::Int));
    }
}
