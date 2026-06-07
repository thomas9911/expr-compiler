use crate::parser::{Ast, BlockAst, ExpressionAst, FunctionDefAst, LiteralAst};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Int,
    BigInt,
    String,
    List,
    Map,
    MapIter,
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
    const MAP_BIT: u8 = 1 << 4;
    const MAP_ITER_BIT: u8 = 1 << 5;
    const FUNCTION_BIT: u8 = 1 << 6;
    const STRING_ITER_BIT: u8 = 1 << 7;
    const ALL_BITS: u8 = Self::INT_BIT
        | Self::BIGINT_BIT
        | Self::STRING_BIT
        | Self::LIST_BIT
        | Self::MAP_BIT
        | Self::MAP_ITER_BIT
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

    pub const fn map() -> Self {
        Self(Self::MAP_BIT)
    }

    pub const fn map_iter() -> Self {
        Self(Self::MAP_ITER_BIT)
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
            ValueKind::Map => Self::MAP_BIT,
            ValueKind::MapIter => Self::MAP_ITER_BIT,
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

    pub fn without(self, kind: ValueKind) -> Self {
        let bit = match kind {
            ValueKind::Int => Self::INT_BIT,
            ValueKind::BigInt => Self::BIGINT_BIT,
            ValueKind::String => Self::STRING_BIT,
            ValueKind::List => Self::LIST_BIT,
            ValueKind::Map => Self::MAP_BIT,
            ValueKind::MapIter => Self::MAP_ITER_BIT,
            ValueKind::Function => Self::FUNCTION_BIT,
            ValueKind::StringIter => Self::STRING_ITER_BIT,
        };
        Self(self.0 & !bit)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueShape {
    Scalar(KindSet),
    Multi(Vec<KindSet>),
    List { items: KindSet },
    Map { values: KindSet },
    MapIter { values: KindSet },
}

impl ValueShape {
    pub fn scalar(kinds: KindSet) -> Self {
        Self::Scalar(kinds)
    }

    pub fn list(items: KindSet) -> Self {
        Self::List { items }
    }

    pub fn map(values: KindSet) -> Self {
        Self::Map { values }
    }

    pub fn map_iter(values: KindSet) -> Self {
        Self::MapIter { values }
    }

    pub fn unknown_scalar() -> Self {
        Self::scalar(KindSet::any())
    }

    pub fn unknown_with_arity(arity: usize) -> Self {
        if arity == 1 {
            Self::scalar(KindSet::empty())
        } else {
            Self::Multi(vec![KindSet::empty(); arity])
        }
    }

    pub fn from_slots(slots: Vec<KindSet>) -> Self {
        if slots.len() == 1 { Self::scalar(slots[0]) } else { Self::Multi(slots) }
    }

    pub fn arity(&self) -> usize {
        match self {
            Self::Scalar(_) | Self::List { .. } | Self::Map { .. } | Self::MapIter { .. } => 1,
            Self::Multi(slots) => slots.len(),
        }
    }

    pub fn slot(&self, index: usize) -> Option<KindSet> {
        match self {
            Self::Scalar(kinds) => (index == 0).then_some(*kinds),
            Self::List { .. } => (index == 0).then_some(KindSet::list()),
            Self::Map { .. } => (index == 0).then_some(KindSet::map()),
            Self::MapIter { .. } => (index == 0).then_some(KindSet::map_iter()),
            Self::Multi(slots) => slots.get(index).copied(),
        }
    }

    pub fn scalar_slot(&self) -> KindSet {
        if self.arity() == 1 { self.slot(0).unwrap_or(KindSet::empty()) } else { KindSet::empty() }
    }

    pub fn list_items(&self) -> Option<KindSet> {
        match self {
            Self::List { items } => Some(*items),
            _ => None,
        }
    }

    pub fn map_values(&self) -> Option<KindSet> {
        match self {
            Self::Map { values } => Some(*values),
            _ => None,
        }
    }

    pub fn map_iter_values(&self) -> Option<KindSet> {
        match self {
            Self::MapIter { values } => Some(*values),
            _ => None,
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Scalar(lhs), Self::Scalar(rhs)) => Self::scalar(lhs.union(*rhs)),
            (Self::List { items: lhs }, Self::List { items: rhs }) => Self::list(lhs.union(*rhs)),
            (Self::Map { values: lhs }, Self::Map { values: rhs }) => Self::map(lhs.union(*rhs)),
            (Self::MapIter { values: lhs }, Self::MapIter { values: rhs }) => {
                Self::map_iter(lhs.union(*rhs))
            }
            (Self::Multi(lhs), Self::Multi(rhs)) => {
                assert_eq!(lhs.len(), rhs.len(), "cannot union shapes of different arity");
                Self::from_slots(
                    lhs.iter().zip(rhs.iter()).map(|(lhs, rhs)| lhs.union(*rhs)).collect(),
                )
            }
            _ if self.arity() == other.arity() && self.arity() == 1 => {
                Self::scalar(self.scalar_slot().union(other.scalar_slot()))
            }
            _ => panic!("cannot union shapes of different arity"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionValueKindAnalysis {
    pub inputs: Vec<ValueShape>,
    pub variables: HashMap<String, ValueShape>,
    pub function_bindings: HashMap<String, String>,
    pub returns: ValueShape,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleValueKindAnalysis {
    pub functions: HashMap<String, FunctionValueKindAnalysis>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FunctionKindSummary {
    inputs: Vec<ValueShape>,
    returns: ValueShape,
}

#[derive(Debug)]
struct InferFunctionResult {
    variables: HashMap<String, ValueShape>,
    function_bindings: HashMap<String, String>,
    returns: ValueShape,
    calls: HashMap<String, Vec<ValueShape>>,
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
                    inputs: (0..input_len)
                        .map(|index| {
                            if name == "main" && index == 0 {
                                ValueShape::list(KindSet::string())
                            } else {
                                ValueShape::scalar(KindSet::empty())
                            }
                        })
                        .collect(),
                    returns: ValueShape::unknown_with_arity(*arity),
                },
            )
        })
        .collect::<HashMap<_, _>>();

    for _ in 0..functions.len().max(1) {
        let mut changed = false;
        let mut incoming = HashMap::<String, Vec<ValueShape>>::new();
        for func in functions {
            let inferred = infer_function(func, &summaries);
            for (callee, args) in inferred.calls {
                merge_input_kinds(&mut incoming, &callee, &args);
            }
            let current =
                summaries.get(&func.name).cloned().unwrap_or_else(|| FunctionKindSummary {
                    inputs: vec![ValueShape::scalar(KindSet::empty()); func.inputs.len()],
                    returns: ValueShape::unknown_with_arity(inferred.returns.arity()),
                });
            let merged_returns = if current.returns.arity() == inferred.returns.arity() {
                current.returns.union(&inferred.returns)
            } else {
                ValueShape::unknown_with_arity(
                    current.returns.arity().max(inferred.returns.arity()),
                )
            };
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
                let current =
                    summaries.get(&func.name).cloned().unwrap_or_else(|| FunctionKindSummary {
                        inputs: vec![ValueShape::scalar(KindSet::empty()); func.inputs.len()],
                        returns: ValueShape::unknown_with_arity(
                            *function_return_arities.get(&func.name).unwrap_or(&1),
                        ),
                    });
                let merged_inputs = current
                    .inputs
                    .iter()
                    .zip(args.iter())
                    .map(|(lhs, rhs)| lhs.union(rhs))
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
                .unwrap_or_else(|| vec![ValueShape::scalar(KindSet::empty()); func.inputs.len()]);
            (
                func.name.clone(),
                FunctionValueKindAnalysis {
                    inputs,
                    variables: inferred.variables,
                    function_bindings: inferred.function_bindings,
                    returns: inferred.returns,
                },
            )
        })
        .collect::<HashMap<_, _>>();

    ModuleValueKindAnalysis { functions: analyses }
}

fn infer_function(
    function: &FunctionDefAst,
    summaries: &HashMap<String, FunctionKindSummary>,
) -> InferFunctionResult {
    let summary_inputs =
        summaries.get(&function.name).map(|summary| summary.inputs.as_slice()).unwrap_or(&[]);
    let mut env = function
        .inputs
        .iter()
        .enumerate()
        .map(|(index, name)| {
            let default_shape = if function.name == "main" && index == 0 {
                ValueShape::list(KindSet::string())
            } else {
                ValueShape::scalar(KindSet::empty())
            };
            let shape = summary_inputs.get(index).cloned().unwrap_or_else(|| default_shape.clone());
            let shape = match shape {
                ValueShape::Scalar(kinds)
                    if kinds.is_empty() && function.name == "main" && index == 0 =>
                {
                    default_shape
                }
                other => other,
            };
            (name.clone(), shape)
        })
        .collect::<HashMap<_, _>>();
    let mut function_bindings = HashMap::new();
    let mut calls = HashMap::new();
    let returns =
        infer_block(&function.block, &mut env, &mut function_bindings, summaries, &mut calls);
    InferFunctionResult { variables: env, function_bindings, returns, calls }
}

fn infer_block(
    block: &BlockAst,
    env: &mut HashMap<String, ValueShape>,
    function_bindings: &mut HashMap<String, String>,
    summaries: &HashMap<String, FunctionKindSummary>,
    calls: &mut HashMap<String, Vec<ValueShape>>,
) -> ValueShape {
    let mut result = ValueShape::unknown_scalar();
    for line in &block.lines {
        result = infer_ast(line, env, function_bindings, summaries, calls);
    }
    result
}

fn infer_ast(
    ast: &Ast,
    env: &mut HashMap<String, ValueShape>,
    function_bindings: &mut HashMap<String, String>,
    summaries: &HashMap<String, FunctionKindSummary>,
    calls: &mut HashMap<String, Vec<ValueShape>>,
) -> ValueShape {
    match ast {
        Ast::Block(block) => infer_block(block, env, function_bindings, summaries, calls),
        Ast::FunctionDef(_) => ValueShape::unknown_scalar(),
        Ast::Lambda { .. } | Ast::FunctionRef(_) => ValueShape::scalar(KindSet::function()),
        Ast::Expression(expr) => infer_expression(expr, env, function_bindings, summaries, calls),
        Ast::MultiValue(values) => ValueShape::from_slots(
            values
                .iter()
                .map(|value| {
                    infer_ast(value, env, function_bindings, summaries, calls).scalar_slot()
                })
                .collect(),
        ),
        Ast::Literal(lit) => infer_literal(lit),
        Ast::ListLiteral(values) => {
            let items = values.iter().fold(KindSet::empty(), |items, value| {
                items
                    .union(infer_ast(value, env, function_bindings, summaries, calls).scalar_slot())
            });
            ValueShape::list(items)
        }
        Ast::MapLiteral(entries) => {
            let values = entries.iter().fold(KindSet::empty(), |kinds, entry| {
                if let crate::parser::MapKeyAst::Dynamic(key) = &entry.key {
                    let _ = infer_ast(key, env, function_bindings, summaries, calls);
                }
                kinds.union(
                    infer_ast(&entry.value, env, function_bindings, summaries, calls).scalar_slot(),
                )
            });
            ValueShape::map(values)
        }
        Ast::Index { collection, index, .. } => {
            let collection_shape = infer_ast(collection, env, function_bindings, summaries, calls);
            let _ = infer_ast(index, env, function_bindings, summaries, calls);
            ValueShape::scalar(index_result_kinds(&collection_shape))
        }
        Ast::IndexAssign { collection, index, value, .. } => {
            let collection_shape = infer_ast(collection, env, function_bindings, summaries, calls);
            let _ = infer_ast(index, env, function_bindings, summaries, calls);
            let value_shape = infer_ast(value, env, function_bindings, summaries, calls);
            if collection_shape.scalar_slot().contains(ValueKind::String) {
                ValueShape::scalar(KindSet::int())
            } else if collection_shape.scalar_slot().contains(ValueKind::List) {
                ValueShape::scalar(value_shape.scalar_slot())
            } else {
                ValueShape::scalar(KindSet::any())
            }
        }
        Ast::Variable(name) => {
            if let Some(shape) = env.get(name.as_str()).cloned() {
                shape
            } else if summaries.contains_key(name.as_ref()) {
                ValueShape::scalar(KindSet::function())
            } else {
                ValueShape::unknown_scalar()
            }
        }
        Ast::Assign { name, value, .. } => {
            let shape = infer_ast(value, env, function_bindings, summaries, calls);
            env.insert(name.clone(), shape.clone());
            match infer_known_callback_name(value, env, function_bindings) {
                Some(binding) => {
                    function_bindings.insert(name.clone(), binding.to_string());
                }
                None => {
                    function_bindings.remove(name);
                }
            }
            shape
        }
        Ast::MultiAssign { names, value, .. } => {
            let shape = infer_ast(value, env, function_bindings, summaries, calls);
            for (index, name) in names.iter().enumerate() {
                env.insert(
                    name.clone(),
                    ValueShape::scalar(shape.slot(index).unwrap_or_else(KindSet::any)),
                );
                function_bindings.remove(name);
            }
            ValueShape::scalar(KindSet::any())
        }
        Ast::If { condition, then, else_, .. } => {
            let _ = infer_ast(condition, env, function_bindings, summaries, calls);
            let mut then_env = env.clone();
            let mut then_bindings = function_bindings.clone();
            apply_condition_narrowing(condition, &mut then_env, &mut then_bindings, true);
            let then_shape = infer_block(then, &mut then_env, &mut then_bindings, summaries, calls);
            let mut else_env = env.clone();
            let mut else_bindings = function_bindings.clone();
            apply_condition_narrowing(condition, &mut else_env, &mut else_bindings, false);
            let else_shape = if let Some(else_block) = else_ {
                infer_block(else_block, &mut else_env, &mut else_bindings, summaries, calls)
            } else {
                ValueShape::unknown_scalar()
            };
            merge_envs(env, then_env, else_env);
            merge_function_bindings(function_bindings, then_bindings, else_bindings);
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
    env: &mut HashMap<String, ValueShape>,
    function_bindings: &mut HashMap<String, String>,
    summaries: &HashMap<String, FunctionKindSummary>,
    calls: &mut HashMap<String, Vec<ValueShape>>,
) -> ValueShape {
    if expr.function.is_empty() {
        return expr
            .args
            .first()
            .map(|arg| infer_ast(arg, env, function_bindings, summaries, calls))
            .unwrap_or_else(ValueShape::unknown_scalar);
    }

    let arg_shapes = expr
        .args
        .iter()
        .map(|arg| infer_ast(arg, env, function_bindings, summaries, calls))
        .collect::<Vec<_>>();
    if matches!(expr.function.as_str(), "list_map" | "list_filter")
        && let Some(callback_name) = expr
            .args
            .get(1)
            .and_then(|callback| infer_known_callback_name(callback, env, function_bindings))
    {
        let callback_input = ValueShape::scalar(
            arg_shapes.first().and_then(ValueShape::list_items).unwrap_or_else(KindSet::empty),
        );
        merge_input_kinds(calls, callback_name, &[callback_input]);
    }
    if matches!(
        expr.function.as_str(),
        "map_try_get"
            | "map_try_delete"
            | "map_try_pop"
            | "map_iter_next"
            | "string_from_codepoints"
    ) {
        return builtin_shape(&expr.function, &arg_shapes);
    }
    if let Some(summary) = summaries.get(&expr.function) {
        merge_input_kinds(calls, &expr.function, &arg_shapes);
        return summary.returns.clone();
    }

    if expr.function == "list_map" {
        let items = expr
            .args
            .get(1)
            .and_then(|callback| {
                infer_known_callback_return_shape(callback, env, function_bindings, summaries)
            })
            .map(|shape| shape.scalar_slot())
            .unwrap_or_else(KindSet::empty);
        return ValueShape::list(items);
    }

    if expr.function == "list_filter" {
        return arg_shapes
            .first()
            .and_then(ValueShape::list_items)
            .map(ValueShape::list)
            .unwrap_or_else(|| ValueShape::list(KindSet::empty()));
    }

    if matches!(expr.function.as_str(), "list_push" | "list_insert" | "list_set")
        && let Some(Ast::Variable(name)) = expr.args.first()
        && let Some(current) = env.get(name.as_str()).cloned()
    {
        let value_index = if expr.function == "list_push" { 1 } else { 2 };
        let new_items = current.list_items().unwrap_or_else(KindSet::empty).union(
            arg_shapes.get(value_index).map(ValueShape::scalar_slot).unwrap_or_else(KindSet::empty),
        );
        env.insert(name.name.clone(), ValueShape::list(new_items));
    }

    if expr.function == "map_set"
        && let Some(Ast::Variable(name)) = expr.args.first()
        && let Some(current) = env.get(name.as_str()).cloned()
    {
        let new_values = current
            .map_values()
            .unwrap_or_else(KindSet::empty)
            .union(arg_shapes.get(2).map(ValueShape::scalar_slot).unwrap_or_else(KindSet::empty));
        env.insert(name.name.clone(), ValueShape::map(new_values));
    }

    builtin_shape(&expr.function, &arg_shapes)
}

fn infer_known_callback_return_shape(
    callback: &Ast,
    env: &HashMap<String, ValueShape>,
    function_bindings: &HashMap<String, String>,
    summaries: &HashMap<String, FunctionKindSummary>,
) -> Option<ValueShape> {
    match callback {
        Ast::FunctionRef(name) => {
            summaries.get(name.as_ref()).map(|summary| summary.returns.clone())
        }
        Ast::Variable(name) if !env.contains_key(name.as_str()) => {
            summaries.get(name.as_ref()).map(|summary| summary.returns.clone())
        }
        Ast::Variable(name) => function_bindings
            .get(name.as_str())
            .and_then(|binding| summaries.get(binding))
            .map(|summary| summary.returns.clone()),
        _ => None,
    }
}

fn infer_known_callback_name<'a>(
    callback: &'a Ast,
    env: &HashMap<String, ValueShape>,
    function_bindings: &'a HashMap<String, String>,
) -> Option<&'a str> {
    match callback {
        Ast::FunctionRef(name) => Some(name.as_ref()),
        Ast::Variable(name) if !env.contains_key(name.as_str()) => Some(name.as_ref()),
        Ast::Variable(name) => function_bindings.get(name.as_str()).map(|name| name.as_str()),
        _ => None,
    }
}

fn predicate_kind(function: &str) -> Option<ValueKind> {
    match function {
        "is_int" => Some(ValueKind::Int),
        "is_bigint" => Some(ValueKind::BigInt),
        "is_string" => Some(ValueKind::String),
        "is_list" => Some(ValueKind::List),
        "is_map" => Some(ValueKind::Map),
        "is_map_iter" => Some(ValueKind::MapIter),
        "is_function" => Some(ValueKind::Function),
        "is_string_iter" => Some(ValueKind::StringIter),
        _ => None,
    }
}

fn condition_predicate_binding(condition: &Ast) -> Option<(&str, ValueKind, bool)> {
    match condition {
        Ast::Expression(ExpressionAst { function, args, .. })
            if function == "not" && args.len() == 1 =>
        {
            let (name, kind, positive) = condition_predicate_binding(&args[0])?;
            Some((name, kind, !positive))
        }
        Ast::Expression(ExpressionAst { function, args, .. }) => {
            let kind = predicate_kind(function)?;
            let Ast::Variable(name) = args.first()? else {
                return None;
            };
            Some((name.as_ref(), kind, true))
        }
        _ => None,
    }
}

fn exact_shape_for_kind(shape: &ValueShape, kind: ValueKind) -> ValueShape {
    match kind {
        ValueKind::Int => ValueShape::scalar(KindSet::int()),
        ValueKind::BigInt => ValueShape::scalar(KindSet::bigint()),
        ValueKind::String => ValueShape::scalar(KindSet::string()),
        ValueKind::List => shape
            .list_items()
            .map(ValueShape::list)
            .unwrap_or_else(|| ValueShape::scalar(KindSet::list())),
        ValueKind::Map => shape
            .map_values()
            .map(ValueShape::map)
            .unwrap_or_else(|| ValueShape::scalar(KindSet::map())),
        ValueKind::MapIter => shape
            .map_iter_values()
            .map(ValueShape::map_iter)
            .unwrap_or_else(|| ValueShape::scalar(KindSet::map_iter())),
        ValueKind::Function => ValueShape::scalar(KindSet::function()),
        ValueKind::StringIter => ValueShape::scalar(KindSet::string_iter()),
    }
}

fn negative_narrowed_shape(shape: &ValueShape, kind: ValueKind) -> ValueShape {
    match (shape, kind) {
        (ValueShape::List { .. }, ValueKind::List)
        | (ValueShape::Map { .. }, ValueKind::Map)
        | (ValueShape::MapIter { .. }, ValueKind::MapIter) => ValueShape::scalar(KindSet::empty()),
        _ => ValueShape::scalar(shape.scalar_slot().without(kind)),
    }
}

fn narrowed_shape(shape: &ValueShape, kind: ValueKind, positive: bool) -> ValueShape {
    if positive {
        if shape.scalar_slot().contains(kind) {
            exact_shape_for_kind(shape, kind)
        } else {
            ValueShape::scalar(KindSet::empty())
        }
    } else {
        negative_narrowed_shape(shape, kind)
    }
}

fn apply_condition_narrowing(
    condition: &Ast,
    env: &mut HashMap<String, ValueShape>,
    function_bindings: &mut HashMap<String, String>,
    then_branch: bool,
) {
    let Some((name, kind, positive_when_true)) = condition_predicate_binding(condition) else {
        return;
    };
    let Some(current) = env.get(name).cloned() else {
        return;
    };
    let next = narrowed_shape(
        &current,
        kind,
        if then_branch { positive_when_true } else { !positive_when_true },
    );
    if !next.scalar_slot().contains(ValueKind::Function) {
        function_bindings.remove(name);
    }
    env.insert(name.to_string(), next);
}

pub fn narrowed_function_analyses_for_condition(
    condition: &Ast,
    base: &FunctionValueKindAnalysis,
) -> (FunctionValueKindAnalysis, FunctionValueKindAnalysis) {
    let mut then_analysis = base.clone();
    apply_condition_narrowing(
        condition,
        &mut then_analysis.variables,
        &mut then_analysis.function_bindings,
        true,
    );
    let mut else_analysis = base.clone();
    apply_condition_narrowing(
        condition,
        &mut else_analysis.variables,
        &mut else_analysis.function_bindings,
        false,
    );
    (then_analysis, else_analysis)
}

fn infer_literal(literal: &LiteralAst) -> ValueShape {
    match literal {
        LiteralAst::Bool(_) => ValueShape::scalar(KindSet::int()),
        LiteralAst::Integer(_) => ValueShape::scalar(KindSet::int()),
        LiteralAst::BigInt(_) => ValueShape::scalar(KindSet::bigint()),
        LiteralAst::String(_) => ValueShape::scalar(KindSet::string()),
    }
}

fn builtin_boolean_shape(name: &str) -> Option<ValueShape> {
    if matches!(
        name,
        "gt" | "lt"
            | "gte"
            | "lte"
            | "eq"
            | "ne"
            | "and"
            | "or"
            | "not"
            | "is_int"
            | "is_bigint"
            | "is_string"
            | "is_list"
            | "is_map"
            | "is_function"
            | "is_string_iter"
            | "is_map_iter"
            | "print"
            | "list_print"
            | "bigint_compare"
            | "bytes_len"
            | "bytes_get"
            | "bytes_pop"
            | "string_iter_done"
            | "string_iter_next"
            | "string_first"
            | "string_last"
            | "string_len"
            | "string_is_empty"
            | "string_is_not_empty"
            | "string_starts_with"
            | "string_ends_with"
            | "string_contains"
            | "string_is_ascii"
            | "string_all"
            | "string_any"
            | "string_is_integer"
            | "list_len"
            | "list_push"
            | "list_insert"
            | "list_set"
            | "list_swap"
            | "map_len"
            | "map_has"
            | "map_iter_done"
            | "map_iter_advance"
    ) {
        Some(ValueShape::scalar(KindSet::int()))
    } else {
        None
    }
}

fn builtin_numeric_shape(name: &str, args: &[ValueShape]) -> Option<ValueShape> {
    let lhs = args.first().map(ValueShape::scalar_slot).unwrap_or_else(KindSet::any);
    let rhs = args.get(1).map(ValueShape::scalar_slot).unwrap_or_else(KindSet::any);
    match name {
        "add" | "subtract" | "multiply" | "divide" | "modulo" => {
            Some(ValueShape::scalar(numeric_result_kinds(lhs, rhs)))
        }
        "bigint_from_int" | "bigint_add" | "bigint_subtract" | "bigint_multiply"
        | "bigint_divide" | "bigint_modulo" => Some(ValueShape::scalar(KindSet::bigint())),
        _ => None,
    }
}

fn builtin_string_shape(name: &str) -> Option<ValueShape> {
    match name {
        "string_concat"
        | "bytes_slice"
        | "string_copy"
        | "string_repeat"
        | "string_reverse"
        | "string_from_codepoints" => Some(ValueShape::scalar(KindSet::string())),
        "string_chars" => Some(ValueShape::scalar(KindSet::string_iter())),
        "string_try_parse_integer" => {
            Some(ValueShape::from_slots(vec![KindSet::int(), KindSet::int(), KindSet::string()]))
        }
        "string_try_parse_bigint" => {
            Some(ValueShape::from_slots(vec![KindSet::int(), KindSet::bigint(), KindSet::string()]))
        }
        "string_try_first" | "string_try_last" | "bytes_try_get" | "string_try_pop" => {
            Some(ValueShape::from_slots(vec![KindSet::int(), KindSet::int(), KindSet::string()]))
        }
        _ => None,
    }
}

fn builtin_list_shape(name: &str, args: &[ValueShape]) -> Option<ValueShape> {
    match name {
        "list_new" => Some(ValueShape::list(KindSet::empty())),
        "list_range" => Some(ValueShape::list(KindSet::int())),
        "list_copy" | "list_filter" => Some(
            args.first()
                .and_then(ValueShape::list_items)
                .map(ValueShape::list)
                .unwrap_or_else(|| ValueShape::list(KindSet::empty())),
        ),
        "list_map" => Some(ValueShape::list(KindSet::empty())),
        "list_get" | "list_pop" | "list_delete" => Some(ValueShape::scalar(
            args.first().and_then(ValueShape::list_items).unwrap_or_else(KindSet::any),
        )),
        _ => None,
    }
}

fn builtin_map_shape(name: &str, args: &[ValueShape]) -> Option<ValueShape> {
    match name {
        "map_new" => Some(ValueShape::map(KindSet::empty())),
        "map_iter" => Some(
            args.first()
                .and_then(ValueShape::map_values)
                .map(ValueShape::map_iter)
                .unwrap_or_else(|| ValueShape::map_iter(KindSet::empty())),
        ),
        "map_get" | "map_delete" => Some(ValueShape::scalar(
            args.first().and_then(ValueShape::map_values).unwrap_or_else(KindSet::any),
        )),
        "map_try_get" | "map_try_delete" => Some(ValueShape::from_slots(vec![
            KindSet::int(),
            args.first().and_then(ValueShape::map_values).unwrap_or_else(KindSet::any),
            KindSet::string(),
        ])),
        "map_try_pop" => Some(ValueShape::from_slots(vec![
            KindSet::int(),
            KindSet::string(),
            args.first().and_then(ValueShape::map_values).unwrap_or_else(KindSet::any),
        ])),
        "map_iter_next" => Some(ValueShape::from_slots(vec![
            KindSet::string(),
            args.first().and_then(ValueShape::map_iter_values).unwrap_or_else(KindSet::any),
        ])),
        "map_iter_key" => Some(ValueShape::scalar(KindSet::string())),
        "map_iter_value" => Some(ValueShape::scalar(
            args.first().and_then(ValueShape::map_iter_values).unwrap_or_else(KindSet::any),
        )),
        "map_keys" => Some(ValueShape::list(KindSet::string())),
        "map_values" => Some(
            args.first()
                .and_then(ValueShape::map_values)
                .map(ValueShape::list)
                .unwrap_or_else(|| ValueShape::list(KindSet::empty())),
        ),
        "map_set" => Some(ValueShape::map(
            args.first()
                .and_then(ValueShape::map_values)
                .unwrap_or_else(KindSet::empty)
                .union(args.get(2).map(ValueShape::scalar_slot).unwrap_or_else(KindSet::empty)),
        )),
        _ => None,
    }
}

fn builtin_shape(name: &str, args: &[ValueShape]) -> ValueShape {
    builtin_boolean_shape(name)
        .or_else(|| builtin_numeric_shape(name, args))
        .or_else(|| builtin_string_shape(name))
        .or_else(|| builtin_list_shape(name, args))
        .or_else(|| builtin_map_shape(name, args))
        .unwrap_or_else(ValueShape::unknown_scalar)
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

fn index_result_kinds(collection: &ValueShape) -> KindSet {
    let mut result = KindSet::empty();
    let collection_kinds = collection.scalar_slot();
    if collection_kinds.contains(ValueKind::String) {
        result = result.union(KindSet::int());
    }
    if let Some(items) = collection.list_items() {
        result = result.union(items);
    } else if collection_kinds.contains(ValueKind::List) {
        result = result.union(KindSet::any());
    }
    if result.is_empty() { KindSet::any() } else { result }
}

fn merge_envs(
    env: &mut HashMap<String, ValueShape>,
    then_env: HashMap<String, ValueShape>,
    else_env: HashMap<String, ValueShape>,
) {
    let names =
        env.keys().chain(then_env.keys()).chain(else_env.keys()).cloned().collect::<HashSet<_>>();
    for name in names {
        let base = env.get(&name).cloned().unwrap_or_else(|| ValueShape::scalar(KindSet::empty()));
        let then_value = then_env.get(&name).cloned().unwrap_or_else(|| base.clone());
        let else_value = else_env.get(&name).cloned().unwrap_or(base);
        env.insert(name, then_value.union(&else_value));
    }
}

fn merge_function_bindings(
    bindings: &mut HashMap<String, String>,
    then_bindings: HashMap<String, String>,
    else_bindings: HashMap<String, String>,
) {
    let names = bindings
        .keys()
        .chain(then_bindings.keys())
        .chain(else_bindings.keys())
        .cloned()
        .collect::<HashSet<_>>();
    bindings.clear();
    for name in names {
        match (then_bindings.get(&name), else_bindings.get(&name)) {
            (Some(lhs), Some(rhs)) if lhs == rhs => {
                bindings.insert(name, lhs.clone());
            }
            _ => {}
        }
    }
}

fn merge_input_kinds(
    calls: &mut HashMap<String, Vec<ValueShape>>,
    callee: &str,
    args: &[ValueShape],
) {
    let entry = calls
        .entry(callee.to_string())
        .or_insert_with(|| vec![ValueShape::scalar(KindSet::empty()); args.len()]);
    if entry.len() < args.len() {
        entry.resize(args.len(), ValueShape::scalar(KindSet::empty()));
    }
    for (slot, arg) in entry.iter_mut().zip(args.iter()) {
        *slot = slot.union(arg);
    }
}

#[cfg(test)]
mod tests {
    use super::{KindSet, ValueKind, ValueShape, narrowed_shape};
    use crate::module::Module;

    #[test]
    fn narrowed_shape_preserves_container_metadata_on_positive_narrowing() {
        assert_eq!(
            narrowed_shape(&ValueShape::list(KindSet::string()), ValueKind::List, true),
            ValueShape::list(KindSet::string())
        );
        assert_eq!(
            narrowed_shape(&ValueShape::map(KindSet::int()), ValueKind::Map, true),
            ValueShape::map(KindSet::int())
        );
        assert_eq!(
            narrowed_shape(&ValueShape::map_iter(KindSet::bigint()), ValueKind::MapIter, true),
            ValueShape::map_iter(KindSet::bigint())
        );
    }

    #[test]
    fn narrowed_shape_removes_exact_container_kinds_on_negative_narrowing() {
        assert_eq!(
            narrowed_shape(&ValueShape::list(KindSet::string()), ValueKind::List, false),
            ValueShape::scalar(KindSet::empty())
        );
        assert_eq!(
            narrowed_shape(&ValueShape::map(KindSet::int()), ValueKind::Map, false),
            ValueShape::scalar(KindSet::empty())
        );
        assert_eq!(
            narrowed_shape(&ValueShape::map_iter(KindSet::bigint()), ValueKind::MapIter, false),
            ValueShape::scalar(KindSet::empty())
        );
    }

    #[test]
    fn analyze_value_kinds_tracks_try_parse_integer_slots() {
        let src = "fn main() do\n    ok, value, err = string_try_parse_integer(\"12\")\n    print(ok)\n    print(value)\n    print(err)\n    0\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("ok"), Some(&ValueShape::scalar(KindSet::int())));
        assert_eq!(main.variables.get("value"), Some(&ValueShape::scalar(KindSet::int())));
        assert_eq!(main.variables.get("err"), Some(&ValueShape::scalar(KindSet::string())));
        assert_eq!(main.returns.slot(0), Some(KindSet::int()));
    }

    #[test]
    fn analyze_value_kinds_tracks_try_parse_bigint_slots() {
        let src = "fn main() do\n    ok, value, err = string_try_parse_bigint(\"12\")\n    print(ok)\n    print(value)\n    print(err)\n    0\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("ok"), Some(&ValueShape::scalar(KindSet::int())));
        assert_eq!(main.variables.get("value"), Some(&ValueShape::scalar(KindSet::bigint())));
        assert_eq!(main.variables.get("err"), Some(&ValueShape::scalar(KindSet::string())));
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
        let src = std::fs::read_to_string("examples/calculator.expr")
            .expect("calculator example should load");
        let module = Module::try_from_source(&src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("lhs_ok"), Some(&ValueShape::scalar(KindSet::int())));
        assert_eq!(main.variables.get("lhs"), Some(&ValueShape::scalar(KindSet::bigint())));
        assert_eq!(main.variables.get("lhs_err"), Some(&ValueShape::scalar(KindSet::string())));
        assert_eq!(main.variables.get("rhs_ok"), Some(&ValueShape::scalar(KindSet::int())));
        assert_eq!(
            main.variables.get("rhs"),
            Some(&ValueShape::scalar(KindSet::bigint().union(KindSet::int())))
        );
        assert_eq!(main.variables.get("rhs_err"), Some(&ValueShape::scalar(KindSet::string())));
        let apply = analysis.functions.get("apply_and_print").expect("apply_and_print missing");
        assert_eq!(apply.variables.get("lhs"), Some(&ValueShape::scalar(KindSet::bigint())));
        assert_eq!(apply.variables.get("rhs"), Some(&ValueShape::scalar(KindSet::bigint())));
        assert!(apply.returns.slot(0).expect("return slot missing").contains(ValueKind::Int));
        let apply_shift =
            analysis.functions.get("apply_and_print_shift").expect("apply_and_print_shift missing");
        assert_eq!(apply_shift.variables.get("lhs"), Some(&ValueShape::scalar(KindSet::bigint())));
        assert_eq!(apply_shift.variables.get("rhs"), Some(&ValueShape::scalar(KindSet::int())));
    }

    #[test]
    fn analyze_value_kinds_tracks_list_literal_item_kinds() {
        let src = "fn main() do\n    xs = [1, \"a\"]\n    xs\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(
            main.variables.get("xs"),
            Some(&ValueShape::list(KindSet::int().union(KindSet::string())))
        );
        assert_eq!(main.returns, ValueShape::list(KindSet::int().union(KindSet::string())));
    }

    #[test]
    fn analyze_value_kinds_tracks_main_args_as_list_of_strings() {
        let src = "fn main(args) do\n    args\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.inputs.get(0), Some(&ValueShape::list(KindSet::string())));
        assert_eq!(main.variables.get("args"), Some(&ValueShape::list(KindSet::string())));
        assert_eq!(main.returns, ValueShape::list(KindSet::string()));
    }

    #[test]
    fn analyze_value_kinds_tracks_map_value_kinds() {
        let src = "fn main() do\n    m = map_new()\n    map_set(m, \"count\", 1)\n    map_set(m, \"name\", \"x\")\n    value = map_get(m, \"count\")\n    m\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        let values = KindSet::int().union(KindSet::string());
        assert_eq!(main.variables.get("m"), Some(&ValueShape::map(values)));
        assert_eq!(main.variables.get("value"), Some(&ValueShape::scalar(values)));
        assert_eq!(main.returns, ValueShape::map(values));
    }

    #[test]
    fn analyze_value_kinds_tracks_map_try_get_slots_from_map_values() {
        let src = "fn main() do\n    m = map_new()\n    map_set(m, \"name\", \"x\")\n    ok, value, err = map_try_get(m, \"name\")\n    print(ok)\n    print(value)\n    print(err)\n    0\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("ok"), Some(&ValueShape::scalar(KindSet::int())));
        assert_eq!(main.variables.get("value"), Some(&ValueShape::scalar(KindSet::string())));
        assert_eq!(main.variables.get("err"), Some(&ValueShape::scalar(KindSet::string())));
    }

    #[test]
    fn analyze_value_kinds_tracks_map_try_pop_slots_from_map_values() {
        let src = "fn main() do\n    m = map_new()\n    map_set(m, \"name\", \"x\")\n    ok, key, value = map_try_pop(m)\n    print(ok)\n    print(key)\n    print(value)\n    0\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("ok"), Some(&ValueShape::scalar(KindSet::int())));
        assert_eq!(main.variables.get("key"), Some(&ValueShape::scalar(KindSet::string())));
        assert_eq!(main.variables.get("value"), Some(&ValueShape::scalar(KindSet::string())));
    }

    #[test]
    fn analyze_value_kinds_tracks_map_iter_next_slots_from_map_values() {
        let src = "fn main() do\n    m = map_new()\n    map_set(m, \"name\", \"x\")\n    it = map_iter(m)\n    key, value = map_iter_next(it)\n    print(key)\n    print(value)\n    0\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("key"), Some(&ValueShape::scalar(KindSet::string())));
        assert_eq!(main.variables.get("value"), Some(&ValueShape::scalar(KindSet::string())));
    }

    #[test]
    fn analyze_value_kinds_narrows_if_branches_from_is_string() {
        let src = "fn main() do\n    x = 0\n    if 1 do\n        x = \"a\"\n    else\n        x = 1\n    end\n    if is_string(x) do\n        bytes_len(x)\n    else\n        0\n    end\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(
            main.variables.get("x"),
            Some(&ValueShape::scalar(KindSet::string().union(KindSet::int())))
        );
        assert_eq!(main.returns, ValueShape::scalar(KindSet::int()));
    }

    #[test]
    fn analyze_value_kinds_tracks_list_map_item_kinds_for_named_callbacks() {
        let src = "fn double(x) do\n    x * 2\nend\n\nfn main() do\n    xs = [1, 2, 3]\n    ys = list_map(xs, double)\n    ys\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("ys"), Some(&ValueShape::list(KindSet::int())));
        assert_eq!(main.returns, ValueShape::list(KindSet::int()));
    }

    #[test]
    fn analyze_value_kinds_tracks_list_filter_item_kinds() {
        let src = "fn main() do\n    xs = [1, 2, 3]\n    ys = list_filter(xs, fn item -> item > 1 end)\n    ys\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("ys"), Some(&ValueShape::list(KindSet::int())));
        assert_eq!(main.returns, ValueShape::list(KindSet::int()));
    }

    #[test]
    fn analyze_value_kinds_tracks_list_map_item_kinds_for_function_aliases() {
        let src = "fn double(x) do\n    x * 2\nend\n\nfn main() do\n    f = double\n    xs = [1, 2, 3]\n    ys = list_map(xs, f)\n    ys\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("f"), Some(&ValueShape::scalar(KindSet::function())));
        assert_eq!(main.function_bindings.get("f"), Some(&"double".to_string()));
        assert_eq!(main.variables.get("ys"), Some(&ValueShape::list(KindSet::int())));
        assert_eq!(main.returns, ValueShape::list(KindSet::int()));
    }

    #[test]
    fn analyze_value_kinds_tracks_list_map_item_kinds_for_inline_lambdas() {
        let src = "fn main() do\n    xs = [1, 2, 3]\n    ys = list_map(xs, fn item -> item * 2 end)\n    ys\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("ys"), Some(&ValueShape::list(KindSet::int())));
        assert_eq!(main.returns, ValueShape::list(KindSet::int()));
    }

    #[test]
    fn analyze_value_kinds_tracks_list_map_item_kinds_for_lambda_aliases() {
        let src = "fn main() do\n    f = fn item -> item * 2 end\n    xs = [1, 2, 3]\n    ys = list_map(xs, f)\n    ys\nend";
        let module = Module::try_from_source(src).expect("source should parse");
        let analysis = module.analyze_value_kinds().expect("analysis should succeed");
        let main = analysis.functions.get("main").expect("main analysis missing");
        assert_eq!(main.variables.get("f"), Some(&ValueShape::scalar(KindSet::function())));
        assert!(main.function_bindings.get("f").is_some());
        assert_eq!(main.variables.get("ys"), Some(&ValueShape::list(KindSet::int())));
        assert_eq!(main.returns, ValueShape::list(KindSet::int()));
    }
}
