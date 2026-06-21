use crate::analysis::{KindSet, ValueKind, ValueShape};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodResolutionError {
    UnknownReceiver,
    AmbiguousReceiver(Vec<ValueKind>),
}

pub fn method_target_functions(method: &str) -> Vec<String> {
    supported_method_kinds().into_iter().map(|kind| method_function_name(*kind, method)).collect()
}

pub fn method_target_functions_for_shape(
    shape: &ValueShape,
    method: &str,
) -> Result<Vec<String>, MethodResolutionError> {
    let receiver = exact_receiver_kind(shape)?;
    let mut targets = vec![];
    if receiver == ValueKind::Struct {
        if let Some(struct_name) = shape.struct_name() {
            targets.push(format!("{struct_name}_{method}"));
        }
    }
    targets.push(method_function_name(receiver, method));
    targets.dedup();
    Ok(targets)
}

pub fn resolve_method(shape: &ValueShape, method: &str) -> Result<String, MethodResolutionError> {
    Ok(method_target_functions_for_shape(shape, method)?
        .into_iter()
        .next()
        .expect("method target list should be non-empty"))
}

pub fn exact_receiver_kind(shape: &ValueShape) -> Result<ValueKind, MethodResolutionError> {
    let kinds = shape.scalar_slot();
    let exact = present_kinds(kinds);
    match exact.len() {
        0 => Err(MethodResolutionError::UnknownReceiver),
        1 => Ok(exact[0]),
        _ => {
            if kinds.is_any() {
                Err(MethodResolutionError::UnknownReceiver)
            } else {
                Err(MethodResolutionError::AmbiguousReceiver(exact))
            }
        }
    }
}

pub fn method_function_name(kind: ValueKind, method: &str) -> String {
    format!("{}_{}", method_prefix(kind), method)
}

pub fn supported_method_kinds() -> &'static [ValueKind] {
    &[
        ValueKind::Int,
        ValueKind::BigInt,
        ValueKind::String,
        ValueKind::List,
        ValueKind::Map,
        ValueKind::Struct,
        ValueKind::MapIter,
        ValueKind::Function,
        ValueKind::StringIter,
    ]
}

fn method_prefix(kind: ValueKind) -> &'static str {
    match kind {
        ValueKind::Int => "int",
        ValueKind::BigInt => "bigint",
        ValueKind::String => "string",
        ValueKind::List => "list",
        ValueKind::Map => "map",
        ValueKind::Struct => "struct",
        ValueKind::MapIter => "map_iter",
        ValueKind::Function => "function",
        ValueKind::StringIter => "string_iter",
    }
}

fn present_kinds(kinds: KindSet) -> Vec<ValueKind> {
    supported_method_kinds().iter().copied().filter(|kind| kinds.contains(*kind)).collect()
}
