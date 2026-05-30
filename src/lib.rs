// pub mod compiler;
pub mod analysis;
pub mod module;
pub mod parser;
pub mod runtime;
pub mod source;
pub mod tokenizer;
pub mod value;

#[cfg(test)]
use crate::{module::Module, runtime::reset_runtime_arena};

#[test]
fn jit_smoke_test() {
    reset_runtime_arena();
    let jit = Module::from_source("fn main() do\n    1 + 2 * 3\nend").compile_to_jit();
    let ptr = jit.get_int_result_fn_ptr("main").expect("cranelift int-result wrapper should exist");
    let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
    assert_eq!(func(), 7);
}
