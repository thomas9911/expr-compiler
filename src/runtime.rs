use std::sync::{Mutex, OnceLock};

use crate::value::{
    BigIntHeader, ListHeader, StringHeader, TAG_BIGINT, TAG_FUNCTION, TAG_INT, TAG_LIST,
    TAG_STRING, TAG_STRING_ITER, Value, ValueTag,
};

const DEFAULT_ARENA_BYTES: usize = 16 * 1024 * 1024;
const LIST_INITIAL_CAPACITY: usize = 1024;

struct Arena {
    buf: Vec<u8>,
    offset: usize,
}

impl Arena {
    fn with_capacity(bytes: usize) -> Self {
        Self {
            buf: vec![0; bytes],
            offset: 0,
        }
    }

    fn reset(&mut self) {
        self.offset = 0;
    }

    fn alloc(&mut self, size: usize, align: usize) -> *mut u8 {
        let base = self.buf.as_ptr() as usize;
        let start = align_up(base + self.offset, align) - base;
        let end = start
            .checked_add(size)
            .unwrap_or_else(|| runtime_trap("arena allocation overflow"));
        if end > self.buf.len() {
            runtime_trap("out of arena memory");
        }
        self.offset = end;
        unsafe { self.buf.as_mut_ptr().add(start) }
    }
}

fn align_up(value: usize, align: usize) -> usize {
    debug_assert!(align.is_power_of_two());
    (value + (align - 1)) & !(align - 1)
}

fn runtime_trap(message: &str) -> ! {
    eprintln!("runtime error: {message}");
    std::process::abort();
}

fn arena() -> &'static Mutex<Arena> {
    static ARENA: OnceLock<Mutex<Arena>> = OnceLock::new();
    ARENA.get_or_init(|| Mutex::new(Arena::with_capacity(DEFAULT_ARENA_BYTES)))
}

fn with_arena<T>(f: impl FnOnce(&mut Arena) -> T) -> T {
    let mut guard = arena()
        .lock()
        .unwrap_or_else(|_| runtime_trap("arena mutex poisoned"));
    f(&mut guard)
}

fn value_ptr(handle: i64) -> *const Value {
    if handle == 0 {
        runtime_trap("null value handle");
    }
    handle as usize as *const Value
}

fn value_ref(handle: i64) -> &'static Value {
    unsafe { &*value_ptr(handle) }
}

fn list_header_ptr(handle: i64) -> *mut ListHeader<Value> {
    let value = value_ref(handle);
    if value.tag != ValueTag::List {
        runtime_trap("expected list value");
    }
    value.payload as usize as *mut ListHeader<Value>
}

fn list_header_ref(handle: i64) -> &'static ListHeader<Value> {
    unsafe { &*list_header_ptr(handle) }
}

fn list_header_mut(handle: i64) -> &'static mut ListHeader<Value> {
    unsafe { &mut *list_header_ptr(handle) }
}

fn alloc_value(arena: &mut Arena, tag: ValueTag, payload: i64) -> i64 {
    let ptr =
        arena.alloc(std::mem::size_of::<Value>(), std::mem::align_of::<Value>()) as *mut Value;
    unsafe {
        *ptr = Value {
            tag,
            padding: [0; 7],
            payload,
        };
    }
    ptr as usize as i64
}

fn new_int(value: i64) -> i64 {
    with_arena(|arena| alloc_value(arena, ValueTag::Int, value))
}

fn print_bigint_ref(header: &BigIntHeader) {
    if header.sign == 0 || header.len == 0 {
        print!("0");
        return;
    }

    let limbs = unsafe { std::slice::from_raw_parts(header.ptr, header.len) };
    let mut work = limbs.to_vec();
    let mut chunks = Vec::new();
    const BASE10: u64 = 1_000_000_000;

    while !work.is_empty() {
        let mut rem = 0u64;
        for limb in work.iter_mut().rev() {
            let cur = (rem << 32) | u64::from(*limb);
            *limb = (cur / BASE10) as u32;
            rem = cur % BASE10;
        }
        chunks.push(rem as u32);
        while work.last() == Some(&0) {
            work.pop();
        }
    }

    if header.sign < 0 {
        print!("-");
    }
    let mut iter = chunks.iter().rev();
    if let Some(first) = iter.next() {
        print!("{first}");
    }
    for chunk in iter {
        print!("{chunk:09}");
    }
}

fn print_string_ref(header: &StringHeader) {
    let bytes = unsafe { std::slice::from_raw_parts(header.ptr, header.len) };
    print!("{}", String::from_utf8_lossy(bytes));
}

fn print_value_ref(value: &Value) {
    match value.tag {
        ValueTag::Int => print!("{}", value.payload),
        ValueTag::List => {
            let header = unsafe { &*(value.payload as usize as *const ListHeader<Value>) };
            print!("[");
            for i in 0..header.len {
                if i != 0 {
                    print!(", ");
                }
                let item = unsafe { &*header.ptr.add(i) };
                print_value_ref(item);
            }
            print!("]");
        }
        ValueTag::String => {
            let header = unsafe { &*(value.payload as usize as *const StringHeader) };
            print_string_ref(header);
        }
        ValueTag::Function => runtime_trap("function values are not supported here yet"),
        ValueTag::BigInt => {
            let header = unsafe { &*(value.payload as usize as *const BigIntHeader) };
            print_bigint_ref(header);
        }
        ValueTag::StringIter => runtime_trap("string iterators are not printable"),
    }
}

fn print_value_inner(handle: i64) {
    let ptr = value_ptr(handle);
    let value = unsafe { &*ptr };
    print_value_ref(value);
}

fn box_inline_value(value: Value) -> i64 {
    with_arena(|arena| alloc_value(arena, value.tag, value.payload))
}

pub fn reset_runtime_arena() {
    with_arena(|arena| arena.reset());
}

pub fn configure_runtime_arena(bytes: usize) {
    if bytes == 0 {
        runtime_trap("arena size must be > 0");
    }
    let mut guard = arena()
        .lock()
        .unwrap_or_else(|_| runtime_trap("arena mutex poisoned"));
    *guard = Arena::with_capacity(bytes);
}

pub fn jit_arena_addresses() -> (i64, i64) {
    with_arena(|arena| {
        let base = arena.buf.as_mut_ptr() as usize as i64;
        let offset = (&mut arena.offset as *mut usize) as usize as i64;
        (base, offset)
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_runtime_oom_host() -> i64 {
    runtime_trap("out of arena memory");
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_alloc_host(size: i64, align: i64) -> i64 {
    let size = usize::try_from(size).unwrap_or_else(|_| runtime_trap("allocation size overflow"));
    let align =
        usize::try_from(align).unwrap_or_else(|_| runtime_trap("allocation align overflow"));
    if align == 0 || !align.is_power_of_two() {
        runtime_trap("allocation align must be a non-zero power of two");
    }
    with_arena(|arena| arena.alloc(size, align) as usize as i64)
}

pub fn decode_int(handle: i64) -> Option<i64> {
    if handle == 0 {
        return None;
    }
    let addr = usize::try_from(handle).ok()?;
    let guard = arena().lock().ok()?;
    let base = guard.buf.as_ptr() as usize;
    let end = base.checked_add(guard.buf.len())?;
    let value_end = addr.checked_add(std::mem::size_of::<Value>())?;
    if addr < base || value_end > end {
        return None;
    }
    drop(guard);

    let value = unsafe { &*(addr as *const Value) };
    (value.tag == ValueTag::Int).then_some(value.payload)
}

fn expect_int(handle: i64) -> i64 {
    decode_int(handle).unwrap_or_else(|| runtime_trap("expected integer value"))
}

fn raw_to_index(raw: i64) -> usize {
    usize::try_from(raw).unwrap_or_else(|_| runtime_trap("list index out of bounds"))
}

fn usize_to_i64(raw: usize) -> i64 {
    i64::try_from(raw).unwrap_or_else(|_| runtime_trap("integer conversion overflow"))
}

fn new_list_handle() -> i64 {
    with_arena(|arena| {
        let data_bytes = LIST_INITIAL_CAPACITY
            .checked_mul(std::mem::size_of::<Value>())
            .unwrap_or_else(|| runtime_trap("list allocation overflow"));
        let data_ptr = arena.alloc(data_bytes, std::mem::align_of::<Value>()) as *mut Value;
        unsafe {
            std::ptr::write_bytes(data_ptr as *mut u8, 0, data_bytes);
        }

        let header_ptr = arena.alloc(
            std::mem::size_of::<ListHeader<Value>>(),
            std::mem::align_of::<ListHeader<Value>>(),
        ) as *mut ListHeader<Value>;
        unsafe {
            *header_ptr = ListHeader {
                ptr: data_ptr,
                len: 0,
                cap: LIST_INITIAL_CAPACITY,
            };
        }

        alloc_value(arena, ValueTag::List, header_ptr as usize as i64)
    })
}

fn list_grow(handle: i64, new_cap: usize) {
    with_arena(|arena| {
        let header = list_header_mut(handle);
        let data_bytes = new_cap
            .checked_mul(std::mem::size_of::<Value>())
            .unwrap_or_else(|| runtime_trap("list allocation overflow"));
        let new_data = arena.alloc(data_bytes, std::mem::align_of::<Value>()) as *mut Value;
        unsafe {
            std::ptr::copy_nonoverlapping(header.ptr, new_data, header.len);
            let tail = new_cap.saturating_sub(header.len);
            if tail != 0 {
                std::ptr::write_bytes(
                    new_data.add(header.len) as *mut u8,
                    0,
                    tail * std::mem::size_of::<Value>(),
                );
            }
        }
        header.ptr = new_data;
        header.cap = new_cap;
    });
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_box_value_host(tag: i64, payload: i64) -> i64 {
    match tag {
        TAG_INT => new_int(payload),
        TAG_LIST => with_arena(|arena| alloc_value(arena, ValueTag::List, payload)),
        TAG_STRING => with_arena(|arena| alloc_value(arena, ValueTag::String, payload)),
        TAG_FUNCTION => with_arena(|arena| alloc_value(arena, ValueTag::Function, payload)),
        TAG_BIGINT => with_arena(|arena| alloc_value(arena, ValueTag::BigInt, payload)),
        TAG_STRING_ITER => with_arena(|arena| alloc_value(arena, ValueTag::StringIter, payload)),
        _ => runtime_trap("unknown value tag"),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_new_host() -> i64 {
    new_list_handle()
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_push_host(list: i64, value: i64) -> i64 {
    let header = list_header_ref(list);
    if header.len == header.cap {
        let new_cap = header
            .cap
            .checked_mul(2)
            .unwrap_or_else(|| runtime_trap("integer overflow"));
        list_grow(list, new_cap);
    }

    let header = list_header_mut(list);
    let value = *value_ref(value);
    unsafe {
        *header.ptr.add(header.len) = value;
    }
    header.len += 1;
    list
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_insert_host(list: i64, index: i64, value: i64) -> i64 {
    let index = raw_to_index(expect_int(index));
    let header = list_header_ref(list);
    if index > header.len {
        runtime_trap("list index out of bounds");
    }
    if header.len == header.cap {
        let new_cap = header
            .cap
            .checked_mul(2)
            .unwrap_or_else(|| runtime_trap("integer overflow"));
        list_grow(list, new_cap);
    }

    let header = list_header_mut(list);
    let value = *value_ref(value);
    unsafe {
        let dst = header.ptr.add(index + 1);
        let src = header.ptr.add(index);
        let count = header.len - index;
        std::ptr::copy(src, dst, count);
        *header.ptr.add(index) = value;
    }
    header.len += 1;
    list
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_len_host(list: i64) -> i64 {
    new_int(usize_to_i64(list_header_ref(list).len))
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_get_host(list: i64, index: i64) -> i64 {
    let index = raw_to_index(expect_int(index));
    let header = list_header_ref(list);
    if index >= header.len {
        runtime_trap("list index out of bounds");
    }
    unsafe { box_inline_value(*header.ptr.add(index)) }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_set_host(list: i64, index: i64, value: i64) -> i64 {
    let index = raw_to_index(expect_int(index));
    let header = list_header_mut(list);
    if index >= header.len {
        runtime_trap("list index out of bounds");
    }
    let value = *value_ref(value);
    unsafe {
        *header.ptr.add(index) = value;
    }
    box_inline_value(value)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_swap_host(list: i64, index_a: i64, index_b: i64) -> i64 {
    let index_a = raw_to_index(expect_int(index_a));
    let index_b = raw_to_index(expect_int(index_b));
    let header = list_header_mut(list);
    if index_a >= header.len || index_b >= header.len {
        runtime_trap("list index out of bounds");
    }
    unsafe {
        let ptr_a = header.ptr.add(index_a);
        let ptr_b = header.ptr.add(index_b);
        std::ptr::swap(ptr_a, ptr_b);
    }
    list
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_pop_host(list: i64) -> i64 {
    let header = list_header_mut(list);
    if header.len == 0 {
        runtime_trap("list pop on empty list");
    }
    header.len -= 1;
    unsafe { box_inline_value(*header.ptr.add(header.len)) }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_copy_host(list: i64) -> i64 {
    let header = list_header_ref(list);
    with_arena(|arena| {
        let data_bytes = header
            .cap
            .checked_mul(std::mem::size_of::<Value>())
            .unwrap_or_else(|| runtime_trap("list allocation overflow"));
        let new_data = arena.alloc(data_bytes, std::mem::align_of::<Value>()) as *mut Value;
        unsafe {
            std::ptr::copy_nonoverlapping(header.ptr, new_data, header.len);
            let tail = header.cap.saturating_sub(header.len);
            if tail != 0 {
                std::ptr::write_bytes(
                    new_data.add(header.len) as *mut u8,
                    0,
                    tail * std::mem::size_of::<Value>(),
                );
            }
        }

        let new_header = arena.alloc(
            std::mem::size_of::<ListHeader<Value>>(),
            std::mem::align_of::<ListHeader<Value>>(),
        ) as *mut ListHeader<Value>;
        unsafe {
            *new_header = ListHeader {
                ptr: new_data,
                len: header.len,
                cap: header.cap,
            };
        }

        alloc_value(arena, ValueTag::List, new_header as usize as i64)
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_print_host(handle: i64) -> i64 {
    print_value_inner(handle);
    println!();
    new_int(0)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_print_host(handle: i64) -> i64 {
    print_value_inner(handle);
    println!();
    new_int(0)
}

#[cfg(test)]
mod tests {
    use super::decode_int;

    #[test]
    fn decode_int_rejects_non_runtime_pointer() {
        assert_eq!(decode_int(1), None);
    }
}
