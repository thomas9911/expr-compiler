use std::sync::{Mutex, OnceLock};

const DEFAULT_ARENA_BYTES: usize = 16 * 1024 * 1024;
const LIST_INITIAL_CAPACITY: usize = 1024;

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq)]
#[allow(dead_code)]
enum ValueTag {
    Int = 1,
    List = 2,
}

#[repr(C)]
struct Value {
    tag: ValueTag,
    _padding: [u8; 7],
    payload: i64,
}

#[repr(C)]
struct ListHeader {
    ptr: *mut i64,
    len: usize,
    cap: usize,
}

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

fn list_header_ptr(handle: i64) -> *mut ListHeader {
    let value = value_ref(handle);
    if value.tag != ValueTag::List {
        runtime_trap("expected list value");
    }
    value.payload as usize as *mut ListHeader
}

fn list_header_ref(handle: i64) -> &'static ListHeader {
    unsafe { &*list_header_ptr(handle) }
}

fn list_header_mut(handle: i64) -> &'static mut ListHeader {
    unsafe { &mut *list_header_ptr(handle) }
}

fn alloc_value(arena: &mut Arena, tag: ValueTag, payload: i64) -> i64 {
    let ptr =
        arena.alloc(std::mem::size_of::<Value>(), std::mem::align_of::<Value>()) as *mut Value;
    unsafe {
        *ptr = Value {
            tag,
            _padding: [0; 7],
            payload,
        };
    }
    ptr as usize as i64
}

fn new_int(value: i64) -> i64 {
    with_arena(|arena| alloc_value(arena, ValueTag::Int, value))
}

fn print_value_inner(handle: i64) {
    let ptr = value_ptr(handle);
    let value = unsafe { &*ptr };
    match value.tag {
        ValueTag::Int => print!("{}", value.payload),
        ValueTag::List => {
            let header = unsafe { &*(value.payload as usize as *const ListHeader) };
            print!("[");
            for i in 0..header.len {
                if i != 0 {
                    print!(", ");
                }
                let item = unsafe { *header.ptr.add(i) };
                print_value_inner(item);
            }
            print!("]");
        }
    }
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

fn truthy(handle: i64) -> bool {
    let value = value_ref(handle);
    match value.tag {
        ValueTag::Int => value.payload != 0,
        ValueTag::List => list_header_ref(handle).len != 0,
    }
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
            .checked_mul(std::mem::size_of::<i64>())
            .unwrap_or_else(|| runtime_trap("list allocation overflow"));
        let data_ptr = arena.alloc(data_bytes, std::mem::align_of::<i64>()) as *mut i64;
        unsafe {
            std::ptr::write_bytes(data_ptr as *mut u8, 0, data_bytes);
        }

        let header_ptr = arena.alloc(
            std::mem::size_of::<ListHeader>(),
            std::mem::align_of::<ListHeader>(),
        ) as *mut ListHeader;
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
            .checked_mul(std::mem::size_of::<i64>())
            .unwrap_or_else(|| runtime_trap("list allocation overflow"));
        let new_data = arena.alloc(data_bytes, std::mem::align_of::<i64>()) as *mut i64;
        unsafe {
            std::ptr::copy_nonoverlapping(header.ptr, new_data, header.len);
            let tail = new_cap.saturating_sub(header.len);
            if tail != 0 {
                std::ptr::write_bytes(new_data.add(header.len) as *mut u8, 0, tail * 8);
            }
        }
        header.ptr = new_data;
        header.cap = new_cap;
    });
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_value_int_host(raw: i64) -> i64 {
    new_int(raw)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_value_to_i64_host(handle: i64) -> i64 {
    expect_int(handle)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_value_is_truthy_host(handle: i64) -> i64 {
    if truthy(handle) { 1 } else { 0 }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_add_host(lhs: i64, rhs: i64) -> i64 {
    let lhs = expect_int(lhs);
    let rhs = expect_int(rhs);
    new_int(
        lhs.checked_add(rhs)
            .unwrap_or_else(|| runtime_trap("integer overflow")),
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_subtract_host(lhs: i64, rhs: i64) -> i64 {
    let lhs = expect_int(lhs);
    let rhs = expect_int(rhs);
    new_int(
        lhs.checked_sub(rhs)
            .unwrap_or_else(|| runtime_trap("integer overflow")),
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_multiply_host(lhs: i64, rhs: i64) -> i64 {
    let lhs = expect_int(lhs);
    let rhs = expect_int(rhs);
    new_int(
        lhs.checked_mul(rhs)
            .unwrap_or_else(|| runtime_trap("integer overflow")),
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_divide_host(lhs: i64, rhs: i64) -> i64 {
    let lhs = expect_int(lhs);
    let rhs = expect_int(rhs);
    if rhs == 0 {
        runtime_trap("division by zero");
    }
    new_int(
        lhs.checked_div(rhs)
            .unwrap_or_else(|| runtime_trap("integer overflow")),
    )
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_modulo_host(lhs: i64, rhs: i64) -> i64 {
    let lhs = expect_int(lhs);
    let rhs = expect_int(rhs);
    if rhs == 0 {
        runtime_trap("division by zero");
    }
    new_int(
        lhs.checked_rem(rhs)
            .unwrap_or_else(|| runtime_trap("integer overflow")),
    )
}

fn compare_boxed(lhs: i64, rhs: i64, pred: impl FnOnce(i64, i64) -> bool) -> i64 {
    let lhs = expect_int(lhs);
    let rhs = expect_int(rhs);
    new_int(if pred(lhs, rhs) { 1 } else { 0 })
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_gt_host(lhs: i64, rhs: i64) -> i64 {
    compare_boxed(lhs, rhs, |lhs, rhs| lhs > rhs)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_lt_host(lhs: i64, rhs: i64) -> i64 {
    compare_boxed(lhs, rhs, |lhs, rhs| lhs < rhs)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_gte_host(lhs: i64, rhs: i64) -> i64 {
    compare_boxed(lhs, rhs, |lhs, rhs| lhs >= rhs)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_lte_host(lhs: i64, rhs: i64) -> i64 {
    compare_boxed(lhs, rhs, |lhs, rhs| lhs <= rhs)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_eq_host(lhs: i64, rhs: i64) -> i64 {
    compare_boxed(lhs, rhs, |lhs, rhs| lhs == rhs)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_ne_host(lhs: i64, rhs: i64) -> i64 {
    compare_boxed(lhs, rhs, |lhs, rhs| lhs != rhs)
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
    unsafe {
        *header.ptr.add(header.len) = value;
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
    unsafe { *header.ptr.add(index) }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_pop_host(list: i64) -> i64 {
    let header = list_header_mut(list);
    if header.len == 0 {
        runtime_trap("list pop on empty list");
    }
    header.len -= 1;
    unsafe { *header.ptr.add(header.len) }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_copy_host(list: i64) -> i64 {
    let header = list_header_ref(list);
    with_arena(|arena| {
        let data_bytes = header
            .cap
            .checked_mul(std::mem::size_of::<i64>())
            .unwrap_or_else(|| runtime_trap("list allocation overflow"));
        let new_data = arena.alloc(data_bytes, std::mem::align_of::<i64>()) as *mut i64;
        unsafe {
            std::ptr::copy_nonoverlapping(header.ptr, new_data, header.len);
            let tail = header.cap.saturating_sub(header.len);
            if tail != 0 {
                std::ptr::write_bytes(new_data.add(header.len) as *mut u8, 0, tail * 8);
            }
        }

        let new_header = arena.alloc(
            std::mem::size_of::<ListHeader>(),
            std::mem::align_of::<ListHeader>(),
        ) as *mut ListHeader;
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
