use std::sync::{Mutex, OnceLock};

const DEFAULT_ARENA_BYTES: usize = 16 * 1024 * 1024;
const LIST_INITIAL_CAPACITY: usize = 4;

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq)]
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
    let mut guard = arena().lock().unwrap_or_else(|_| runtime_trap("arena mutex poisoned"));
    f(&mut guard)
}

fn value_ptr(handle: i64) -> *mut Value {
    if handle == 0 {
        runtime_trap("null value handle");
    }
    handle as usize as *mut Value
}

fn alloc_value(arena: &mut Arena, tag: ValueTag, payload: i64) -> i64 {
    let ptr = arena.alloc(std::mem::size_of::<Value>(), std::mem::align_of::<Value>()) as *mut Value;
    unsafe {
        *ptr = Value {
            tag,
            _padding: [0; 7],
            payload,
        };
    }
    ptr as usize as i64
}

fn as_int(handle: i64) -> i64 {
    let ptr = value_ptr(handle);
    let value = unsafe { &*ptr };
    if value.tag != ValueTag::Int {
        runtime_trap("expected integer value");
    }
    value.payload
}

fn as_list_header_ptr(handle: i64) -> *mut ListHeader {
    let ptr = value_ptr(handle);
    let value = unsafe { &*ptr };
    if value.tag != ValueTag::List {
        runtime_trap("expected list value");
    }
    value.payload as usize as *mut ListHeader
}

fn validate_index(index: i64) -> usize {
    if index < 0 {
        runtime_trap("negative list index");
    }
    index as usize
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
    let mut guard = arena().lock().unwrap_or_else(|_| runtime_trap("arena mutex poisoned"));
    *guard = Arena::with_capacity(bytes);
}

pub fn decode_int(handle: i64) -> Option<i64> {
    if handle == 0 {
        return None;
    }
    let addr = handle as usize;
    let guard = arena().lock().ok()?;
    let base = guard.buf.as_ptr() as usize;
    let end = base.checked_add(guard.buf.len())?;
    if addr < base || addr + std::mem::size_of::<Value>() > end {
        return None;
    }
    drop(guard);

    let ptr = addr as *const Value;
    let value = unsafe { &*ptr };
    (value.tag == ValueTag::Int).then_some(value.payload)
}

pub fn decode_int_or_trap(handle: i64) -> i64 {
    decode_int(handle).unwrap_or_else(|| runtime_trap("expected integer value"))
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_value_int_host(raw: i64) -> i64 {
    new_int(raw)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_value_to_i64_host(handle: i64) -> i64 {
    as_int(handle)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_value_is_truthy_host(handle: i64) -> i64 {
    let ptr = value_ptr(handle);
    let value = unsafe { &*ptr };
    match value.tag {
        ValueTag::Int => i64::from(value.payload != 0),
        ValueTag::List => {
            let header = unsafe { &*(value.payload as usize as *const ListHeader) };
            i64::from(header.len != 0)
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_print_host(handle: i64) -> i64 {
    print_value_inner(handle);
    println!();
    new_int(0)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_add_host(lhs: i64, rhs: i64) -> i64 {
    let value = as_int(lhs)
        .checked_add(as_int(rhs))
        .unwrap_or_else(|| runtime_trap("integer overflow in add"));
    new_int(value)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_subtract_host(lhs: i64, rhs: i64) -> i64 {
    let value = as_int(lhs)
        .checked_sub(as_int(rhs))
        .unwrap_or_else(|| runtime_trap("integer overflow in subtract"));
    new_int(value)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_multiply_host(lhs: i64, rhs: i64) -> i64 {
    let value = as_int(lhs)
        .checked_mul(as_int(rhs))
        .unwrap_or_else(|| runtime_trap("integer overflow in multiply"));
    new_int(value)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_divide_host(lhs: i64, rhs: i64) -> i64 {
    let left = as_int(lhs);
    let right = as_int(rhs);
    if right == 0 {
        runtime_trap("divide by zero");
    }
    let value = left
        .checked_div(right)
        .unwrap_or_else(|| runtime_trap("integer overflow in divide"));
    new_int(value)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_modulo_host(lhs: i64, rhs: i64) -> i64 {
    let left = as_int(lhs);
    let right = as_int(rhs);
    if right == 0 {
        runtime_trap("modulo by zero");
    }
    let value = left
        .checked_rem(right)
        .unwrap_or_else(|| runtime_trap("integer overflow in modulo"));
    new_int(value)
}

fn cmp_to_int(lhs: i64, rhs: i64, f: impl Fn(i64, i64) -> bool) -> i64 {
    new_int(i64::from(f(as_int(lhs), as_int(rhs))))
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_gt_host(lhs: i64, rhs: i64) -> i64 {
    cmp_to_int(lhs, rhs, |x, y| x > y)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_lt_host(lhs: i64, rhs: i64) -> i64 {
    cmp_to_int(lhs, rhs, |x, y| x < y)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_gte_host(lhs: i64, rhs: i64) -> i64 {
    cmp_to_int(lhs, rhs, |x, y| x >= y)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_lte_host(lhs: i64, rhs: i64) -> i64 {
    cmp_to_int(lhs, rhs, |x, y| x <= y)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_eq_host(lhs: i64, rhs: i64) -> i64 {
    cmp_to_int(lhs, rhs, |x, y| x == y)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_ne_host(lhs: i64, rhs: i64) -> i64 {
    cmp_to_int(lhs, rhs, |x, y| x != y)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_new_host() -> i64 {
    with_arena(|arena| {
        let data_ptr = arena.alloc(
            LIST_INITIAL_CAPACITY * std::mem::size_of::<i64>(),
            std::mem::align_of::<i64>(),
        ) as *mut i64;
        let header_ptr =
            arena.alloc(std::mem::size_of::<ListHeader>(), std::mem::align_of::<ListHeader>())
                as *mut ListHeader;

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

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_push_host(handle: i64, value: i64) -> i64 {
    with_arena(|arena| {
        let header_ptr = as_list_header_ptr(handle);
        let header = unsafe { &mut *header_ptr };

        if header.len == header.cap {
            let new_cap = header
                .cap
                .checked_mul(2)
                .unwrap_or_else(|| runtime_trap("list capacity overflow"));
            let new_ptr = arena.alloc(
                new_cap * std::mem::size_of::<i64>(),
                std::mem::align_of::<i64>(),
            ) as *mut i64;
            unsafe {
                std::ptr::copy_nonoverlapping(header.ptr, new_ptr, header.len);
            }
            header.ptr = new_ptr;
            header.cap = new_cap;
        }

        unsafe {
            *header.ptr.add(header.len) = value;
        }
        header.len += 1;

        handle
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_len_host(handle: i64) -> i64 {
    let header_ptr = as_list_header_ptr(handle);
    let header = unsafe { &*header_ptr };
    new_int(header.len as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_get_host(handle: i64, index: i64) -> i64 {
    let header_ptr = as_list_header_ptr(handle);
    let header = unsafe { &*header_ptr };
    let idx = validate_index(as_int(index));
    if idx >= header.len {
        runtime_trap("list index out of bounds");
    }
    unsafe { *header.ptr.add(idx) }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_pop_host(handle: i64) -> i64 {
    let header_ptr = as_list_header_ptr(handle);
    let header = unsafe { &mut *header_ptr };
    if header.len == 0 {
        runtime_trap("list pop from empty list");
    }
    header.len -= 1;
    unsafe { *header.ptr.add(header.len) }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_copy_host(handle: i64) -> i64 {
    with_arena(|arena| {
        let src_ptr = as_list_header_ptr(handle);
        let src = unsafe { &*src_ptr };

        let cap = src.cap.max(LIST_INITIAL_CAPACITY);
        let data_ptr = arena.alloc(cap * std::mem::size_of::<i64>(), std::mem::align_of::<i64>())
            as *mut i64;
        if src.len > 0 {
            unsafe {
                std::ptr::copy_nonoverlapping(src.ptr, data_ptr, src.len);
            }
        }

        let header_ptr =
            arena.alloc(std::mem::size_of::<ListHeader>(), std::mem::align_of::<ListHeader>())
                as *mut ListHeader;
        unsafe {
            *header_ptr = ListHeader {
                ptr: data_ptr,
                len: src.len,
                cap,
            };
        }

        alloc_value(arena, ValueTag::List, header_ptr as usize as i64)
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_print_host(handle: i64) -> i64 {
    print_value_inner(handle);
    println!();
    new_int(0)
}
