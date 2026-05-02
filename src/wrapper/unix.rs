use std::sync::{Mutex, OnceLock};

const DEFAULT_ARENA_BYTES: usize = 16 * 1024 * 1024;
const LIST_INITIAL_CAPACITY: usize = 1024;

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

fn value_ptr(handle: i64) -> *mut Value {
    if handle == 0 {
        runtime_trap("null value handle");
    }
    handle as usize as *mut Value
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

#[unsafe(no_mangle)]
pub extern "C" fn __expr_value_int_host(raw: i64) -> i64 {
    new_int(raw)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_print_host(handle: i64) -> i64 {
    print_value_inner(handle);
    println!();
    new_int(0)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_new_host() -> i64 {
    with_arena(|arena| {
        let data_ptr = arena.alloc(
            LIST_INITIAL_CAPACITY * std::mem::size_of::<i64>(),
            std::mem::align_of::<i64>(),
        ) as *mut i64;
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

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_push_host(handle: i64, value: i64) -> i64 {
    with_arena(|arena| {
        let header = unsafe { &mut *as_list_header_ptr(handle) };
        if header.len == header.cap {
            let new_cap = match header.cap.checked_mul(2) {
                Some(value) => value,
                None => runtime_trap("integer overflow"),
            };
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
    let header = unsafe { &*as_list_header_ptr(handle) };
    new_int(i64::try_from(header.len).unwrap_or_else(|_| runtime_trap("integer overflow")))
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_get_host(handle: i64, index: i64) -> i64 {
    let raw_index = as_int(index);
    let idx = usize::try_from(raw_index).unwrap_or_else(|_| runtime_trap("list index out of bounds"));
    let header = unsafe { &*as_list_header_ptr(handle) };
    if idx >= header.len {
        runtime_trap("list index out of bounds");
    }
    unsafe { *header.ptr.add(idx) }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_pop_host(handle: i64) -> i64 {
    let header = unsafe { &mut *as_list_header_ptr(handle) };
    if header.len == 0 {
        runtime_trap("list pop on empty list");
    }
    header.len -= 1;
    unsafe { *header.ptr.add(header.len) }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_copy_host(handle: i64) -> i64 {
    let src = unsafe { &*as_list_header_ptr(handle) };
    with_arena(|arena| {
        let data_ptr = arena.alloc(
            src.cap * std::mem::size_of::<i64>(),
            std::mem::align_of::<i64>(),
        ) as *mut i64;
        if src.len > 0 {
            unsafe {
                std::ptr::copy_nonoverlapping(src.ptr, data_ptr, src.len);
            }
        }
        let header_ptr = arena.alloc(
            std::mem::size_of::<ListHeader>(),
            std::mem::align_of::<ListHeader>(),
        ) as *mut ListHeader;
        unsafe {
            *header_ptr = ListHeader {
                ptr: data_ptr,
                len: src.len,
                cap: src.cap,
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

unsafe extern "C" {
    fn __expr_main() -> i64;
}

fn main() {
    let code = unsafe { __expr_main() };
    let int_code = as_int(code);
    let exit_code = i32::try_from(int_code).unwrap_or(1);
    std::process::exit(exit_code);
}
