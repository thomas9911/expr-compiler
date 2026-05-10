#![no_std]
#![no_main]

use core::convert::TryFrom;
use core::panic::PanicInfo;
use core::ptr;

#[link(name = "c")]
unsafe extern "C" {
    fn write(fd: i32, buf: *const u8, count: usize) -> isize;
    fn _exit(status: i32) -> !;
}

const STDOUT_FILENO: i32 = 1;
const STDERR_FILENO: i32 = 2;
const ARENA_BYTES: usize = 16 * 1024 * 1024;
const LIST_INITIAL_CAPACITY: usize = 1024;

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq)]
enum ValueTag {
    Int = 1,
    List = 2,
    String = 3,
    Function = 4,
}

#[repr(C)]
#[derive(Copy, Clone)]
struct Value {
    tag: ValueTag,
    _padding: [u8; 7],
    payload: i64,
}

#[repr(C)]
struct ListHeader {
    ptr: *mut Value,
    len: usize,
    cap: usize,
}

static mut ARENA: [u8; ARENA_BYTES] = [0; ARENA_BYTES];
static mut ARENA_OFFSET: usize = 0;

fn runtime_abort() -> ! {
    unsafe {
        _exit(1);
    }
}

fn align_up(value: usize, align: usize) -> usize {
    (value + (align - 1)) & !(align - 1)
}

unsafe fn arena_alloc(size: usize, align: usize) -> *mut u8 {
    let base = core::ptr::addr_of_mut!(ARENA) as *mut u8 as usize;
    let start = align_up(base + ARENA_OFFSET, align) - base;
    let end = match start.checked_add(size) {
        Some(v) => v,
        None => runtime_abort(),
    };
    if end > ARENA_BYTES {
        runtime_abort();
    }
    ARENA_OFFSET = end;
    core::ptr::addr_of_mut!(ARENA).cast::<u8>().add(start)
}

fn value_ptr(handle: i64) -> *mut Value {
    if handle == 0 {
        runtime_abort();
    }
    handle as usize as *mut Value
}

unsafe fn alloc_value(tag: ValueTag, payload: i64) -> i64 {
    let ptr =
        arena_alloc(core::mem::size_of::<Value>(), core::mem::align_of::<Value>()) as *mut Value;
    (*ptr).tag = tag;
    (*ptr)._padding = [0; 7];
    (*ptr).payload = payload;
    ptr as usize as i64
}

fn as_int(handle: i64) -> i64 {
    unsafe {
        let ptr = value_ptr(handle);
        if (*ptr).tag != ValueTag::Int {
            runtime_abort();
        }
        (*ptr).payload
    }
}

fn as_list_header_ptr(handle: i64) -> *mut ListHeader {
    unsafe {
        let ptr = value_ptr(handle);
        if (*ptr).tag != ValueTag::List {
            runtime_abort();
        }
        (*ptr).payload as usize as *mut ListHeader
    }
}

fn write_fd(fd: i32, buf: &[u8]) {
    let mut offset = 0usize;
    while offset < buf.len() {
        let written = unsafe { write(fd, buf.as_ptr().add(offset), buf.len() - offset) };
        if written <= 0 {
            runtime_abort();
        }
        offset += written as usize;
    }
}

fn write_stdout(buf: &[u8]) {
    write_fd(STDOUT_FILENO, buf);
}

fn write_stderr(buf: &[u8]) {
    write_fd(STDERR_FILENO, buf);
}

fn runtime_trap(message: &str) -> ! {
    write_stderr(b"runtime error: ");
    write_stderr(message.as_bytes());
    write_stderr(b"\n");
    runtime_abort()
}

fn write_i64(n: i64) {
    let negative = n < 0;
    let mut value = if negative {
        (-(n as i128)) as u128
    } else {
        n as u128
    };

    let mut rev_digits = [0u8; 20];
    let mut rev_len = 0usize;
    loop {
        rev_digits[rev_len] = b'0' + (value % 10) as u8;
        rev_len += 1;
        value /= 10;
        if value == 0 {
            break;
        }
    }

    let mut out = [0u8; 21];
    let mut idx = 0usize;
    if negative {
        out[idx] = b'-';
        idx += 1;
    }
    let mut i = rev_len;
    while i > 0 {
        i -= 1;
        out[idx] = rev_digits[i];
        idx += 1;
    }
    write_stdout(&out[..idx]);
}

fn print_value_inner(handle: i64) {
    unsafe fn print_inline_value(value: &Value) {
        match value.tag {
            ValueTag::Int => write_i64(value.payload),
            ValueTag::List => {
                let header = &*(value.payload as usize as *const ListHeader);
                write_stdout(b"[");
                let mut i = 0usize;
                while i < header.len {
                    if i != 0 {
                        write_stdout(b", ");
                    }
                    print_inline_value(&*header.ptr.add(i));
                    i += 1;
                }
                write_stdout(b"]");
            }
            ValueTag::String => runtime_abort(),
            ValueTag::Function => runtime_abort(),
        }
    }

    unsafe {
        let ptr = value_ptr(handle);
        print_inline_value(&*ptr);
    }
}

fn new_int(value: i64) -> i64 {
    unsafe { alloc_value(ValueTag::Int, value) }
}

#[unsafe(no_mangle)]
pub extern "C" fn memcpy(dst: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    let mut i = 0usize;
    unsafe {
        while i < n {
            *dst.add(i) = *src.add(i);
            i += 1;
        }
    }
    dst
}

#[unsafe(no_mangle)]
pub extern "C" fn memset(dst: *mut u8, value: i32, n: usize) -> *mut u8 {
    let mut i = 0usize;
    let byte = value as u8;
    unsafe {
        while i < n {
            *dst.add(i) = byte;
            i += 1;
        }
    }
    dst
}

#[unsafe(no_mangle)]
pub extern "C" fn memcmp(a: *const u8, b: *const u8, n: usize) -> i32 {
    let mut i = 0usize;
    unsafe {
        while i < n {
            let x = *a.add(i);
            let y = *b.add(i);
            if x != y {
                return x as i32 - y as i32;
            }
            i += 1;
        }
    }
    0
}

#[unsafe(no_mangle)]
pub extern "C" fn rust_eh_personality() {}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_alloc_host(size: i64, align: i64) -> i64 {
    let size = usize::try_from(size).unwrap_or_else(|_| runtime_abort());
    let align = usize::try_from(align).unwrap_or_else(|_| runtime_abort());
    if align == 0 || !align.is_power_of_two() {
        runtime_abort();
    }
    unsafe { arena_alloc(size, align) as usize as i64 }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_box_value_host(tag: i64, payload: i64) -> i64 {
    match tag {
        1 => unsafe { alloc_value(ValueTag::Int, payload) },
        2 => unsafe { alloc_value(ValueTag::List, payload) },
        3 => unsafe { alloc_value(ValueTag::String, payload) },
        4 => unsafe { alloc_value(ValueTag::Function, payload) },
        _ => runtime_abort(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_runtime_oom_host() -> i64 {
    runtime_trap("out of arena memory");
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_print_host(handle: i64) -> i64 {
    print_value_inner(handle);
    write_stdout(b"\n");
    new_int(0)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_new_host() -> i64 {
    unsafe {
        let data_ptr = arena_alloc(
            LIST_INITIAL_CAPACITY * core::mem::size_of::<Value>(),
            core::mem::align_of::<Value>(),
        ) as *mut Value;
        let header_ptr = arena_alloc(
            core::mem::size_of::<ListHeader>(),
            core::mem::align_of::<ListHeader>(),
        ) as *mut ListHeader;
        (*header_ptr).ptr = data_ptr;
        (*header_ptr).len = 0;
        (*header_ptr).cap = LIST_INITIAL_CAPACITY;
        alloc_value(ValueTag::List, header_ptr as usize as i64)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_push_host(handle: i64, value: i64) -> i64 {
    unsafe {
        let header = &mut *as_list_header_ptr(handle);
        if header.len == header.cap {
            let new_cap = match header.cap.checked_mul(2) {
                Some(v) => v,
                None => runtime_trap("integer overflow"),
            };
            let new_ptr = arena_alloc(
                new_cap * core::mem::size_of::<Value>(),
                core::mem::align_of::<Value>(),
            ) as *mut Value;
            ptr::copy_nonoverlapping(header.ptr, new_ptr, header.len);
            header.ptr = new_ptr;
            header.cap = new_cap;
        }
        *header.ptr.add(header.len) = *value_ptr(value);
        header.len += 1;
        handle
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_insert_host(handle: i64, index: i64, value: i64) -> i64 {
    let raw_index = as_int(index);
    if raw_index < 0 {
        runtime_trap("list index out of bounds");
    }
    let idx = raw_index as usize;
    unsafe {
        let header = &*as_list_header_ptr(handle);
        if idx > header.len {
            runtime_trap("list index out of bounds");
        }
        if header.len == header.cap {
            let new_cap = match header.cap.checked_mul(2) {
                Some(v) => v,
                None => runtime_trap("integer overflow"),
            };
            let new_ptr = arena_alloc(
                new_cap * core::mem::size_of::<Value>(),
                core::mem::align_of::<Value>(),
            ) as *mut Value;
            ptr::copy_nonoverlapping(header.ptr, new_ptr, header.len);
            let header_mut = &mut *as_list_header_ptr(handle);
            header_mut.ptr = new_ptr;
            header_mut.cap = new_cap;
        }
        let header = &mut *as_list_header_ptr(handle);
        let mut pos = header.len;
        while pos > idx {
            *header.ptr.add(pos) = *header.ptr.add(pos - 1);
            pos -= 1;
        }
        *header.ptr.add(idx) = *value_ptr(value);
        header.len += 1;
        handle
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_len_host(handle: i64) -> i64 {
    let header = unsafe { &*as_list_header_ptr(handle) };
    if header.len > i64::MAX as usize {
        runtime_trap("integer overflow");
    }
    new_int(header.len as i64)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_get_host(handle: i64, index: i64) -> i64 {
    let raw_index = as_int(index);
    if raw_index < 0 {
        runtime_trap("list index out of bounds");
    }
    let idx = raw_index as usize;
    let header = unsafe { &*as_list_header_ptr(handle) };
    if idx >= header.len {
        runtime_trap("list index out of bounds");
    }
    unsafe {
        let value = *header.ptr.add(idx);
        alloc_value(value.tag, value.payload)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_set_host(handle: i64, index: i64, value: i64) -> i64 {
    let raw_index = as_int(index);
    if raw_index < 0 {
        runtime_trap("list index out of bounds");
    }
    let idx = raw_index as usize;
    let header = unsafe { &mut *as_list_header_ptr(handle) };
    if idx >= header.len {
        runtime_trap("list index out of bounds");
    }
    unsafe {
        let value = *value_ptr(value);
        *header.ptr.add(idx) = value;
        alloc_value(value.tag, value.payload)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_swap_host(handle: i64, index_a: i64, index_b: i64) -> i64 {
    let raw_index_a = as_int(index_a);
    let raw_index_b = as_int(index_b);
    if raw_index_a < 0 || raw_index_b < 0 {
        runtime_trap("list index out of bounds");
    }
    let idx_a = raw_index_a as usize;
    let idx_b = raw_index_b as usize;
    let header = unsafe { &mut *as_list_header_ptr(handle) };
    if idx_a >= header.len || idx_b >= header.len {
        runtime_trap("list index out of bounds");
    }
    unsafe {
        ptr::swap(header.ptr.add(idx_a), header.ptr.add(idx_b));
    }
    handle
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_pop_host(handle: i64) -> i64 {
    let header = unsafe { &mut *as_list_header_ptr(handle) };
    if header.len == 0 {
        runtime_trap("list pop on empty list");
    }
    header.len -= 1;
    unsafe {
        let value = *header.ptr.add(header.len);
        alloc_value(value.tag, value.payload)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_copy_host(handle: i64) -> i64 {
    unsafe {
        let src = &*as_list_header_ptr(handle);
        let data_ptr = arena_alloc(
            src.cap * core::mem::size_of::<Value>(),
            core::mem::align_of::<Value>(),
        ) as *mut Value;
        if src.len > 0 {
            ptr::copy_nonoverlapping(src.ptr, data_ptr, src.len);
        }
        let header_ptr = arena_alloc(
            core::mem::size_of::<ListHeader>(),
            core::mem::align_of::<ListHeader>(),
        ) as *mut ListHeader;
        (*header_ptr).ptr = data_ptr;
        (*header_ptr).len = src.len;
        (*header_ptr).cap = src.cap;
        alloc_value(ValueTag::List, header_ptr as usize as i64)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_print_host(handle: i64) -> i64 {
    print_value_inner(handle);
    write_stdout(b"\n");
    new_int(0)
}

unsafe extern "C" {
    fn __expr_main_i64() -> i64;
}

#[unsafe(no_mangle)]
pub extern "C" fn main() -> i32 {
    let int_code = unsafe { __expr_main_i64() };
    if int_code < i32::MIN as i64 || int_code > i32::MAX as i64 {
        1
    } else {
        int_code as i32
    }
}

#[panic_handler]
fn panic(_info: &PanicInfo<'_>) -> ! {
    runtime_abort()
}
