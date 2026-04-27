use std::sync::{Mutex, OnceLock};

const DEFAULT_ARENA_BYTES: usize = 16 * 1024 * 1024;

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
    let value = unsafe { &*value_ptr(handle) };
    (value.tag == ValueTag::Int).then_some(value.payload)
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
pub extern "C" fn __expr_list_print_host(handle: i64) -> i64 {
    print_value_inner(handle);
    println!();
    new_int(0)
}
