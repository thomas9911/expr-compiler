pub const TAG_INT: i64 = 1;
pub const TAG_LIST: i64 = 2;
pub const TAG_STRING: i64 = 3;
pub const TAG_FUNCTION: i64 = 4;
pub const TAG_BIGINT: i64 = 5;

pub const VALUE_SIZE: i64 = 16;
pub const VALUE_PAYLOAD_OFFSET: i32 = 8;
pub const CLOSURE_SIZE: i64 = 16;
pub const CLOSURE_FUNCTION_ORDINAL_OFFSET: i32 = 0;
pub const CLOSURE_ENV_PTR_OFFSET: i32 = 8;
pub const LIST_HEADER_SIZE: i64 = 24;
pub const LIST_PTR_OFFSET: i32 = 0;
pub const LIST_LEN_OFFSET: i32 = 8;
pub const LIST_CAP_OFFSET: i32 = 16;
pub const BIGINT_HEADER_SIZE: i64 = 32;
pub const BIGINT_SIGN_OFFSET: i32 = 0;
pub const BIGINT_LEN_OFFSET: i32 = 8;
pub const BIGINT_CAP_OFFSET: i32 = 16;
pub const BIGINT_PTR_OFFSET: i32 = 24;
pub const BIGINT_LIMB_SIZE: i64 = 4;

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ValueTag {
    Int = TAG_INT as u8,
    List = TAG_LIST as u8,
    String = TAG_STRING as u8,
    Function = TAG_FUNCTION as u8,
    BigInt = TAG_BIGINT as u8,
}

impl ValueTag {
    pub fn from_raw(raw: u8) -> Option<Self> {
        match raw as i64 {
            TAG_INT => Some(Self::Int),
            TAG_LIST => Some(Self::List),
            TAG_STRING => Some(Self::String),
            TAG_FUNCTION => Some(Self::Function),
            TAG_BIGINT => Some(Self::BigInt),
            _ => None,
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct Value {
    pub tag: ValueTag,
    pub padding: [u8; 7],
    pub payload: i64,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct ListHeader<T> {
    pub ptr: *mut T,
    pub len: usize,
    pub cap: usize,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct BigIntHeader {
    pub sign: i64,
    pub len: usize,
    pub cap: usize,
    pub ptr: *mut u32,
}
