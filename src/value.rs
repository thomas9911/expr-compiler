pub const TAG_INT: i64 = 1;
pub const TAG_LIST: i64 = 2;
pub const TAG_STRING: i64 = 3;
pub const TAG_FUNCTION: i64 = 4;
pub const TAG_BIGINT: i64 = 5;
pub const TAG_STRING_ITER: i64 = 6;
pub const TAG_MULTI: i64 = 7;
pub const TAG_MAP: i64 = 8;

pub const VALUE_SIZE: i64 = 16;
pub const VALUE_PAYLOAD_OFFSET: i32 = 8;
pub const CLOSURE_SIZE: i64 = 16;
pub const CLOSURE_FUNCTION_ORDINAL_OFFSET: i32 = 0;
pub const CLOSURE_ENV_PTR_OFFSET: i32 = 8;
pub const LIST_HEADER_SIZE: i64 = 24;
pub const LIST_PTR_OFFSET: i32 = 0;
pub const LIST_LEN_OFFSET: i32 = 8;
pub const LIST_CAP_OFFSET: i32 = 16;
pub const STRING_HEADER_SIZE: i64 = 24;
pub const STRING_LEN_OFFSET: i32 = 0;
pub const STRING_CAP_OFFSET: i32 = 8;
pub const STRING_PTR_OFFSET: i32 = 16;
pub const STRING_ITER_HEADER_SIZE: i64 = 16;
pub const STRING_ITER_STRING_OFFSET: i32 = 0;
pub const STRING_ITER_INDEX_OFFSET: i32 = 8;
pub const MULTI_HEADER_SIZE: i64 = 16;
pub const MULTI_LEN_OFFSET: i32 = 0;
pub const MULTI_PTR_OFFSET: i32 = 8;
pub const BIGINT_HEADER_SIZE: i64 = 32;
pub const BIGINT_SIGN_OFFSET: i32 = 0;
pub const BIGINT_LEN_OFFSET: i32 = 8;
pub const BIGINT_CAP_OFFSET: i32 = 16;
pub const BIGINT_PTR_OFFSET: i32 = 24;
pub const BIGINT_LIMB_SIZE: i64 = 4;
pub const MAP_HEADER_SIZE: i64 = 24;
pub const MAP_LEN_OFFSET: i32 = 0;
pub const MAP_CAP_OFFSET: i32 = 8;
pub const MAP_PTR_OFFSET: i32 = 16;
pub const MAP_ENTRY_SIZE: i64 = 40;
pub const MAP_ENTRY_HASH_OFFSET: i32 = 0;
pub const MAP_ENTRY_KEY_OFFSET: i32 = 8;
pub const MAP_ENTRY_VALUE_TAG_OFFSET: i32 = 16;
pub const MAP_ENTRY_VALUE_PAYLOAD_OFFSET: i32 = 24;
pub const MAP_ENTRY_STATE_OFFSET: i32 = 32;
pub const MAP_ENTRY_EMPTY: i64 = 0;
pub const MAP_ENTRY_OCCUPIED: i64 = 1;
pub const MAP_ENTRY_TOMBSTONE: i64 = 2;

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ValueTag {
    Int = TAG_INT as u8,
    List = TAG_LIST as u8,
    String = TAG_STRING as u8,
    Function = TAG_FUNCTION as u8,
    BigInt = TAG_BIGINT as u8,
    StringIter = TAG_STRING_ITER as u8,
    Multi = TAG_MULTI as u8,
    Map = TAG_MAP as u8,
}

impl ValueTag {
    pub fn from_raw(raw: u8) -> Option<Self> {
        match raw as i64 {
            TAG_INT => Some(Self::Int),
            TAG_LIST => Some(Self::List),
            TAG_STRING => Some(Self::String),
            TAG_FUNCTION => Some(Self::Function),
            TAG_BIGINT => Some(Self::BigInt),
            TAG_STRING_ITER => Some(Self::StringIter),
            TAG_MULTI => Some(Self::Multi),
            TAG_MAP => Some(Self::Map),
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
pub struct StringHeader {
    pub len: usize,
    pub cap: usize,
    pub ptr: *mut u8,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct StringIterHeader {
    pub string_ptr: i64,
    pub byte_index: i64,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct MultiHeader<T> {
    pub len: usize,
    pub ptr: *mut T,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct BigIntHeader {
    pub sign: i64,
    pub len: usize,
    pub cap: usize,
    pub ptr: *mut u32,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct MapHeader {
    pub len: usize,
    pub cap: usize,
    pub ptr: *mut MapEntry,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct MapEntry {
    pub hash: u64,
    pub key_ptr: i64,
    pub value_tag: i64,
    pub value_payload: i64,
    pub state: i64,
}

#[test]
fn verify_value() {
    assert_eq!(ValueTag::from_raw(TAG_INT as u8).unwrap(), ValueTag::Int);
    assert_eq!(ValueTag::from_raw(TAG_LIST as u8).unwrap(), ValueTag::List);
    assert_eq!(ValueTag::from_raw(TAG_STRING as u8).unwrap(), ValueTag::String);
    assert_eq!(ValueTag::from_raw(TAG_FUNCTION as u8).unwrap(), ValueTag::Function);
    assert_eq!(ValueTag::from_raw(TAG_BIGINT as u8).unwrap(), ValueTag::BigInt);
    assert_eq!(ValueTag::from_raw(TAG_STRING_ITER as u8).unwrap(), ValueTag::StringIter);
    assert_eq!(ValueTag::from_raw(TAG_MULTI as u8).unwrap(), ValueTag::Multi);
    assert_eq!(ValueTag::from_raw(TAG_MAP as u8).unwrap(), ValueTag::Map);
}
