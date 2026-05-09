#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define VALUE_TAG_INT 1
#define VALUE_TAG_LIST 2
#define VALUE_TAG_STRING 3
#define VALUE_TAG_FUNCTION 4

typedef struct Value {
    uint8_t tag;
    uint8_t padding[7];
    int64_t payload;
} Value;

typedef struct ListHeader {
    Value *ptr;
    size_t len;
    size_t cap;
} ListHeader;

static const Value PRINT_ZERO = {VALUE_TAG_INT, {0, 0, 0, 0, 0, 0, 0}, 0};

static void runtime_trap(const char *message) {
    fprintf(stderr, "runtime error: %s\n", message);
    abort();
}

static const Value *value_ptr(int64_t handle) {
    if (handle == 0) {
        runtime_trap("null value handle");
    }
    return (const Value *)(uintptr_t)handle;
}

static void print_value_ref(const Value *value) {
    if (value->tag == VALUE_TAG_INT) {
        printf("%" PRId64, value->payload);
        return;
    }
    if (value->tag == VALUE_TAG_LIST) {
        const ListHeader *header = (const ListHeader *)(uintptr_t)value->payload;
        putchar('[');
        for (size_t i = 0; i < header->len; i++) {
            if (i != 0) {
                printf(", ");
            }
            print_value_ref(&header->ptr[i]);
        }
        putchar(']');
        return;
    }
    if (value->tag == VALUE_TAG_STRING) {
        runtime_trap("string values are not supported yet");
    }
    if (value->tag == VALUE_TAG_FUNCTION) {
        runtime_trap("function values are not supported yet");
    }
    runtime_trap("unknown value tag");
}

static void print_value_inner(int64_t handle) {
    const Value *value = value_ptr(handle);
    print_value_ref(value);
}

int64_t __expr_print_host(int64_t handle) {
    print_value_inner(handle);
    putchar('\n');
    return (int64_t)(uintptr_t)&PRINT_ZERO;
}

int64_t __expr_list_print_host(int64_t handle) {
    print_value_inner(handle);
    putchar('\n');
    return (int64_t)(uintptr_t)&PRINT_ZERO;
}
