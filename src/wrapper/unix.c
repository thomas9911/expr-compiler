#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VALUE_TAG_INT 1
#define VALUE_TAG_LIST 2
#define VALUE_TAG_STRING 3
#define VALUE_TAG_FUNCTION 4
#define VALUE_TAG_BIGINT 5
#define VALUE_TAG_STRING_ITER 6

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

typedef struct StringHeader {
    size_t len;
    size_t cap;
    uint8_t *ptr;
} StringHeader;

typedef struct BigIntHeader {
    int64_t sign;
    size_t len;
    size_t cap;
    uint32_t *ptr;
} BigIntHeader;

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

static int64_t alloc_value_host(uint8_t tag, int64_t payload) {
    Value *value = (Value *)malloc(sizeof(Value));
    if (value == NULL) {
        runtime_trap("out of arena memory");
    }
    value->tag = tag;
    memset(value->padding, 0, sizeof(value->padding));
    value->payload = payload;
    return (int64_t)(uintptr_t)value;
}

static int64_t new_string_from_bytes(const uint8_t *bytes, size_t len) {
    uint8_t *data = (uint8_t *)malloc(len == 0 ? 1 : len);
    if (data == NULL) {
        runtime_trap("out of arena memory");
    }
    if (len != 0) {
        memcpy(data, bytes, len);
    }

    StringHeader *header = (StringHeader *)malloc(sizeof(StringHeader));
    if (header == NULL) {
        runtime_trap("out of arena memory");
    }
    header->len = len;
    header->cap = len;
    header->ptr = data;
    return alloc_value_host(VALUE_TAG_STRING, (int64_t)(uintptr_t)header);
}

static int64_t new_argv_list(int argc, char **argv) {
    size_t arg_count = argc > 1 ? (size_t)(argc - 1) : 0;
    size_t cap = arg_count == 0 ? 1 : arg_count;
    Value *items = (Value *)malloc(cap * sizeof(Value));
    if (items == NULL) {
        runtime_trap("out of arena memory");
    }

    for (size_t i = 0; i < arg_count; i++) {
        int64_t string = new_string_from_bytes((const uint8_t *)argv[i + 1], strlen(argv[i + 1]));
        items[i] = *value_ptr(string);
    }

    ListHeader *header = (ListHeader *)malloc(sizeof(ListHeader));
    if (header == NULL) {
        runtime_trap("out of arena memory");
    }
    header->ptr = items;
    header->len = arg_count;
    header->cap = cap;
    return alloc_value_host(VALUE_TAG_LIST, (int64_t)(uintptr_t)header);
}

static void print_value_ref(const Value *value) {
    if (value->tag == VALUE_TAG_BIGINT) {
        const BigIntHeader *header = (const BigIntHeader *)(uintptr_t)value->payload;
        if (header->sign == 0 || header->len == 0) {
            printf("0");
            return;
        }
        uint32_t *work = malloc(header->len * sizeof(uint32_t));
        if (work == NULL) {
            runtime_trap("out of arena memory");
        }
        memcpy(work, header->ptr, header->len * sizeof(uint32_t));
        uint32_t chunks[128];
        size_t chunk_len = 0;
        size_t len = header->len;
        while (len > 0) {
            uint64_t rem = 0;
            for (size_t i = len; i-- > 0;) {
                uint64_t cur = (rem << 32) | work[i];
                work[i] = (uint32_t)(cur / 1000000000ULL);
                rem = cur % 1000000000ULL;
            }
            chunks[chunk_len++] = (uint32_t)rem;
            while (len > 0 && work[len - 1] == 0) {
                len--;
            }
        }
        free(work);
        if (header->sign < 0) {
            putchar('-');
        }
        printf("%" PRIu32, chunks[chunk_len - 1]);
        for (size_t i = chunk_len - 1; i-- > 0;) {
            printf("%09" PRIu32, chunks[i]);
        }
        return;
    }
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
        const StringHeader *header = (const StringHeader *)(uintptr_t)value->payload;
        fwrite(header->ptr, 1, header->len, stdout);
        return;
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

int64_t __expr_runtime_oom_host(void) {
    runtime_trap("out of arena memory");
}

extern int64_t __expr_main_i64(int64_t arg_tag, int64_t arg_payload);

int main(int argc, char **argv) {
    int64_t args = new_argv_list(argc, argv);
    const Value *value = value_ptr(args);
    int64_t exit_code = __expr_main_i64((int64_t)value->tag, value->payload);
    if (exit_code < INT32_MIN || exit_code > INT32_MAX) {
        return 1;
    }
    return (int)exit_code;
}
