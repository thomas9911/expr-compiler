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
