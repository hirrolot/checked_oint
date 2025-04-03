#include "int128_support.c"
#include "int_macros.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#ifndef NDEBUG

static bool is_valid_base(const int base) {
    switch (base) {
    case 2:
    case 8:
    case 10:
    case 16: return true;
    default: return false;
    }
}

#endif // NDEBUG

#define X(namespace, f, max_int_ty)                                            \
    static int impl_##namespace##_scan_exn(                                    \
        const char s[const restrict], C_INT_TY(namespace) *const restrict x,   \
        const int base) {                                                      \
        if (isspace(s[0]) || '\0' == s[0]) {                                   \
            return -1;                                                         \
        }                                                                      \
                                                                               \
        errno = 0;                                                             \
        char *end = NULL;                                                      \
        const max_int_ty result = f(s, &end, base);                            \
        if (end[0] != '\0') {                                                  \
            /* There are some uninterpreted characters left. */                \
            return -1;                                                         \
        }                                                                      \
        _Pragma("GCC diagnostic push");                                        \
        _Pragma("GCC diagnostic ignored \"-Wtype-limits\"");                   \
        if (ERANGE == errno || result < C_INT_MIN(namespace) ||                \
            result > C_INT_MAX(namespace)) {                                   \
            return -1;                                                         \
        }                                                                      \
        _Pragma("GCC diagnostic pop");                                         \
                                                                               \
        *x = (C_INT_TY(namespace))result;                                      \
        return 0;                                                              \
    }

X(int, strtoimax, intmax_t)
X(u32, strtoumax, uintmax_t)
X(u64, strtoumax, uintmax_t)
X(i32, strtoimax, intmax_t)
X(i64, strtoimax, intmax_t)

#undef X

#define X(namespace)                                                           \
    value checked_oint_##namespace##_bit_not(value x) {                        \
        CAMLparam1(x);                                                         \
        CAMLreturn(Val_int((int)~C_VALUE(namespace, x)));                      \
    }

X(i8)
X(i16)

#undef X

#define X(namespace, name, op)                                                 \
    value checked_oint_##namespace##_##name(value x, value y) {                \
        CAMLparam2(x, y);                                                      \
        assert(Int_val(y) >= 0);                                               \
        assert(Int_val(y) < (int)C_INT_BIT_WIDTH(namespace));                  \
                                                                               \
        CAMLreturn(                                                            \
            Val_int((int)(C_VALUE(namespace, x) op C_VALUE(namespace, y))));   \
    }

X(i8, shift_left, <<)
X(i8, shift_right, >>)
X(i16, shift_left, <<)
X(i16, shift_right, >>)

#undef X

#define X(namespace)                                                           \
    value checked_oint_##namespace##_scan_exn(value s, value base) {           \
        CAMLparam2(s, base);                                                   \
        assert(String_val(s));                                                 \
        assert(is_valid_base(Int_val(base)));                                  \
                                                                               \
        C_INT_TY(namespace) z = 0;                                             \
        if (-1 ==                                                              \
            impl_##namespace##_scan_exn(String_val(s), &z, Int_val(base))) {   \
            caml_failwith(__func__);                                           \
        }                                                                      \
                                                                               \
        CAMLreturn(OCAML_VALUE(namespace, z));                                 \
    }

X(int)
X(u32)
X(u64)
X(i32)
X(i64)

#undef X

#define X(namespace)                                                           \
    static value namespace##_wrap(const C_INT_TY(namespace) x) {               \
        CAMLparam0();                                                          \
        CAMLlocal1(result);                                                    \
                                                                               \
        result = caml_alloc_tuple(2);                                          \
        Store_field(result, 0, caml_copy_int64((uint64_t)(x >> 64)));          \
        Store_field(result, 1, caml_copy_int64((uint64_t)x));                  \
                                                                               \
        CAMLreturn(result);                                                    \
    }                                                                          \
                                                                               \
    static C_INT_TY(namespace) namespace##_unwrap(value x) {                   \
        const uint64_t high = C_VALUE(u64, Field(x, 0)),                       \
                       low = C_VALUE(u64, Field(x, 1));                        \
        return (C_INT_TY(namespace))high << 64 | (C_INT_TY(namespace))low;     \
    }                                                                          \
                                                                               \
    value checked_oint_##namespace##_equal(value x, value y) {                 \
        CAMLparam2(x, y);                                                      \
        CAMLreturn(Val_bool(namespace##_unwrap(x) == namespace##_unwrap(y)));  \
    }                                                                          \
                                                                               \
    value checked_oint_##namespace##_compare(value x, value y) {               \
        CAMLparam2(x, y);                                                      \
                                                                               \
        if (namespace##_unwrap(x) < namespace##_unwrap(y)) {                   \
            CAMLreturn(Val_int(-1));                                           \
        } else if (namespace##_unwrap(x) > namespace##_unwrap(y)) {            \
            CAMLreturn(Val_int(1));                                            \
        } else {                                                               \
            CAMLreturn(Val_int(0));                                            \
        }                                                                      \
    }                                                                          \
                                                                               \
    value checked_oint_##namespace##_min(value unit) {                         \
        CAMLparam1(unit);                                                      \
        CAMLreturn(namespace##_wrap(C_INT_MIN(namespace)));                    \
    }                                                                          \
                                                                               \
    value checked_oint_##namespace##_max(value unit) {                         \
        CAMLparam1(unit);                                                      \
        CAMLreturn(namespace##_wrap(C_INT_MAX(namespace)));                    \
    }                                                                          \
                                                                               \
    value checked_oint_##namespace##_print(value x) {                          \
        CAMLparam1(x);                                                         \
                                                                               \
        char buffer[MAX_INT_PRINT_SIZE] = {0};                                 \
        const int n = impl_##namespace##_print(namespace##_unwrap(x), buffer); \
                                                                               \
        CAMLreturn(caml_alloc_initialized_string(n, buffer));                  \
    }                                                                          \
                                                                               \
    value checked_oint_##namespace##_scan_exn(value s, value base) {           \
        CAMLparam2(s, base);                                                   \
        assert(String_val(s));                                                 \
        assert(is_valid_base(Int_val(base)));                                  \
                                                                               \
        C_INT_TY(namespace) z = 0;                                             \
        if (-1 ==                                                              \
            impl_##namespace##_scan_exn(String_val(s), &z, Int_val(base))) {   \
            caml_failwith(__func__);                                           \
        }                                                                      \
                                                                               \
        CAMLreturn(namespace##_wrap(z));                                       \
    }                                                                          \
                                                                               \
    X_OP2(namespace, add, +)                                                   \
    X_OP2(namespace, sub, -)                                                   \
    X_OP2(namespace, mul, *)                                                   \
    X_OP2(namespace, div, /)                                                   \
    X_OP2(namespace, rem, %)                                                   \
    X_OP1(namespace, bit_not, ~)                                               \
    X_OP2(namespace, bit_or, |)                                                \
    X_OP2(namespace, bit_and, &)                                               \
    X_OP2(namespace, bit_xor, ^)                                               \
    X_OP2(namespace, shift_left, <<)                                           \
    X_OP2(namespace, shift_right, >>)

#define X_OP1(namespace, name, op)                                             \
    value checked_oint_##namespace##_##name(value x) {                         \
        CAMLparam1(x);                                                         \
        CAMLreturn(namespace##_wrap(op namespace##_unwrap(x)));                \
    }
#define X_OP2(namespace, name, op)                                             \
    value checked_oint_##namespace##_##name(value x, value y) {                \
        CAMLparam2(x, y);                                                      \
        CAMLreturn(                                                            \
            namespace##_wrap(namespace##_unwrap(x) op namespace##_unwrap(y))); \
    }

X(u128)
X(i128)

#undef X
#undef X_OP1
#undef X_OP2

#define X(signedness, namespace)                                               \
    value checked_oint_##signedness##128##_of_##namespace(value x) {           \
        CAMLparam1(x);                                                         \
        CAMLreturn(signedness##128##_wrap(                                     \
            (C_INT_TY(signedness##128))C_VALUE(namespace, x)));                \
    }

X(u, u64)
X(i, i64)

#undef X

#define X(namespace)                                                           \
    value checked_oint_i128_to_##namespace(value x) {                          \
        CAMLparam1(x);                                                         \
        CAMLreturn(                                                            \
            OCAML_VALUE(namespace, (C_INT_TY(namespace))C_VALUE(i128, x)));    \
    }

X(int)
X(i32)
X(i64)

#undef X
