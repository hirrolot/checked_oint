#ifndef CHECKED_OINT_UTIL_H
#define CHECKED_OINT_UTIL_H

#include <inttypes.h>
#include <limits.h>
#include <stdint.h>

#include <caml/mlvalues.h>

#define C_VALUE(namespace, x) C_VALUE_##namespace(x)
#define C_VALUE_int(x)        Int_val(x)
#define C_VALUE_u8(x)         ((uint8_t)Int_val(x))
#define C_VALUE_u16(x)        ((uint16_t)Int_val(x))
#define C_VALUE_u32(x)        ((uint32_t)Int32_val(x))
#define C_VALUE_u64(x)        ((uint64_t)Int64_val(x))
#define C_VALUE_u128(x)       u128_unwrap(x)
#define C_VALUE_i8(x)         ((int8_t)Int_val(x))
#define C_VALUE_i16(x)        ((int16_t)Int_val(x))
#define C_VALUE_i32(x)        Int32_val(x)
#define C_VALUE_i64(x)        Int64_val(x)
#define C_VALUE_i128(x)       i128_unwrap(x)

#define OCAML_VALUE(namespace, x) OCAML_VALUE_##namespace(x)
#define OCAML_VALUE_int(x)        Val_int(x)
#define OCAML_VALUE_u8(x)         Val_int((int)(x))
#define OCAML_VALUE_u16(x)        Val_int((int)(x))
#define OCAML_VALUE_u32(x)        caml_copy_int32((int32_t)(x))
#define OCAML_VALUE_u64(x)        caml_copy_int64((int64_t)(x))
#define OCAML_VALUE_u128(x)       u128_wrap(x)
#define OCAML_VALUE_i8(x)         Val_int((int)(x))
#define OCAML_VALUE_i16(x)        Val_int((int)(x))
#define OCAML_VALUE_i32(x)        caml_copy_int32(x)
#define OCAML_VALUE_i64(x)        caml_copy_int64(x)
#define OCAML_VALUE_i128(x)       i128_wrap(x)

#define C_INT_TY(namespace) C_INT_TY_##namespace
#define C_INT_TY_int        int
#define C_INT_TY_u8         uint8_t
#define C_INT_TY_u16        uint16_t
#define C_INT_TY_u32        uint32_t
#define C_INT_TY_u64        uint64_t
#define C_INT_TY_u128       unsigned __int128
#define C_INT_TY_i8         int8_t
#define C_INT_TY_i16        int16_t
#define C_INT_TY_i32        int32_t
#define C_INT_TY_i64        int64_t
#define C_INT_TY_i128       __int128

#define C_INT_MIN(namespace) C_INT_MIN_##namespace
#define C_INT_MIN_int        INT_MIN
#define C_INT_MIN_u8         UINT8_C(0)
#define C_INT_MIN_u16        UINT16_C(0)
#define C_INT_MIN_u32        UINT32_C(0)
#define C_INT_MIN_u64        UINT64_C(0)
#define C_INT_MIN_u128       ((unsigned __int128)0)
#define C_INT_MIN_i8         INT8_MIN
#define C_INT_MIN_i16        INT16_MIN
#define C_INT_MIN_i32        INT32_MIN
#define C_INT_MIN_i64        INT64_MIN
#define C_INT_MIN_i128       (~C_INT_MAX_i128)

#define C_INT_MAX(namespace) C_INT_MAX_##namespace
#define C_INT_MAX_int        INT_MAX
#define C_INT_MAX_u8         UINT8_MAX
#define C_INT_MAX_u16        UINT16_MAX
#define C_INT_MAX_u32        UINT32_MAX
#define C_INT_MAX_u64        UINT64_MAX
#define C_INT_MAX_u128       (~C_INT_MIN_u128)
#define C_INT_MAX_i8         INT8_MAX
#define C_INT_MAX_i16        INT16_MAX
#define C_INT_MAX_i32        INT32_MAX
#define C_INT_MAX_i64        INT64_MAX
#define C_INT_MAX_i128       ((__int128)(C_INT_MAX_u128 >> 1))

#define C_INT_BIT_WIDTH(namespace) (sizeof(C_INT_TY(namespace)) * CHAR_BIT)

#endif // CHECKED_OINT_UTIL_H
