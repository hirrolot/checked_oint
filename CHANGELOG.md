# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased

### Added

 - The `checked_oint.guard` virtual library for controlling the behaviour of polymorphic comparison, with two implementations:
   - `checked_oint.guard-on`: polymorphic comparisons raise `Invalid_argument`, at the cost of pairing every integer with a guard value. The default.
   - `checked_oint.guard-off`: polymorphic comparisons silently succeed, but the representation is zero-cost.
 - The `S.to_int` and `S.to_int_exn` functions... finally.

## 2.0.1 - 2026-06-11

### Fixed

 - Zero-extend `u32` integers in `U64.of_value`, `I64.of_value`, `U128.of_value`, and `I128.of_value` instead of sign-extending them.
 - Truncate the results of `I8.shift_left` and `I16.shift_left` to 8 and 16 bits, respectively.
 - Reject negative integers in `U32.of_string` and `U64.of_string`.
 - Avoid undefined behavior when reassembling 128-bit integers in the C stubs.

## 2.0.0 - 2026-06-10

### Added

 - `S.ty` as the type representation of `S.t`.

### Changed

 - Rework `Int_ty.t` as a GADT indexed by the corresponding OCaml type; the new `Int_ty.equate` decides type equality.
 - Replace the `generic` type with `value`, which carries an `Int_ty.t` witness; rename the `S.of_generic` function family accordingly.
 - The `ops` function is now typed: `ops : 'a Int_ty.t -> (module S with type t = 'a)`.

### Removed

 - `singleton` and `pair`/`pair_exn`, which materialized a fresh first-class module on every call.
 - The `signedness`, `bitness`, and `int_ty` types, together with `S.int_ty`.

## 1.0.1 - 2025-11-30

### Changed

 - Only require `bisect_ppx` and `alcotest` for testing, not for production builds.

## 1.0.0 - 2025-05-15

### Changed

 - Relicense the project under the [Unlicense license](https://opensource.org/license/unlicense).

## 0.6.0 - 2025-04-12

### Changed

 - Put all the integer type shortcuts into the new `Int_ty` module.
 - The `generic_int_ty` function is now available as `Int_ty.(of_generic : generic -> t)`.

## 0.5.0 - 2025-04-07

### Added

 - `S.of_generic` and `S.of_generic_exn` to convert between any two integer types.

## 0.4.1 - 2025-01-03

### Changed

 - Use [manual interfacing with C] instead of [`ocaml-ctypes`].
   - This fixes compilation on Fedora Linux, openSUSE, FreeBSD, and Oracle Linux (issue https://github.com/hirrolot/checked_oint/issues/1).

[manual interfacing with C]: https://ocaml.org/manual/latest/intfc.html
[`ocaml-ctypes`]: https://github.com/yallop/ocaml-ctypes

### Fixed

 - Disable C assertions (`-DNDEBUG`) under the `release` profile.

## 0.4.0 - 2024-11-24

### Added

 - The `pair` function as an `option`-returning counterpart of `pair_exn`.

## 0.3.0 - 2024-11-09

### Added

 - Integer type shortcuts:
   - `u8_int_ty`, `u16_int_ty`, `u32_int_ty`, `u64_int_ty`, `u128_int_ty`
   - `i8_int_ty`, `i16_int_ty`, `i32_int_ty`, `i64_int_ty`, `i128_int_ty`

## 0.2.2 - 2024-11-01

### Fixed

 - Detect out-of-range letters during 128-bit integer parsing.

## 0.2.1 - 2024-10-16

### Fixed

 - Support binary, octal, and hexadecimal `of_string` formats for 128-bit integers.

## 0.2.0 - 2024-06-14

### Added

 - The `is_zero`, `is_one`, `is_all_ones` predicates on `generic` integers.

### Changed

 - Rename `S.ty` to `S.int_ty` (in accordance with `generic_int_ty`).

## 0.1.2 - 2024-05-15

### Added

 - `S.of_int` and `S.of_string` which return `option` instead of raising an exception.

### Fixed

 - Specify a required version of `alcotest`.

## 0.1.1 - 2024-05-05

### Added

 - `S.ty` for getting a type representation (`int_ty`).

### Fixed

 - Specify required versions of `ocaml` and `ctypes-foreign` in `dune-project`.

## 0.1.0 - 2024-05-01

### Added

 - This library.
