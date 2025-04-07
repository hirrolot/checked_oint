# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## unreleased

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
