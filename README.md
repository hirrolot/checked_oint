# `checked_oint`

`checked_oint` is an OCaml library for checked integer arithmetic. We support the full set of signed and unsigned integers of bitnesses 8, 16, 32, 64, and 128.

## Installation

```
$ opam install checked_oint
```

## Usage

```ocaml
open Checked_oint

let () =
  let x = U8.of_int_exn 50 in
  let y = U8.of_int_exn 70 in
  assert (U8.equal (U8.add_exn x y) (U8.of_int_exn 120));
  assert (Option.is_none (U8.mul x y))
```

You can find the API documentation [here](https://hirrolot.github.io/checked_oint/checked_oint/Checked_oint/index.html).

## Implementation

 - `u8`, `u16`, `i8`, and `i16` are represented as `int` internally.
 - `u32` and `i32` (resp. `u64` and `i64`) are represented as `int32` (resp. `int64`) internally.
 - `u128` and `i128` are represented as `{ high : int64; low : int64 }` internally.
 - Operations on integers of 8, 16, 32, and 64 bits are implemented primarily in OCaml, save a small amount of C [stub functions].
 - Operations on 128-bit integers are implemented solely in C.
   - We heavily rely on the [`__int128` extension] by GCC and Clang.

[stub functions]: https://ocaml.org/manual/latest/intfc.html
[`__int128` extension]: https://gcc.gnu.org/onlinedocs/gcc/_005f_005fint128.html

## Release procedure

 1. Update the `version` field in `dune-project`.
 1. Type `dune build` to generate `checked_oint.opam`.
 1. Update `CHANGELOG.md`.
 1. Release the project in [GitHub Releases].
    1. Generate a source code archive and include it in the release:
       <br>`git archive HEAD -o checked_oint-<major>.<minor>.<patch>.tar.gz`
 1. Type `git pull && opam publish`.
    1. Specify the correct archive URL and checksums (by running `md5sum` and `sha512sum` on the archive).

[GitHub Releases]: https://github.com/hirrolot/checked_oint/releases

Including the output of `git archive` is needed because GitHub does not guarantee [stability of source code archives]. With a static asset, we will always get the same checksum, which is crucial for packaging.

[stability of source code archives]: https://github.blog/open-source/git/update-on-the-future-stability-of-source-code-archives-and-hashes/
