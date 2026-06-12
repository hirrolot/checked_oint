(** [checked_oint] is an OCaml library for checked integer arithmetic.

    This library supports signed and unsigned integers of bitnesses 8, 16, 32, 64, and
    128. {!S} is the main signature that contains common operations for all integer types;
    specific implementations are named {!module-U8}, {!module-U16}, and so on. *)

(** The exception raised on underflows/overflows. *)
exception Out_of_range

(** An unsigned 8-bit integer. *)
type u8 [@@deriving eq, show, ord]

(** An unsigned 16-bit integer. *)
type u16 [@@deriving eq, show, ord]

(** An unsigned 32-bit integer. *)
type u32 [@@deriving eq, show, ord]

(** An unsigned 64-bit integer. *)
type u64 [@@deriving eq, show, ord]

(** An unsigned 128-bit integer. *)
type u128 [@@deriving eq, show, ord]

(** A signed 8-bit integer. *)
type i8 [@@deriving eq, show, ord]

(** A signed 16-bit integer. *)
type i16 [@@deriving eq, show, ord]

(** A signed 32-bit integer. *)
type i32 [@@deriving eq, show, ord]

(** A signed 64-bit integer. *)
type i64 [@@deriving eq, show, ord]

(** A signed 128-bit integer. *)
type i128 [@@deriving eq, show, ord]

(** Represents a materialized integer type. *)
module Int_ty : sig
  type _ t =
    | U8 : u8 t
    | U16 : u16 t
    | U32 : u32 t
    | U64 : u64 t
    | U128 : u128 t
    | I8 : i8 t
    | I16 : i16 t
    | I32 : i32 t
    | I64 : i64 t
    | I128 : i128 t

  (** A witness of type equality. *)
  type (_, _) eq = Eq : ('a, 'a) eq

  (** Determines whether two integer types are the same. *)
  val equate : 'a t * 'b t -> ('a, 'b) eq option

  (** An integer type whose OCaml representation is unknown. *)
  type container = Container : _ t -> container

  (** The list of all integer types. *)
  val all : container list
end
[@@ocamlformat "module-item-spacing = compact"]

(** An integer of an arbitrary type. *)
type value = Value : 'a Int_ty.t * 'a -> value [@@deriving eq, show]

(** [true] if the integer is 0, [false] otherwise. *)
val is_zero : value -> bool

(** [true] if the integer is 1, [false] otherwise. *)
val is_one : value -> bool

(** [true] if the integer has all bits set to 1, [false] otherwise. *)
val is_all_ones : value -> bool

(** The signature of operations on integers. *)
module type S = sig
  (** The particular integer type.

      The bitwise operations {!bit_not}, {!bit_or}, {!bit_and}, {!bit_xor}, {!shift_left},
      and {!shift_right} assume that signed integers are represented in the two's
      complement notation.

      NOTE: polymorphic comparisons raise [Invalid_argument] by default; use {!equal} and
      {!compare} instead. This behaviour is controlled by the [checked_oint.guard]
      virtual library. *)
  type t [@@deriving eq, show, ord]

  (** The type representation. *)
  val ty : t Int_ty.t

  (** The number of bits of this integer type. *)
  val bits : int

  (** The value of 0. *)
  val zero : t

  (** The value of 1. *)
  val one : t

  (** The value that has all bits set to 1. *)
  val all_ones : t

  (** The minimum value of this type. *)
  val min_int : t

  (** The maximum value of this type. *)
  val max_int : t

  (** Whether this integer type is signed or not. *)
  val is_signed : bool

  (** Constructs a value out of [int]; returns [None] on overflow/underflow. *)
  val of_int : int -> t option

  (** Constructs a value out of [string]; returns [None] on overflow/underflow.

      We allow the following classes of non-negative integers:
      - Binary: [0b] ([0B]) followed by a non-empty sequence of binary digits [0] and [1].
      - Octal: [0o] ([0O]) followed by a non-empty sequence of octal digits [0], ..., [7].
      - Decimal: a non-empty sequence of decimal digits [0], ..., [9].
      - Hexadecimal: [0x] ([0X]) followed by a non-empty sequence of decimal digits [0],
        ..., [9] and letters [a], ..., [f] ([A], ..., [F]).

      A negative integer is described by the [-] character followed by a well-formed
      non-negative integer.

      All other combinations of characters will return [None]. *)
  val of_string : string -> t option

  (** Finds a successor; returns [None] on overflow. *)
  val succ : t -> t option

  (** Finds a predecessor; returns [None] on underflow. *)
  val pred : t -> t option

  (** Performs unary negation; returns [None] on overflow/underflow. *)
  val neg : t -> t option

  (** Finds an absolute value; returns [None] on overflow. *)
  val abs : t -> t option

  (** Performs addition; returns [None] on overflow/underflow. *)
  val add : t -> t -> t option

  (** Performs subtraction; returns [None] on overflow/underflow. *)
  val sub : t -> t -> t option

  (** Performs multiplication; returns [None] on overflow/underflow. *)
  val mul : t -> t -> t option

  (** Performs division; returns [None] on overflow/underflow. *)
  val div : t -> t -> t option

  (** Finds a remainder; returns [None] on overflow/underflow. *)
  val rem : t -> t -> t option

  (** Performs bitwise negation. *)
  val bit_not : t -> t

  (** Performs bitwise disjunction. *)
  val bit_or : t -> t -> t

  (** Performs bitwise conjunction. *)
  val bit_and : t -> t -> t

  (** Performs bitwise exclusive disjunction. *)
  val bit_xor : t -> t -> t

  (** Performs left shifting; returns [None] on overflow/underflow. *)
  val shift_left : t -> t -> t option

  (** Performs right shifting; returns [None] on overflow/underflow.

      This is a logical shift for unsigned integer types and arithmetic shift for signed
      integer types. *)
  val shift_right : t -> t -> t option

  (** Finds the minimum integer of two. *)
  val min : t -> t -> t

  (** Finds the maximum integer of two. *)
  val max : t -> t -> t

  (** Same as {!of_int} but raises {!Out_of_range} instead of returning [None]. *)
  val of_int_exn : int -> t

  (** Same as {!of_string} but raises {!Out_of_range} instead of returning [None]. *)
  val of_string_exn : string -> t

  (** Finds a successor; raises {!Out_of_range} on overflow. *)
  val succ_exn : t -> t

  (** Finds a predecessor; raises {!Out_of_range} on underflow. *)
  val pred_exn : t -> t

  (** Performs unary negation; raises {!Out_of_range} on overflow/underflow. *)
  val neg_exn : t -> t

  (** Finds an absolute value; raises {!Out_of_range} on overflow. *)
  val abs_exn : t -> t

  (** Performs addition; raises {!Out_of_range} on overflow/underflow. *)
  val add_exn : t -> t -> t

  (** Performs subtraction; raises {!Out_of_range} on overflow/underflow. *)
  val sub_exn : t -> t -> t

  (** Performs multiplication; raises {!Out_of_range} on overflow/underflow. *)
  val mul_exn : t -> t -> t

  (** Performs division; raises {!Out_of_range} on overflow/underflow. *)
  val div_exn : t -> t -> t

  (** Finds a remainder; raises {!Out_of_range} on overflow/underflow. *)
  val rem_exn : t -> t -> t

  (** Performs left shifting; raises {!Out_of_range} on overflow/underflow. *)
  val shift_left_exn : t -> t -> t

  (** Performs right shifting; raises {!Out_of_range} on overflow/underflow.

      This is a logical shift for unsigned integer types and arithmetic shift for signed
      integer types. *)
  val shift_right_exn : t -> t -> t

  (** Prints a value in decimal; same as {!show}. *)
  val to_string : t -> string

  (** Constructs an integer out of {!value}; returns [None] on overflow/underflow. *)
  val of_value : value -> t option

  (** Same as {!of_value} but raises {!Out_of_range} instead of returning [None]. *)
  val of_value_exn : value -> t

  (** Casts an integer into {!value}. *)
  val to_value : t -> value
end

(** The implementation of {!u8}. *)
module U8 : S with type t = u8

(** The implementation of {!u16}. *)
module U16 : S with type t = u16

(** The implementation of {!u32}. *)
module U32 : S with type t = u32

(** The implementation of {!u64}. *)
module U64 : S with type t = u64

(** The implementation of {!u128}. *)
module U128 : sig
  include S with type t = u128

  (** Projects {!u128} into the high and low parts, respectively.

      This function is useful for generating 128-bit literals from within C code. Suppose
      that [U128.split x] is [(high, low)]; in order to obtain the original [x], one can
      write the following C expression:

      - [((unsigned __int128)high << 64 | (unsigned __int128)low)] *)
  val split : t -> u64 * u64
end

(** The implementation of {!i8}. *)
module I8 : S with type t = i8

(** The implementation of {!i16}. *)
module I16 : S with type t = i16

(** The implementation of {!i32}. *)
module I32 : S with type t = i32

(** The implementation of {!i64}. *)
module I64 : S with type t = i64

(** The implementation of {!i128}. *)
module I128 : sig
  include S with type t = i128

  (** Same as {!U128.split} but for {!i128}. *)
  val split : t -> u64 * u64
end

(** Finds an integer implementation based on its type representation. *)
val ops : 'a Int_ty.t -> (module S with type t = 'a)
