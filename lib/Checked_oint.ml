module C = struct
  type u128 =
    { u128_high : int64
    ; u128_low : int64
    }

  type i128 =
    { i128_high : int64
    ; i128_low : int64
    }

  external i8_bit_not : int -> int = "checked_oint_i8_bit_not" [@@noalloc]
  external i16_bit_not : int -> int = "checked_oint_i16_bit_not" [@@noalloc]
  external i8_shift_left : int -> int -> int = "checked_oint_i8_shift_left" [@@noalloc]
  external i8_shift_right : int -> int -> int = "checked_oint_i8_shift_right" [@@noalloc]
  external i16_shift_left : int -> int -> int = "checked_oint_i16_shift_left" [@@noalloc]

  external i16_shift_right : int -> int -> int = "checked_oint_i16_shift_right"
  [@@noalloc]

  external int_scan_exn : string -> int -> int = "checked_oint_int_scan_exn"
  external u32_scan_exn : string -> int -> int32 = "checked_oint_u32_scan_exn"
  external u64_scan_exn : string -> int -> int64 = "checked_oint_u64_scan_exn"
  external i32_scan_exn : string -> int -> int32 = "checked_oint_i32_scan_exn"
  external i64_scan_exn : string -> int -> int64 = "checked_oint_i64_scan_exn"
  external u128_equal : u128 -> u128 -> bool = "checked_oint_u128_equal" [@@noalloc]
  external u128_compare : u128 -> u128 -> int = "checked_oint_u128_compare" [@@noalloc]
  external u128_min : unit -> u128 = "checked_oint_u128_min"
  external u128_max : unit -> u128 = "checked_oint_u128_max"
  external u128_print : u128 -> string = "checked_oint_u128_print"
  external u128_scan_exn : string -> int -> u128 = "checked_oint_u128_scan_exn"
  external u128_add : u128 -> u128 -> u128 = "checked_oint_u128_add"
  external u128_sub : u128 -> u128 -> u128 = "checked_oint_u128_sub"
  external u128_mul : u128 -> u128 -> u128 = "checked_oint_u128_mul"
  external u128_div : u128 -> u128 -> u128 = "checked_oint_u128_div"
  external u128_rem : u128 -> u128 -> u128 = "checked_oint_u128_rem"
  external u128_bit_not : u128 -> u128 = "checked_oint_u128_bit_not"
  external u128_bit_or : u128 -> u128 -> u128 = "checked_oint_u128_bit_or"
  external u128_bit_and : u128 -> u128 -> u128 = "checked_oint_u128_bit_and"
  external u128_bit_xor : u128 -> u128 -> u128 = "checked_oint_u128_bit_xor"
  external u128_shift_left : u128 -> u128 -> u128 = "checked_oint_u128_shift_left"
  external u128_shift_right : u128 -> u128 -> u128 = "checked_oint_u128_shift_right"
  external i128_equal : i128 -> i128 -> bool = "checked_oint_i128_equal" [@@noalloc]
  external i128_compare : i128 -> i128 -> int = "checked_oint_i128_compare" [@@noalloc]
  external i128_min : unit -> i128 = "checked_oint_i128_min"
  external i128_max : unit -> i128 = "checked_oint_i128_max"
  external i128_print : i128 -> string = "checked_oint_i128_print"
  external i128_scan_exn : string -> int -> i128 = "checked_oint_i128_scan_exn"
  external i128_add : i128 -> i128 -> i128 = "checked_oint_i128_add"
  external i128_sub : i128 -> i128 -> i128 = "checked_oint_i128_sub"
  external i128_mul : i128 -> i128 -> i128 = "checked_oint_i128_mul"
  external i128_div : i128 -> i128 -> i128 = "checked_oint_i128_div"
  external i128_rem : i128 -> i128 -> i128 = "checked_oint_i128_rem"
  external i128_bit_not : i128 -> i128 = "checked_oint_i128_bit_not"
  external i128_bit_or : i128 -> i128 -> i128 = "checked_oint_i128_bit_or"
  external i128_bit_and : i128 -> i128 -> i128 = "checked_oint_i128_bit_and"
  external i128_bit_xor : i128 -> i128 -> i128 = "checked_oint_i128_bit_xor"
  external i128_shift_left : i128 -> i128 -> i128 = "checked_oint_i128_shift_left"
  external i128_shift_right : i128 -> i128 -> i128 = "checked_oint_i128_shift_right"
end
[@@ocamlformat "module-item-spacing = compact"]

(* Unlike [Int64.of_int32], this function zero-extends its argument. *)
let u64_of_u32 x = Int64.(logand (of_int32 x) 0xFFFFFFFFL)

(* Conversions to/from unsigned 128-bit integers. *)
include struct
  let u128_of_i128_unchecked C.{ i128_high; i128_low } =
      C.{ u128_high = i128_high; u128_low = i128_low }
  ;;

  let u128_of_u64 x = C.{ u128_high = 0L; u128_low = x }

  let u128_of_u32 x = u128_of_u64 (u64_of_u32 x)
end

(* Conversions to/from signed 128-bit integers. *)
include struct
  let i128_of_u128_unchecked C.{ u128_high; u128_low } =
      C.{ i128_high = u128_high; i128_low = u128_low }
  ;;

  let i128_of_u64 x = C.{ i128_high = 0L; i128_low = x }

  let i128_of_u32 x = i128_of_u64 (u64_of_u32 x)

  let i128_of_i64 x =
      if Int64.(compare x zero >= 0)
      then i128_of_u64 x
      else C.{ i128_high = -1L; i128_low = x }
  ;;

  let i128_of_i32 x = i128_of_i64 (Int64.of_int32 x)

  let i128_to_i64_unchecked C.{ i128_high; i128_low } =
      if Int64.equal i128_high 0L
      then i128_low
      else if Int64.equal i128_high (-1L)
      then i128_low
      else failwith "Impossible"
  ;;

  let i128_to_i32_unchecked x = Int64.to_int32 (i128_to_i64_unchecked x)
end

exception Out_of_range

(* Polymorphic comparison operators will raise an exception on [unit -> unit]. *)
type 'a wrapper = (unit -> unit) * 'a

let wrap x = (fun () -> ()), x

let unwrap (_f, x) = x

let wrap_op1 f x = wrap (f (unwrap x))

let wrap_op2 f x y = wrap (f (unwrap x) (unwrap y))

let wrap_to_string f x = f (unwrap x)

let equal_int_wrapper x y = Int.equal (unwrap x) (unwrap y)

let pp_int_wrapper fmt x = Format.pp_print_string fmt (string_of_int (unwrap x))

let compare_int_wrapper x y = Int.compare (unwrap x) (unwrap y)

type int_wrapper = int wrapper

type u8 = int_wrapper [@@deriving eq, show, ord]

type u16 = int_wrapper [@@deriving eq, show, ord]

let u32_to_string x = Printf.sprintf "%lu" (unwrap x)

let equal_u32_wrapper x y = Int32.equal (unwrap x) (unwrap y)

let pp_u32_wrapper fmt x = Format.pp_print_string fmt (u32_to_string x)

let compare_u32_wrapper x y = Int32.unsigned_compare (unwrap x) (unwrap y)

type u32_wrapper = Int32.t wrapper

type u32 = u32_wrapper [@@deriving eq, show, ord]

let u64_to_string x = Printf.sprintf "%Lu" (unwrap x)

let equal_u64_wrapper x y = Int64.equal (unwrap x) (unwrap y)

let pp_u64_wrapper fmt x = Format.pp_print_string fmt (u64_to_string x)

let compare_u64_wrapper x y = Int64.unsigned_compare (unwrap x) (unwrap y)

type u64_wrapper = Int64.t wrapper

type u64 = u64_wrapper [@@deriving eq, show, ord]

let u128_to_string = wrap_to_string C.u128_print

let equal_u128_wrapper x y = C.u128_equal (unwrap x) (unwrap y)

let pp_u128_wrapper fmt x = Format.pp_print_string fmt (u128_to_string x)

let compare_u128_wrapper x y = C.u128_compare (unwrap x) (unwrap y)

type u128_wrapper = C.u128 wrapper

type u128 = u128_wrapper [@@deriving eq, show, ord]

type i8 = int_wrapper [@@deriving eq, show, ord]

type i16 = int_wrapper [@@deriving eq, show, ord]

let equal_i32_wrapper x y = Int32.equal (unwrap x) (unwrap y)

let pp_i32_wrapper fmt x = Format.pp_print_string fmt (Int32.to_string (unwrap x))

let compare_i32_wrapper x y = Int32.compare (unwrap x) (unwrap y)

type i32_wrapper = Int32.t wrapper

type i32 = i32_wrapper [@@deriving eq, show, ord]

let equal_i64_wrapper x y = Int64.equal (unwrap x) (unwrap y)

let pp_i64_wrapper fmt x = Format.pp_print_string fmt (Int64.to_string (unwrap x))

let compare_i64_wrapper x y = Int64.compare (unwrap x) (unwrap y)

type i64_wrapper = Int64.t wrapper

type i64 = i64_wrapper [@@deriving eq, show, ord]

let i128_to_string = wrap_to_string C.i128_print

let equal_i128_wrapper x y = C.i128_equal (unwrap x) (unwrap y)

let pp_i128_wrapper fmt x = Format.pp_print_string fmt (i128_to_string x)

let compare_i128_wrapper x y = C.i128_compare (unwrap x) (unwrap y)

type i128_wrapper = C.i128 wrapper

type i128 = i128_wrapper [@@deriving eq, show, ord]

module Int_ty = struct
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

  type (_, _) eq = Eq : ('a, 'a) eq

  let equate : type a b. a t * b t -> (a, b) eq option = function
    | U8, U8 -> Some Eq
    | U16, U16 -> Some Eq
    | U32, U32 -> Some Eq
    | U64, U64 -> Some Eq
    | U128, U128 -> Some Eq
    | I8, I8 -> Some Eq
    | I16, I16 -> Some Eq
    | I32, I32 -> Some Eq
    | I64, I64 -> Some Eq
    | I128, I128 -> Some Eq
    | _, _ -> None
  ;;

  type container = Container : _ t -> container

  let all =
      [ Container U8; Container U16; Container U32; Container U64; Container U128 ]
      @ [ Container I8; Container I16; Container I32; Container I64; Container I128 ]
  ;;
end
[@@ocamlformat "module-item-spacing = compact"]

type value = Value : 'a Int_ty.t * 'a -> value

module type Basic = sig
  type t [@@deriving eq, show, ord]

  val ty : t Int_ty.t
  val bits : int
  val min_int : t
  val max_int : t
  val of_int_unchecked : int -> t
  val add_unchecked : t -> t -> t
  val sub_unchecked : t -> t -> t
  val mul_unchecked : t -> t -> t
  val div_unchecked : t -> t -> t
  val rem_unchecked : t -> t -> t
  val shift_left_unchecked : t -> t -> t
  val shift_right_unchecked : t -> t -> t
  val bit_not : t -> t
  val bit_or : t -> t -> t
  val bit_and : t -> t -> t
  val bit_xor : t -> t -> t
  val of_int : int -> t option
  val of_string : string -> t option
  val of_value : value -> t option
end
[@@ocamlformat "module-item-spacing = compact"]

let determine_base s =
    let cut_prefix prefix = String.(sub s (length prefix) (length s - length prefix)) in
    let ( let$ ) (prefix, base) k =
        if String.starts_with ~prefix s
        then base, cut_prefix prefix
        else if String.starts_with ~prefix:("-" ^ prefix) s
        then base, "-" ^ cut_prefix ("-" ^ prefix)
        else k ()
    in
    let$ () = "0b", 2 in
    let$ () = "0B", 2 in
    let$ () = "0o", 8 in
    let$ () = "0O", 8 in
    let$ () = "0x", 16 in
    let$ () = "0X", 16 in
    10, s
;;

(* This module is designed only for 8- and 16-bit integers, whether signed or unsigned. *)
module Inherit_int_basic (S : sig
    val min_int : int_wrapper
    val max_int : int_wrapper
  end) =
struct
  let of_int_unchecked x = wrap x
  let add_unchecked = wrap_op2 ( + )
  let sub_unchecked = wrap_op2 ( - )
  let mul_unchecked = wrap_op2 ( * )
  let div_unchecked = wrap_op2 ( / )
  let rem_unchecked = wrap_op2 Int.rem
  let bit_or = wrap_op2 ( lor )
  let bit_and = wrap_op2 ( land )
  let bit_xor = wrap_op2 ( lxor )

  let of_int x =
      if Int.compare x (unwrap S.min_int) < 0 || Int.compare x (unwrap S.max_int) > 0
      then None
      else Some (of_int_unchecked x)
  ;;

  let of_string s =
      let base, s = determine_base s in
      match C.int_scan_exn s base with
      | exception Failure _s -> None
      | n
        when Int.compare n (unwrap S.min_int) < 0 || Int.compare n (unwrap S.max_int) > 0
        -> None
      | n -> Some (wrap n)
  ;;

  let of_value : value -> int_wrapper option =
      let min_value, max_value = unwrap S.min_int, unwrap S.max_int in
      let of_small_int x =
          if Int.compare x min_value >= 0 && Int.compare x max_value <= 0
          then Some (wrap x)
          else None
      in
      function
      | Value (Int_ty.U8, (_f, x)) -> of_small_int x
      | Value (Int_ty.U16, (_f, x)) -> of_small_int x
      | Value (Int_ty.I8, (_f, x)) -> of_small_int x
      | Value (Int_ty.I16, (_f, x)) -> of_small_int x
      | Value (Int_ty.U32, (_f, x)) ->
        let open Int32 in
        if unsigned_compare x (of_int max_value) <= 0
        then Some (wrap (to_int x))
        else None
      | Value (Int_ty.U64, (_f, x)) ->
        let open Int64 in
        if unsigned_compare x (of_int max_value) <= 0
        then Some (wrap (to_int x))
        else None
      | Value (Int_ty.I32, (_f, x)) ->
        let open Int32 in
        if compare x (of_int min_value) >= 0 && compare x (of_int max_value) <= 0
        then Some (wrap (to_int x))
        else None
      | Value (Int_ty.I64, (_f, x)) ->
        let open Int64 in
        if compare x (of_int min_value) >= 0 && compare x (of_int max_value) <= 0
        then Some (wrap (to_int x))
        else None
      | Value (Int_ty.U128, (_f, x)) ->
        if C.u128_compare x (u128_of_u64 (Int64.of_int max_value)) <= 0
        then Some (wrap (Int64.to_int x.u128_low))
        else None
      | Value (Int_ty.I128, (_f, x)) ->
        if
          C.i128_compare x (i128_of_i64 (Int64.of_int min_value)) >= 0
          && C.i128_compare x (i128_of_i64 (Int64.of_int max_value)) <= 0
        then Some (wrap (Int64.to_int (i128_to_i64_unchecked x)))
        else None
  ;;
end
[@@ocamlformat "module-item-spacing = compact"]

module U8_basic : Basic with type t = u8 = struct
  type t = u8 [@@deriving eq, show, ord]

  let ty = Int_ty.U8
  let bits = 8
  let min_int = wrap 0
  let max_int = wrap 255
  let shift_left_unchecked = wrap_op2 (fun x y -> (x lsl y) land unwrap max_int)
  let shift_right_unchecked = wrap_op2 (fun x y -> x lsr y)
  let bit_not = wrap_op1 (fun x -> lnot x land unwrap max_int)

  include Inherit_int_basic (struct
      let min_int = min_int
      let max_int = max_int
    end)
end
[@@ocamlformat "module-item-spacing = compact"]

module U16_basic : Basic with type t = u16 = struct
  type t = u16 [@@deriving eq, show, ord]

  let ty = Int_ty.U16
  let bits = 16
  let min_int = wrap 0
  let max_int = wrap 65535
  let shift_left_unchecked = wrap_op2 (fun x y -> (x lsl y) land unwrap max_int)
  let shift_right_unchecked = wrap_op2 (fun x y -> x lsr y)
  let bit_not = wrap_op1 (fun x -> lnot x land unwrap max_int)

  include Inherit_int_basic (struct
      let min_int = min_int
      let max_int = max_int
    end)
end
[@@ocamlformat "module-item-spacing = compact"]

module U32_basic : Basic with type t = u32 = struct
  type t = u32 [@@deriving eq, show, ord]

  let ty = Int_ty.U32
  let bits = 32
  let min_int = wrap Int32.zero
  let max_int = wrap Int32.minus_one
  let of_int_unchecked x = wrap (Int32.of_int x)
  let add_unchecked = wrap_op2 Int32.add
  let sub_unchecked = wrap_op2 Int32.sub
  let mul_unchecked = wrap_op2 Int32.mul
  let div_unchecked = wrap_op2 Int32.unsigned_div
  let rem_unchecked = wrap_op2 Int32.unsigned_rem
  let shift_left_unchecked = wrap_op2 (fun x y -> Int32.(shift_left x (to_int y)))

  let shift_right_unchecked =
      wrap_op2 (fun x y -> Int32.(shift_right_logical x (to_int y)))
  ;;

  let bit_not = wrap_op1 Int32.lognot
  let bit_or = wrap_op2 Int32.logor
  let bit_and = wrap_op2 Int32.logand
  let bit_xor = wrap_op2 Int32.logxor

  let of_int x =
      if Int.compare x 0 < 0 || Int64.(compare (of_int x) (pred (shift_left 1L 32)) > 0)
      then None
      else Some (of_int_unchecked x)
  ;;

  let of_string s =
      let base, s = determine_base s in
      try Some (wrap (C.u32_scan_exn s base)) with
      | Failure _s -> None
  ;;

  let of_value =
      let max_u32_as_i64 = 0xFFFFFFFFL in
      let of_small_int x = Some (wrap (Int32.of_int x)) in
      let of_small_signed_int x =
          if Int.compare x 0 >= 0 then Some (wrap (Int32.of_int x)) else None
      in
      function
      | Value (Int_ty.U8, (_f, x)) -> of_small_int x
      | Value (Int_ty.U16, (_f, x)) -> of_small_int x
      | Value (Int_ty.U32, x) -> Some x
      | Value (Int_ty.U64, (_f, x)) ->
        if Int64.unsigned_compare x max_u32_as_i64 <= 0
        then Some (wrap (Int64.to_int32 x))
        else None
      | Value (Int_ty.U128, (_f, x)) ->
        if C.u128_compare x (u128_of_u64 max_u32_as_i64) <= 0
        then Some (wrap (Int64.to_int32 x.u128_low))
        else None
      | Value (Int_ty.I8, (_f, x)) -> of_small_signed_int x
      | Value (Int_ty.I16, (_f, x)) -> of_small_signed_int x
      | Value (Int_ty.I32, (_f, x)) ->
        if Int32.(compare x zero >= 0) then Some (wrap x) else None
      | Value (Int_ty.I64, (_f, x)) ->
        if Int64.(compare x zero >= 0 && compare x max_u32_as_i64 <= 0)
        then Some (wrap (Int64.to_int32 x))
        else None
      | Value (Int_ty.I128, (_f, x)) ->
        if
          C.i128_compare x (i128_of_i64 0L) >= 0
          && C.i128_compare x (i128_of_i64 max_u32_as_i64) <= 0
        then Some (wrap (Int64.to_int32 x.i128_low))
        else None
  ;;
end
[@@ocamlformat "module-item-spacing = compact"]

module U64_basic : Basic with type t = u64 = struct
  type t = u64 [@@deriving eq, show, ord]

  let ty = Int_ty.U64
  let bits = 64
  let min_int = wrap Int64.zero
  let max_int = wrap Int64.minus_one
  let of_int_unchecked x = wrap (Int64.of_int x)
  let add_unchecked = wrap_op2 Int64.add
  let sub_unchecked = wrap_op2 Int64.sub
  let mul_unchecked = wrap_op2 Int64.mul
  let div_unchecked = wrap_op2 Int64.unsigned_div
  let rem_unchecked = wrap_op2 Int64.unsigned_rem
  let shift_left_unchecked = wrap_op2 (fun x y -> Int64.(shift_left x (to_int y)))

  let shift_right_unchecked =
      wrap_op2 (fun x y -> Int64.(shift_right_logical x (to_int y)))
  ;;

  let bit_not = wrap_op1 Int64.lognot
  let bit_or = wrap_op2 Int64.logor
  let bit_and = wrap_op2 Int64.logand
  let bit_xor = wrap_op2 Int64.logxor
  let of_int x = if Int.compare x 0 < 0 then None else Some (of_int_unchecked x)

  let of_string s =
      let base, s = determine_base s in
      try Some (wrap (C.u64_scan_exn s base)) with
      | Failure _s -> None
  ;;

  let of_value =
      let max_u64_as_i64 = Int64.minus_one in
      let of_small_int x = Some (wrap (Int64.of_int x)) in
      let of_small_signed_int x =
          if Int.compare x 0 >= 0 then Some (wrap (Int64.of_int x)) else None
      in
      function
      | Value (Int_ty.U8, (_f, x)) -> of_small_int x
      | Value (Int_ty.U16, (_f, x)) -> of_small_int x
      | Value (Int_ty.U32, (_f, x)) -> Some (wrap (u64_of_u32 x))
      | Value (Int_ty.U64, x) -> Some x
      | Value (Int_ty.U128, (_f, x)) ->
        if C.u128_compare x (u128_of_u64 max_u64_as_i64) <= 0
        then Some (wrap x.u128_low)
        else None
      | Value (Int_ty.I8, (_f, x)) -> of_small_signed_int x
      | Value (Int_ty.I16, (_f, x)) -> of_small_signed_int x
      | Value (Int_ty.I32, (_f, x)) ->
        if Int32.(compare x zero >= 0) then Some (wrap (Int64.of_int32 x)) else None
      | Value (Int_ty.I64, (_f, x)) ->
        if Int64.(compare x zero >= 0) then Some (wrap x) else None
      | Value (Int_ty.I128, (_f, x)) ->
        if
          C.i128_compare x (i128_of_i64 0L) >= 0
          && C.i128_compare x (i128_of_u64 max_u64_as_i64) <= 0
        then Some (wrap x.i128_low)
        else None
  ;;
end
[@@ocamlformat "module-item-spacing = compact"]

module U128_basic : Basic with type t = u128 = struct
  type t = u128 [@@deriving eq, show, ord]

  let ty = Int_ty.U128
  let bits = 128
  let min_int = wrap (C.u128_min ())
  let max_int = wrap (C.u128_max ())
  let of_int_unchecked x = wrap (u128_of_u64 (Int64.of_int x))
  let add_unchecked = wrap_op2 C.u128_add
  let sub_unchecked = wrap_op2 C.u128_sub
  let mul_unchecked = wrap_op2 C.u128_mul
  let div_unchecked = wrap_op2 C.u128_div
  let rem_unchecked = wrap_op2 C.u128_rem
  let shift_left_unchecked = wrap_op2 C.u128_shift_left
  let shift_right_unchecked = wrap_op2 C.u128_shift_right
  let bit_not = wrap_op1 C.u128_bit_not
  let bit_or = wrap_op2 C.u128_bit_or
  let bit_and = wrap_op2 C.u128_bit_and
  let bit_xor = wrap_op2 C.u128_bit_xor
  let of_int x = if Int.compare x 0 < 0 then None else Some (of_int_unchecked x)

  let of_string s =
      let base, s = determine_base s in
      try Some (wrap (C.u128_scan_exn s base)) with
      | Failure _s -> None
  ;;

  let of_value =
      let of_small_int x = Some (wrap (u128_of_u64 (Int64.of_int x))) in
      let of_small_signed_int x =
          if Int.compare x 0 >= 0
          then Some (wrap (u128_of_u64 (Int64.of_int x)))
          else None
      in
      function
      | Value (Int_ty.U8, (_f, x)) -> of_small_int x
      | Value (Int_ty.U16, (_f, x)) -> of_small_int x
      | Value (Int_ty.U32, (_f, x)) -> Some (wrap (u128_of_u32 x))
      | Value (Int_ty.U64, (_f, x)) -> Some (wrap (u128_of_u64 x))
      | Value (Int_ty.U128, x) -> Some x
      | Value (Int_ty.I8, (_f, x)) -> of_small_signed_int x
      | Value (Int_ty.I16, (_f, x)) -> of_small_signed_int x
      | Value (Int_ty.I32, (_f, x)) ->
        if Int32.(compare x zero >= 0) then Some (wrap (u128_of_u32 x)) else None
      | Value (Int_ty.I64, (_f, x)) ->
        if Int64.(compare x zero >= 0) then Some (wrap (u128_of_u64 x)) else None
      | Value (Int_ty.I128, (_f, x)) ->
        if C.(i128_compare x (i128_of_i64 0L) >= 0)
        then Some (wrap (u128_of_i128_unchecked x))
        else None
  ;;
end
[@@ocamlformat "module-item-spacing = compact"]

module I8_basic : Basic with type t = i8 = struct
  type t = i8 [@@deriving eq, show, ord]

  let ty = Int_ty.I8
  let bits = 8
  let min_int = wrap (-128)
  let max_int = wrap 127
  let shift_left_unchecked = wrap_op2 C.i8_shift_left
  let shift_right_unchecked = wrap_op2 C.i8_shift_right
  let bit_not = wrap_op1 C.i8_bit_not

  include Inherit_int_basic (struct
      let min_int = min_int
      let max_int = max_int
    end)
end
[@@ocamlformat "module-item-spacing = compact"]

module I16_basic : Basic with type t = i16 = struct
  type t = i16 [@@deriving eq, show, ord]

  let ty = Int_ty.I16
  let bits = 16
  let min_int = wrap (-32768)
  let max_int = wrap 32767
  let shift_left_unchecked = wrap_op2 C.i16_shift_left
  let shift_right_unchecked = wrap_op2 C.i16_shift_right
  let bit_not = wrap_op1 C.i16_bit_not

  include Inherit_int_basic (struct
      let min_int = min_int
      let max_int = max_int
    end)
end
[@@ocamlformat "module-item-spacing = compact"]

module I32_basic : Basic with type t = i32 = struct
  type t = i32 [@@deriving eq, show, ord]

  let ty = Int_ty.I32
  let bits = 32
  let min_int = wrap Int32.min_int
  let max_int = wrap Int32.max_int
  let of_int_unchecked x = wrap (Int32.of_int x)
  let add_unchecked = wrap_op2 Int32.add
  let sub_unchecked = wrap_op2 Int32.sub
  let mul_unchecked = wrap_op2 Int32.mul
  let div_unchecked = wrap_op2 Int32.div
  let rem_unchecked = wrap_op2 Int32.rem
  let shift_left_unchecked = wrap_op2 (fun x y -> Int32.(shift_left x (to_int y)))
  let shift_right_unchecked = wrap_op2 (fun x y -> Int32.(shift_right x (to_int y)))
  let bit_not = wrap_op1 Int32.lognot
  let bit_or = wrap_op2 Int32.logor
  let bit_and = wrap_op2 Int32.logand
  let bit_xor = wrap_op2 Int32.logxor

  let of_int x =
      let min_int_as_int64, max_int_as_int64 =
          Int64.of_int32 (unwrap min_int), Int64.of_int32 (unwrap max_int)
      in
      if
        Int64.(
          compare (of_int x) min_int_as_int64 < 0
          || compare (of_int x) max_int_as_int64 > 0)
      then None
      else Some (of_int_unchecked x)
  ;;

  let of_string s =
      let base, s = determine_base s in
      try Some (wrap (C.i32_scan_exn s base)) with
      | Failure _s -> None
  ;;

  let of_value =
      let min_i32_as_i64, max_i32_as_i64 =
          Int64.(of_int32 Int32.min_int, of_int32 Int32.max_int)
      in
      let of_small_int x = Some (wrap (Int32.of_int x)) in
      function
      | Value (Int_ty.U8, (_f, x)) -> of_small_int x
      | Value (Int_ty.U16, (_f, x)) -> of_small_int x
      | Value (Int_ty.I8, (_f, x)) -> of_small_int x
      | Value (Int_ty.I16, (_f, x)) -> of_small_int x
      | Value (Int_ty.U32, (_f, x)) ->
        if Int32.(unsigned_compare x max_int <= 0) then Some (wrap x) else None
      | Value (Int_ty.U64, (_f, x)) ->
        if Int64.unsigned_compare x max_i32_as_i64 <= 0
        then Some (wrap (Int64.to_int32 x))
        else None
      | Value (Int_ty.U128, (_f, x)) ->
        if C.u128_compare x (u128_of_u64 max_i32_as_i64) <= 0
        then Some (wrap (Int64.to_int32 x.u128_low))
        else None
      | Value (Int_ty.I32, x) -> Some x
      | Value (Int_ty.I64, (_f, x)) ->
        if Int64.(compare x min_i32_as_i64 >= 0 && compare x max_i32_as_i64 <= 0)
        then Some (wrap (Int64.to_int32 x))
        else None
      | Value (Int_ty.I128, (_f, x)) ->
        if
          C.i128_compare x (i128_of_i64 min_i32_as_i64) >= 0
          && C.i128_compare x (i128_of_i64 max_i32_as_i64) <= 0
        then Some (wrap (i128_to_i32_unchecked x))
        else None
  ;;
end
[@@ocamlformat "module-item-spacing = compact"]

module I64_basic : Basic with type t = i64 = struct
  type t = i64 [@@deriving eq, show, ord]

  let ty = Int_ty.I64
  let bits = 64
  let min_int = wrap Int64.min_int
  let max_int = wrap Int64.max_int
  let of_int_unchecked x = wrap (Int64.of_int x)
  let add_unchecked = wrap_op2 Int64.add
  let sub_unchecked = wrap_op2 Int64.sub
  let mul_unchecked = wrap_op2 Int64.mul
  let div_unchecked = wrap_op2 Int64.div
  let rem_unchecked = wrap_op2 Int64.rem
  let shift_left_unchecked = wrap_op2 (fun x y -> Int64.(shift_left x (to_int y)))
  let shift_right_unchecked = wrap_op2 (fun x y -> Int64.(shift_right x (to_int y)))
  let bit_not = wrap_op1 Int64.lognot
  let bit_or = wrap_op2 Int64.logor
  let bit_and = wrap_op2 Int64.logand
  let bit_xor = wrap_op2 Int64.logxor
  let of_int x = Some (of_int_unchecked x)

  let of_string s =
      let base, s = determine_base s in
      try Some (wrap (C.i64_scan_exn s base)) with
      | Failure _s -> None
  ;;

  let of_value =
      let of_small_int x = Some (wrap (Int64.of_int x)) in
      function
      | Value (Int_ty.U8, (_f, x)) -> of_small_int x
      | Value (Int_ty.U16, (_f, x)) -> of_small_int x
      | Value (Int_ty.I8, (_f, x)) -> of_small_int x
      | Value (Int_ty.I16, (_f, x)) -> of_small_int x
      | Value (Int_ty.U32, (_f, x)) -> Some (wrap (u64_of_u32 x))
      | Value (Int_ty.I32, (_f, x)) -> Some (wrap (Int64.of_int32 x))
      | Value (Int_ty.U64, (_f, x)) ->
        if Int64.(unsigned_compare x max_int <= 0) then Some (wrap x) else None
      | Value (Int_ty.U128, (_f, x)) ->
        if C.u128_compare x (u128_of_u64 Int64.max_int) <= 0
        then Some (wrap x.u128_low)
        else None
      | Value (Int_ty.I64, x) -> Some x
      | Value (Int_ty.I128, (_f, x)) ->
        if
          C.i128_compare x (i128_of_i64 Int64.min_int) >= 0
          && C.i128_compare x (i128_of_i64 Int64.max_int) <= 0
        then Some (wrap (i128_to_i64_unchecked x))
        else None
  ;;
end
[@@ocamlformat "module-item-spacing = compact"]

module I128_basic : Basic with type t = i128 = struct
  type t = i128 [@@deriving eq, show, ord]

  let ty = Int_ty.I128
  let bits = 128
  let min_int = wrap (C.i128_min ())
  let max_int = wrap (C.i128_max ())
  let of_int_unchecked x = wrap (i128_of_i64 (Int64.of_int x))
  let add_unchecked = wrap_op2 C.i128_add
  let sub_unchecked = wrap_op2 C.i128_sub
  let mul_unchecked = wrap_op2 C.i128_mul
  let div_unchecked = wrap_op2 C.i128_div
  let rem_unchecked = wrap_op2 C.i128_rem
  let shift_left_unchecked = wrap_op2 C.i128_shift_left
  let shift_right_unchecked = wrap_op2 C.i128_shift_right
  let bit_not = wrap_op1 C.i128_bit_not
  let bit_or = wrap_op2 C.i128_bit_or
  let bit_and = wrap_op2 C.i128_bit_and
  let bit_xor = wrap_op2 C.i128_bit_xor
  let of_int x = Some (of_int_unchecked x)

  let of_string s =
      let base, s = determine_base s in
      try Some (wrap (C.i128_scan_exn s base)) with
      | Failure _s -> None
  ;;

  let of_value =
      let max_i128_as_u128 = u128_of_i128_unchecked (C.i128_max ()) in
      let of_small_int x = Some (wrap (i128_of_i64 (Int64.of_int x))) in
      function
      | Value (Int_ty.U8, (_f, x)) -> of_small_int x
      | Value (Int_ty.U16, (_f, x)) -> of_small_int x
      | Value (Int_ty.I8, (_f, x)) -> of_small_int x
      | Value (Int_ty.I16, (_f, x)) -> of_small_int x
      | Value (Int_ty.U32, (_f, x)) -> Some (wrap (i128_of_u32 x))
      | Value (Int_ty.U64, (_f, x)) -> Some (wrap (i128_of_u64 x))
      | Value (Int_ty.U128, (_f, x)) ->
        if C.u128_compare x max_i128_as_u128 <= 0
        then Some (wrap (i128_of_u128_unchecked x))
        else None
      | Value (Int_ty.I32, (_f, x)) -> Some (wrap (i128_of_i32 x))
      | Value (Int_ty.I64, (_f, x)) -> Some (wrap (i128_of_i64 x))
      | Value (Int_ty.I128, x) -> Some x
  ;;
end
[@@ocamlformat "module-item-spacing = compact"]

module type S = sig
  type t [@@deriving eq, show, ord]

  val ty : t Int_ty.t
  val bits : int
  val zero : t
  val one : t
  val all_ones : t
  val min_int : t
  val max_int : t
  val is_signed : bool
  val of_int : int -> t option
  val of_string : string -> t option
  val succ : t -> t option
  val pred : t -> t option
  val neg : t -> t option
  val abs : t -> t option
  val add : t -> t -> t option
  val sub : t -> t -> t option
  val mul : t -> t -> t option
  val div : t -> t -> t option
  val rem : t -> t -> t option
  val bit_not : t -> t
  val bit_or : t -> t -> t
  val bit_and : t -> t -> t
  val bit_xor : t -> t -> t
  val shift_left : t -> t -> t option
  val shift_right : t -> t -> t option
  val min : t -> t -> t
  val max : t -> t -> t
  val of_int_exn : int -> t
  val of_string_exn : string -> t
  val succ_exn : t -> t
  val pred_exn : t -> t
  val neg_exn : t -> t
  val abs_exn : t -> t
  val add_exn : t -> t -> t
  val sub_exn : t -> t -> t
  val mul_exn : t -> t -> t
  val div_exn : t -> t -> t
  val rem_exn : t -> t -> t
  val shift_left_exn : t -> t -> t
  val shift_right_exn : t -> t -> t
  val to_string : t -> string
  val of_value : value -> t option
  val of_value_exn : value -> t
  val to_value : t -> value
end
[@@ocamlformat "module-item-spacing = compact"]

let overflow, underflow, div_by_zero = None, None, None

module Make (S : Basic) : S with type t = S.t = struct
  include S
  open S

  let zero, one = of_int_unchecked 0, of_int_unchecked 1

  let all_ones = if equal min_int zero then max_int else of_int_unchecked (-1)

  let is_signed = not (equal min_int zero)

  let pred x = if equal x min_int then underflow else Some (sub_unchecked x one)

  let succ x = if equal x max_int then overflow else Some (add_unchecked x one)

  let neg x =
      if equal x min_int && is_signed
      then overflow
      else if (not (equal x zero)) && not is_signed
      then underflow
      else Some (sub_unchecked zero x)
  ;;

  let abs x =
      if equal x min_int && is_signed
      then overflow
      else Some (if compare x zero < 0 then sub_unchecked zero x else x)
  ;;

  let add x y =
      if compare y zero >= 0 && compare x (sub_unchecked max_int y) > 0
      then overflow
      else if compare y zero < 0 && compare x (sub_unchecked min_int y) < 0
      then underflow
      else Some (add_unchecked x y)
  ;;

  let sub x y =
      if compare y zero < 0 && compare x (add_unchecked max_int y) > 0
      then overflow
      else if compare y zero >= 0 && compare x (add_unchecked min_int y) < 0
      then underflow
      else Some (sub_unchecked x y)
  ;;

  let mul x y =
      match () with
      | _
        when (compare x zero > 0
              && compare y zero > 0
              && compare x (div_unchecked max_int y) > 0)
             || (compare x zero < 0
                 && compare y zero < 0
                 && compare x (div_unchecked max_int y) < 0) -> overflow
      | _
        when (compare x zero < 0
              && compare y zero > 0
              && compare x (div_unchecked min_int y) < 0)
             || (compare x zero > 0
                 && is_signed
                 && compare y (of_int_unchecked (-1)) < 0
                 && compare x (div_unchecked min_int y) > 0) -> underflow
      | _ -> Some (mul_unchecked x y)
  ;;

  let div x y =
      if equal y zero
      then div_by_zero
      else if equal x min_int && is_signed && equal y (of_int_unchecked (-1))
      then overflow
      else Some (div_unchecked x y)
  ;;

  let rem x y =
      if equal y zero
      then div_by_zero
      else if equal x min_int && is_signed && equal y (of_int_unchecked (-1))
      then overflow
      else Some (rem_unchecked x y)
  ;;

  let shift_left x y =
      if compare y zero < 0
      then underflow
      else if compare y (of_int_unchecked bits) >= 0
      then overflow
      else Some (shift_left_unchecked x y)
  ;;

  let shift_right x y =
      if compare y zero < 0
      then underflow
      else if compare y (of_int_unchecked bits) >= 0
      then overflow
      else Some (shift_right_unchecked x y)
  ;;

  let min x y = if compare x y <= 0 then x else y

  let max x y = if compare x y >= 0 then x else y

  let of_int_exn x =
      match of_int x with
      | Some x -> x
      | None -> raise Out_of_range
  ;;

  let of_string_exn s =
      match of_string s with
      | Some x -> x
      | None -> raise Out_of_range
  ;;

  let succ_exn, pred_exn, neg_exn, abs_exn =
      let check f x =
          match f x with
          | Some z -> z
          | None -> raise Out_of_range
      in
      check succ, check pred, check neg, check abs
  ;;

  let add_exn, sub_exn, mul_exn, div_exn, rem_exn, shift_left_exn, shift_right_exn =
      let check f x y =
          match f x y with
          | Some z -> z
          | None -> raise Out_of_range
      in
      ( check add
      , check sub
      , check mul
      , check div
      , check rem
      , check shift_left
      , check shift_right )
  ;;

  let of_value_exn x =
      match of_value x with
      | Some y -> y
      | None -> raise Out_of_range
  ;;

  let to_value x = Value (ty, x)

  let to_string = show
end

module U8 : S with type t = u8 = Make (U8_basic)

module U16 : S with type t = u16 = Make (U16_basic)

module U32 : S with type t = u32 = Make (U32_basic)

module U64 : S with type t = u64 = Make (U64_basic)

module U128 : sig
  include S with type t = u128

  val split : t -> u64 * u64
end = struct
  include Make (U128_basic)

  let split (x : u128) : u64 * u64 =
      C.(wrap (unwrap x).u128_high, wrap (unwrap x).u128_low)
  ;;
end

module I8 : S with type t = i8 = Make (I8_basic)

module I16 : S with type t = i16 = Make (I16_basic)

module I32 : S with type t = i32 = Make (I32_basic)

module I64 : S with type t = i64 = Make (I64_basic)

module I128 : sig
  include S with type t = i128

  val split : t -> u64 * u64
end = struct
  include Make (I128_basic)

  let split (x : i128) : u64 * u64 =
      C.(wrap (unwrap x).i128_high, wrap (unwrap x).i128_low)
  ;;
end

let ops : type a. a Int_ty.t -> (module S with type t = a) = function
  | Int_ty.U8 -> (module U8)
  | Int_ty.U16 -> (module U16)
  | Int_ty.U32 -> (module U32)
  | Int_ty.U64 -> (module U64)
  | Int_ty.U128 -> (module U128)
  | Int_ty.I8 -> (module I8)
  | Int_ty.I16 -> (module I16)
  | Int_ty.I32 -> (module I32)
  | Int_ty.I64 -> (module I64)
  | Int_ty.I128 -> (module I128)
;;

let equal_value v1 v2 =
    match v1, v2 with
    | Value (ty, x), Value (ty', y) ->
      (match Int_ty.equate (ty, ty') with
       | Some Int_ty.Eq ->
         let (module S) = ops ty in
         S.equal x y
       | None -> false)
;;

let pp_value fmt (Value (ty, x)) =
    let (module S) = ops ty in
    S.pp fmt x
;;

let show_value v = Format.asprintf "%a" pp_value v

let is_zero (Value (ty, x)) =
    let (module S) = ops ty in
    S.(equal x zero)
;;

let is_one (Value (ty, x)) =
    let (module S) = ops ty in
    S.(equal x one)
;;

let is_all_ones (Value (ty, x)) =
    let (module S) = ops ty in
    S.(equal x all_ones)
;;
