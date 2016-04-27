open Ctypes

val sts_ok : int

val core_init : unit -> int
val pc_param_set_any : unit -> int

type bn
type g1

module Internal : sig
  val bn_new  : bn ptr -> unit
  val bn_free : bn -> unit

  val bn_mod     : bn -> bn -> bn -> unit
  val bn_set_dig : bn -> Unsigned.UInt64.t -> unit
  val bn_set_2b  : bn -> int -> unit
  val bn_rand    : bn -> int -> int -> unit

  val bn_size_str : bn -> int -> int
  val bn_write_str : char ptr -> int -> bn -> int -> unit

  val g1_new  : g1 ptr -> unit
  val g1_free : g1 -> unit
  val g1_get_gen : g1 -> unit
end

val bn_rand : ?pos:bool -> bits:int -> bn
val bn_size_str  : bn -> int -> int
val bn_write_str : bn -> int -> string

val g1_gen       : unit -> g1
val g1_ord       : unit -> bn
val g1_is_unity  : g1 -> bool
val g1_unity     : unit -> g1
val g1_equal     : g1 -> g1 -> bool
val g1_rand      : unit -> g1
val g1_is_valid  : g1 -> bool
val g1_size_bin   : g1 -> int
val g1_read_bin  : string -> g1
val g1_write_bin : g1 -> string
