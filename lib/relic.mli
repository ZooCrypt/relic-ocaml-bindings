open Ctypes

val sts_ok : int

val core_init : unit -> int
val pc_param_set_any : unit -> int

type bn

module Internal : sig
  val bn_new  : bn ptr -> unit
  val bn_free : bn -> unit

  val bn_mod     : bn -> bn -> bn -> unit
  val bn_set_dig : bn -> Unsigned.UInt64.t -> unit
  val bn_set_2b  : bn -> int -> unit
  val bn_rand    : bn -> int -> int -> unit

  val bn_size_str : bn -> int -> int
  val bn_write_str : char ptr -> int -> bn -> int -> unit
end

val bn_rand : ?pos:bool -> bits:int -> bn
val bn_size_str  : bn -> int -> int
val bn_write_str : bn -> int -> string

