val sts_ok : int

val core_init : unit -> int

type bn

val bn_null : bn -> unit
val bn_new  : bn -> unit
val bn_free : bn -> unit

(* FIXME: do we want to use out arguments or allocate a new bn and return it? *)

val bn_mod     : bn -> bn -> bn -> unit
val bn_set_dig : bn -> Unsigned.UInt64.t -> unit
val bn_set_2b  : bn -> int -> unit
val bn_rand    : bn -> int -> int -> unit
