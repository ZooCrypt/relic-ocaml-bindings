open Ctypes

val sts_ok : int
val sts_ok : int
val sts_err : int

val cmp_lt : int
val cmp_eq : int
val cmp_gt : int
val cmp_ne : int

val bn_pos : int
val bn_neg : int

val core_init : unit -> int
val pc_param_set_any : unit -> int

type bn
type g1
type g2
type gt

module Internal : sig
  val bn_new  : bn ptr -> unit
  val bn_free : bn -> unit

  val bn_mod     : bn -> bn -> bn -> unit
  val bn_set_dig : bn -> Unsigned.UInt64.t -> unit
  val bn_set_2b  : bn -> int -> unit
  val bn_rand    : bn -> int -> int -> unit

  val bn_size_str  : bn -> int -> int
  val bn_write_str : char ptr -> int -> bn -> int -> unit

  val pc_param_level  : unit -> int
  val pc_map_is_type1 : unit -> bool
  val pc_map_is_type3 : unit -> bool

  val g1_new       : g1 ptr -> unit
  val g1_free      : g1 -> unit
  val g1_get_gen   : g1 -> unit
  val g1_get_ord   : bn -> unit
  val g1_is_infty  : g1 -> bool
  val g1_set_infty : g1 -> unit
  val g1_cmp       : g1 -> g1 -> int
  val g1_rand      : g1 -> unit
  val g1_is_valid  : g1 -> bool
  val g1_size_bin  : g1 -> int -> int
  val g1_read_bin  : g1 -> char ptr -> int -> unit
  val g1_write_bin : char ptr -> int -> g1 -> int -> unit
  val g1_neg       : g1 -> g1 -> unit
  val g1_add       : g1 -> g1 -> g1 -> unit
  val g1_sub       : g1 -> g1 -> g1 -> unit
  val g1_mul       : g1 -> g1 -> bn -> unit
  val g1_norm      : g1 -> g1 -> unit
  val g1_mul_gen   : g1 -> bn -> unit

  val g2_new       : g2 ptr -> unit
  val g2_free      : g2 -> unit
  val g2_get_gen   : g2 -> unit
  val g2_get_ord   : bn -> unit
  val g2_is_infty  : g2 -> bool
  val g2_set_infty : g2 -> unit
  val g2_cmp       : g2 -> g2 -> int
  val g2_rand      : g2 -> unit
  val g2_is_valid  : g2 -> bool
  val g2_size_bin  : g2 -> int -> int
  val g2_read_bin  : g2 -> char ptr -> int -> unit
  val g2_write_bin : char ptr -> int -> g2 -> int -> unit
  val g2_neg       : g2 -> g2 -> unit
  val g2_add       : g2 -> g2 -> g2 -> unit
  val g2_sub       : g2 -> g2 -> g2 -> unit
  val g2_mul       : g2 -> g2 -> bn -> unit
  val g2_norm      : g2 -> g2 -> unit
  val g2_mul_gen   : g2 -> bn -> unit

  val gt_new       : gt ptr -> unit
  val gt_free      : gt -> unit
  val gt_get_gen   : gt -> unit
  val gt_get_ord   : bn -> unit
  val gt_is_unity  : gt -> bool
  val gt_zero      : gt -> unit
  val gt_set_unity : gt -> unit
  val gt_cmp       : gt -> gt -> int
  val gt_rand      : gt -> unit
  val gt_size_bin  : gt -> int -> int
  val gt_read_bin  : gt -> char ptr -> int -> unit
  val gt_write_bin : char ptr -> int -> gt -> int -> unit
  val gt_inv       : gt -> gt -> unit
  val gt_mul       : gt -> gt -> gt -> unit
  val gt_exp       : gt -> gt -> bn -> unit

end

val bn_mod         : bn -> bn -> bn
val bn_from_uint64 : Unsigned.UInt64.t -> bn
val bn_2powern     : int -> bn
val bn_rand        : ?pos:bool -> bits:int -> bn
val bn_size_str    : bn -> int -> int
val bn_write_str   : bn -> int -> string

val pc_param_level : unit -> int
val pc_map_type    : unit -> int

val g1_gen       : unit -> g1
val g1_ord       : unit -> bn
val g1_is_infty  : g1 -> bool
val g1_infty     : unit -> g1
val g1_equal     : g1 -> g1 -> bool
val g1_rand      : unit -> g1
val g1_is_valid  : g1 -> bool
val g1_size_bin  : ?compress:bool -> g1 -> int
val g1_read_bin  : string -> g1
val g1_write_bin : ?compress:bool -> g1 -> string
val g1_neg       : g1 -> g1
val g1_add       : g1 -> g1 -> g1
val g1_sub       : g1 -> g1 -> g1
val g1_mul       : g1 -> bn -> g1
val g1_norm      : g1 -> g1
val g1_mul_gen   : bn -> g1

val g2_gen       : unit -> g2
val g2_ord       : unit -> bn
val g2_is_infty  : g2 -> bool
val g2_infty     : unit -> g2
val g2_equal     : g2 -> g2 -> bool
val g2_rand      : unit -> g2
val g2_is_valid  : g2 -> bool
val g2_size_bin  : ?compress:bool -> g2 -> int
val g2_read_bin  : string -> g2
val g2_write_bin : ?compress:bool -> g2 -> string
val g2_neg       : g2 -> g2
val g2_add       : g2 -> g2 -> g2
val g2_sub       : g2 -> g2 -> g2
val g2_mul       : g2 -> bn -> g2
val g2_norm      : g2 -> g2
val g2_mul_gen   : bn -> g2

val gt_gen       : unit -> gt
val gt_ord       : unit -> bn
val gt_is_unity  : gt -> bool
val gt_zero      : unit -> gt
val gt_unity     : unit -> gt
val gt_equal     : gt -> gt -> bool
val gt_rand      : unit -> gt
val gt_size_bin  : ?compress:bool -> gt -> int
val gt_read_bin  : string -> gt
val gt_write_bin : ?compress:bool -> gt -> string
val gt_inv       : gt -> gt
val gt_mul       : gt -> gt -> gt
val gt_exp       : gt -> bn -> gt
