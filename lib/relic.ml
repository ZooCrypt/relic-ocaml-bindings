open Ctypes

module R  = Ffi_bindings.Bindings(Ffi_generated)
module RT = Ffi_bindings.Types(Ffi_generated_types)

let sts_ok = RT.sts_ok
let sts_err = RT.sts_err

let cmp_lt = RT.cmp_lt
let cmp_eq = RT.cmp_eq
let cmp_gt = RT.cmp_gt
let cmp_ne = RT.cmp_ne

let bn_pos = RT.bn_pos
let bn_neg = RT.bn_neg

let fp_digs  = RT.fp_digs
let fp_bytes = RT.fp_bytes
let fp_digit = RT.fp_digit
let align    = RT.align
let fp_size  = R.fp_size

let core_init = R.core_init
let pc_param_set_any = R.pc_param_set_any

type bn = R.Bn.t
type g1 = R.G1.t
type g2 = R.G2.t
type gt = R.Gt.t

module Internal = struct
  let bn_new  = R.bn_new
  let bn_free = R.bn_free

  let bn_mod     = R.bn_mod
  let bn_set_dig = R.bn_set_dig
  let bn_set_2b  = R.bn_set_2b
  let bn_rand    = R.bn_rand
    
  let bn_size_str = R.bn_size_str
  let bn_write_str = R.bn_write_str

  let pc_param_level  = R.pc_param_level
  let pc_map_is_type1 = R.pc_map_is_type1
  let pc_map_is_type3 = R.pc_map_is_type3

  let g1_new       = R.g1_new
  let g1_free      = R.g1_free
  let g1_get_gen   = R.g1_get_gen
  let g1_get_ord   = R.g1_get_ord
  let g1_is_infty  = R.g1_is_infty
  let g1_set_infty = R.g1_set_infty
  let g1_cmp       = R.g1_cmp
  let g1_rand      = R.g1_rand
  let g1_is_valid  = R.g1_is_valid
  let g1_size_bin  = R.g1_size_bin
  let g1_read_bin  = R.g1_read_bin
  let g1_write_bin = R.g1_write_bin
  let g1_neg       = R.g1_neg
  let g1_add       = R.g1_add
  let g1_sub       = R.g1_sub
  let g1_mul       = R.g1_mul
  let g1_norm      = R.g1_norm
  let g1_mul_gen   = R.g1_mul_gen

  let g2_new       = R.g2_new
  let g2_free      = R.g2_free
  let g2_get_gen   = R.g2_get_gen
  let g2_get_ord   = R.g2_get_ord
  let g2_is_infty  = R.g2_is_infty
  let g2_set_infty = R.g2_set_infty
  let g2_cmp       = R.g2_cmp
  let g2_rand      = R.g2_rand
  let g2_is_valid  = R.g2_is_valid
  let g2_size_bin  = R.g2_size_bin
  let g2_read_bin  = R.g2_read_bin
  let g2_write_bin = R.g2_write_bin
  let g2_neg       = R.g2_neg
  let g2_add       = R.g2_add
  let g2_sub       = R.g2_sub
  let g2_mul       = R.g2_mul
  let g2_norm      = R.g2_norm
  let g2_mul_gen   = R.g2_mul_gen

  let gt_new       = R.gt_new
  let gt_free      = R.gt_free
  let gt_get_gen   = R.gt_get_gen
  let gt_get_ord   = R.gt_get_ord
  let gt_is_unity  = R.gt_is_unity
  let gt_zero      = R.gt_zero
  let gt_set_unity = R.gt_set_unity
  let gt_cmp       = R.gt_cmp
  let gt_rand      = R.gt_rand
  let gt_size_bin  = R.gt_size_bin
  let gt_read_bin  = R.gt_read_bin
  let gt_write_bin = R.gt_write_bin
  let gt_inv       = R.gt_inv
  let gt_mul       = R.gt_mul
  let gt_exp       = R.gt_exp
  
end

(* ** Finalizers *)

let deref_bn bn = Gc.finalise Internal.bn_free bn
(*
let deref_ptr finaliser e_p =
  let e = !@e_p in
  Gc.finalise finaliser e;
  e
*)
let deref_g1 g1 = Gc.finalise Internal.g1_free g1

let deref_g2 g2 = Gc.finalise Internal.g2_free g2

let deref_gt gt_p = Gc.finalise Internal.gt_free gt_p

(* ** Big numbers *)

let allocate_bn () =
  let bn_p = R.Bn.allocate ~finalise:deref_bn () in (* allocate bn_t which is a pointer to a structure *)
  Internal.bn_new bn_p;                   (* allocate bn_st, the actual struct with the values *)
  !@bn_p

let bn_mod a m =
  let bn = allocate_bn () in 
  Internal.bn_mod bn a m;
  bn

let bn_from_uint64 n =
  let bn = allocate_bn () in
  Internal.bn_set_dig bn n;
  bn

let bn_2powern n =
  let bn = allocate_bn () in
  Internal.bn_set_2b bn n;
  bn

let bn_rand ?(pos=false) ~bits =
  let bn = allocate_bn () in  
  Internal.bn_rand bn bn_pos bits;
  bn

let bn_size_str bn radix =
  Internal.bn_size_str bn radix

let bn_write_str bn radix =
  let n_chars = bn_size_str bn radix in
  let buf = Ctypes.allocate_n char ~count:n_chars in
  let _ = Internal.bn_write_str buf n_chars bn radix in
  Ctypes.coerce (ptr char) string buf

(* ** Groups *)

let compress_flag compress = if compress then 1 else 0

let pc_param_level () = Internal.pc_param_level ()

let pc_map_type () =
  if Internal.pc_map_is_type1 ()      then 1
  else if Internal.pc_map_is_type1 () then 3
  else -1

(* *** G1 *)

let allocate_g1 () =
  let g1_p = R.G1.allocate ~finalise:deref_g1 () in
  Internal.g1_new g1_p;
  !@ g1_p  

let g1_gen () =
  let g1 = allocate_g1 () in
  Internal.g1_get_gen g1;
  g1

let g1_ord () =
  let bn = allocate_bn () in
  Internal.g1_get_ord bn;
  bn
  
let g1_is_infty g1 =
  Internal.g1_is_infty g1

let g1_infty () =
  let g1 = allocate_g1 () in
  Internal.g1_set_infty g1;
  g1

let g1_equal g1 g1' =
  if (Internal.g1_cmp g1 g1') = cmp_eq then true
  else false

let g1_rand () =
  let g1 = allocate_g1 () in
  Internal.g1_rand g1;
  g1

let g1_is_valid g1 =
  Internal.g1_is_valid g1

let g1_size_bin ?(compress=false) g1 =
  let flag = compress_flag compress in
  Internal.g1_size_bin g1 flag

let g1_read_bin str =
  let g1 = allocate_g1 () in
  let length = String.length str in
  let buf = Ctypes.allocate_n char ~count:length in
  for i = 0 to length-1 do
    buf +@ i <-@ str.[i];
    ()
  done;
  Internal.g1_read_bin g1 buf length;
  g1

let g1_write_bin ?(compress=false) g1 =
  let flag = compress_flag compress in
  let n_chars = Internal.g1_size_bin g1 flag in
  let buf = Ctypes.allocate_n char ~count:n_chars in
  let _ = Internal.g1_write_bin buf n_chars g1 flag in
  Ctypes.string_from_ptr buf ~length:n_chars

let g1_neg g1 =
  let res = allocate_g1 () in
  Internal.g1_neg res g1;
  res

let g1_add g1 g1' =
  let res = allocate_g1 () in
  Internal.g1_add res g1 g1';
  res

let g1_sub g1 g1' =
  let res = allocate_g1 () in
  Internal.g1_sub res g1 g1';
  res

let g1_mul g1 k =
  let res = allocate_g1 () in
  Internal.g1_mul res g1 k;
  res

let g1_norm g1 =
  let res = allocate_g1 () in
  Internal.g1_norm res g1;
  res

let g1_mul_gen k =
  let res = allocate_g1 () in
  Internal.g1_mul_gen res k;
  res


(* *** G2 *)


let allocate_g2 () =
  let g2_p = R.G2.allocate ~finalise:deref_g2 () in
  Internal.g2_new g2_p;
  !@ g2_p  

let g2_gen () =
  let g2 = allocate_g2 () in
  Internal.g2_get_gen g2;
  g2

let g2_ord () =
  let bn = allocate_bn () in
  Internal.g2_get_ord bn;
  bn
  
let g2_is_infty g2 =
  Internal.g2_is_infty g2

let g2_infty () =
  let g2 = allocate_g2 () in
  Internal.g2_set_infty g2;
  g2

let g2_equal g2 g2' =
  if (Internal.g2_cmp g2 g2') = cmp_eq then true
  else false

let g2_rand () =
  let g2 = allocate_g2 () in
  Internal.g2_rand g2;
  g2

let g2_is_valid g2 =
  Internal.g2_is_valid g2

let g2_size_bin ?(compress=false) g2 =
  let flag = compress_flag compress in
  Internal.g2_size_bin g2 flag

let g2_read_bin str =
  let g2 = allocate_g2 () in
  let length = String.length str in
  let buf = Ctypes.allocate_n char ~count:length in
  for i = 0 to length-1 do
    buf +@ i <-@ str.[i];
    ()
  done;
  Internal.g2_read_bin g2 buf length;
  g2

let g2_write_bin ?(compress=false) g2 =
  let flag = compress_flag compress in
  let n_chars = Internal.g2_size_bin g2 flag in
  let buf = Ctypes.allocate_n char ~count:n_chars in
  let _ = Internal.g2_write_bin buf n_chars g2 flag in
  Ctypes.string_from_ptr buf ~length:n_chars

let g2_neg g2 =
  let res = allocate_g2 () in
  Internal.g2_neg res g2;
  res

let g2_add g2 g2' =
  let res = allocate_g2 () in
  Internal.g2_add res g2 g2';
  res

let g2_sub g2 g2' =
  let res = allocate_g2 () in
  Internal.g2_sub res g2 g2';
  res

let g2_mul g2 k =
  let res = allocate_g2 () in
  Internal.g2_mul res g2 k;
  res

let g2_norm g2 =
  let res = allocate_g2 () in
  Internal.g2_norm res g2;
  res

let g2_mul_gen k =
  let res = allocate_g2 () in
  Internal.g2_mul_gen res k;
  res


(* *** Gt *)

let allocate_gt () =
  let gt_p = R.Gt.allocate (*~finalise:deref_gt *) () in
  Internal.gt_new gt_p;
  gt_p

let gt_gen () =
  let gt_p = allocate_gt () in
  Internal.gt_get_gen gt_p;
  !@gt_p

let gt_ord () =
  let bn = allocate_bn () in
  Internal.gt_get_ord bn;
  bn
  
let gt_is_unity gt =
  let gt_p = allocate_gt () in
  gt_p <-@ gt;
  Internal.gt_is_unity gt_p

let gt_zero () =
  let gt_p = allocate_gt () in
  Internal.gt_zero gt_p;
  !@gt_p

let gt_unity () =
  let gt_p = allocate_gt () in
  Internal.gt_set_unity gt_p;
  !@gt_p

let gt_equal gt gt' =
  let gt_p  = allocate_gt () in
  let gt'_p = allocate_gt () in
  gt_p  <-@ gt;
  gt'_p <-@ gt';
  if (Internal.gt_cmp gt_p gt'_p) = cmp_eq then true
  else false

let gt_rand () =
  let gt_p = allocate_gt () in
  Internal.gt_rand gt_p;
  !@gt_p

let gt_size_bin ?(compress=false) gt =
  let flag = compress_flag compress in
  let gt_p = allocate_gt () in
  gt_p <-@ gt;
  Internal.gt_size_bin gt_p flag

let gt_read_bin str =
  let gt_p = allocate_gt () in
  let length = String.length str in
  let buf = Ctypes.allocate_n char ~count:length in
  for i = 0 to length-1 do
    buf +@ i <-@ str.[i];
    ()
  done;
  Internal.gt_read_bin gt_p buf length;
  !@gt_p

let gt_write_bin ?(compress=false) gt =
  let flag = compress_flag compress in
  let gt_p = allocate_gt () in
  gt_p <-@ gt;
  let n_chars = Internal.gt_size_bin gt_p flag in
  let buf = Ctypes.allocate_n char ~count:n_chars in
  let _ = Internal.gt_write_bin buf n_chars gt_p flag in
  Ctypes.string_from_ptr buf ~length:n_chars

let gt_inv gt =
  let gt_p = allocate_gt () in
  gt_p  <-@ gt;
  let res = allocate_gt () in
  Internal.gt_inv res gt_p;
  !@res

let gt_mul gt gt' =
  let gt_p  = allocate_gt () in
  let gt'_p = allocate_gt () in
  gt_p  <-@ gt;
  gt'_p <-@ gt';
  let res = allocate_gt () in
  Internal.gt_mul res gt_p gt'_p;
  !@res

let gt_exp gt k =
  let gt_p  = allocate_gt () in
  gt_p  <-@ gt;
  let res = allocate_gt () in
  Internal.gt_exp res gt_p k;
  !@res
