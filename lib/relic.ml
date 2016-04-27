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

let core_init = R.core_init
let pc_param_set_any = R.pc_param_set_any

type bn = R.Bn.t
type g1 = R.G1.t

module Internal = struct
  let bn_new  = R.bn_new
  let bn_free = R.bn_free

  let bn_mod     = R.bn_mod
  let bn_set_dig = R.bn_set_dig
  let bn_set_2b  = R.bn_set_2b
  let bn_rand    = R.bn_rand
    
  let bn_size_str = R.bn_size_str
  let bn_write_str = R.bn_write_str

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
end

(* ** Finalizers *)

let deref_bn bn = Gc.finalise Internal.bn_free bn

let deref_g1 g1 = Gc.finalise Internal.g1_free g1

(* ** Big numbers *)

let allocate_bn () =
  let bn_p = R.Bn.allocate ~finalise:deref_bn () in (* allocate bn_t which is a pointer to a structure *)
  Internal.bn_new bn_p;                   (* allocate bn_st, the actual struct with the values *)
  !@ bn_p

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
  coerce (ptr char) string buf

(* ** Groups *)

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
  
let g1_is_unity g1 =
  Internal.g1_is_infty g1

let g1_unity () =
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

let g1_size_bin g1 =
  let flag = 0 in
  Internal.g1_size_bin g1 flag

let g1_read_bin str =
  let g1 = allocate_g1 () in
  let length = String.length str in
  let buf = Ctypes.allocate_n char ~count:length in
  (* buf <-@ str; *) (* FIXME *)
  Internal.g1_read_bin g1 buf length;
  g1

let g1_write_bin g1 =
  let flag = 0 in
  let n_chars = Internal.g1_size_bin g1 flag in
  let buf = Ctypes.allocate_n char ~count:n_chars in
  let _ = Internal.g1_write_bin buf n_chars g1 flag in
  coerce (ptr char) string buf
