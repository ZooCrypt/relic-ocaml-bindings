open Ctypes

module R  = Ffi_bindings.Bindings(Ffi_generated)
module RT = Ffi_bindings.Types(Ffi_generated_types)


let sts_ok = RT.sts_ok
let bn_pos = RT.bn_pos
let bn_neg = RT.bn_neg

let core_init = R.core_init
let pc_param_set_any = R.pc_param_set_any

type bn = R.Bn.t

module Internal = struct
  let bn_new  = R.bn_new
  let bn_free = R.bn_free

  let bn_mod     = R.bn_mod
  let bn_set_dig = R.bn_set_dig
  let bn_set_2b  = R.bn_set_2b
  let bn_rand    = R.bn_rand
    
  let bn_size_str = R.bn_size_str
  let bn_write_str = R.bn_write_str
end

let bn_rand ?(pos=false) ~bits =
  let open Internal in
  let bn_p = R.Bn.allocate () in (* allocate bn_t which is a pointer to a structure *)
  bn_new bn_p;                   (* allocate bn_st, the actual struct with the values *)
  let bn = !@ bn_p in
  bn_rand bn bn_pos bits;
  bn

let bn_size_str bn radix =
  Internal.bn_size_str bn radix

let bn_write_str bn radix =
  let n_chars = bn_size_str bn radix in
  let buf = Ctypes.allocate_n char ~count:n_chars in
  let _ = Internal.bn_write_str buf n_chars bn radix in
  coerce (ptr char) string buf

