open Ctypes

module R  = Ffi_bindings.Bindings(Ffi_generated)
module RT = Ffi_bindings.Types(Ffi_generated_types)


let sts_ok = RT.sts_ok

let bn_pos = 0
let bn_neg = 1

let core_init = R.core_init

type bn = R.Bn.t

module Internal = struct
  let bn_new  = R.bn_new
  let bn_free = R.bn_free

  (* FIXME: do we want to use out arguments or allocate a new bn and return it? *)

  let bn_mod     = R.bn_mod
  let bn_set_dig = R.bn_set_dig
  let bn_set_2b  = R.bn_set_2b
  let bn_rand    = R.bn_rand
end

let bn_rand ?(pos=false) ~bits =
  let open Internal in
  let bn = !@(R.Bn.allocate ()) in (* allocate bn_t which is a pointer to a structure *)
  bn_new bn;                       (* allocate bn_st, the actual struct with the values *)
  bn_rand bn bn_neg bits;
  bn
