open Ctypes

module R  = Ffi_bindings.Bindings(Ffi_generated)
module RT = Ffi_bindings.Types(Ffi_generated_types)


let sts_ok = RT.sts_ok

let core_init = R.core_init

type bn = R.Bn.t

let bn_null = R.bn_null
let bn_new  = R.bn_new
let bn_free = R.bn_free

(* FIXME: do we want to use out arguments or allocate a new bn and return it? *)

let bn_mod     = R.bn_mod
let bn_set_dig = R.bn_set_dig
let bn_set_2b  = R.bn_set_2b
let bn_rand    = R.bn_rand
