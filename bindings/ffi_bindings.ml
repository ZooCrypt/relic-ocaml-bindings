(* * Bindings for Relic *)

open Ctypes

(* ** Types module *)

module Types (F: Cstubs.Types.TYPE) = struct
  open F
  let sts_ok  = constant "STS_OK" int
  let sts_err = constant "STS_ERR" int

  let cmp_lt  = constant "CMP_LT" int
  let cmp_eq  = constant "CMP_EQ" int
  let cmp_gt  = constant "CMP_GT" int
  let cmp_ne  = constant "CMP_NE" int

  let bn_pos = constant "BN_POS" int
  let bn_neg = constant "BN_NEG" int
end

(* ** Bindings module *)

module Bindings (F : Cstubs.FOREIGN) = struct
  open F
  
(* *** Typedefs *)

  module Typedef (TN : sig val type_name : string end) : sig
    type t
    val t : t Ctypes.typ
    val allocate : ?finalise:(t -> unit) -> unit -> t ptr
    val to_string : t -> string
  end = struct
    type t = unit ptr
    let t = typedef (ptr void) TN.type_name

    let allocate ?finalise () =
      let finalise = match finalise with
        | Some f -> Some (fun p -> f !@p)
        | None   -> None
      in
      allocate ?finalise t null
    let to_string p =
      Nativeint.to_string @@ raw_address_of_ptr p
  end

(* *** Initialization *)

  let core_init = foreign "core_init" (void @-> returning int)
  let pc_param_set_any = foreign "pc_param_set_any" (void @-> returning int)

(* *** Big numbers *)

  module Bn = Typedef(struct let type_name = "bn_t" end)
    
  let bn = Bn.t
    
  let bn_new  = foreign "w_bn_new" (ptr bn @-> returning void)
  let bn_free = foreign "bn_free" (bn @-> returning void)

  let bn_mod     = foreign "bn_mod"      (bn @-> bn @-> bn @-> returning void)
  let bn_set_dig = foreign "bn_set_dig"  (bn @-> uint64_t @-> returning void)
  let bn_set_2b  = foreign "bn_set_dig"  (bn @-> int @-> returning void)
  let bn_rand    = foreign "bn_rand"     (bn @-> int @-> int @-> returning void)

  let bn_size_str = foreign "bn_size_str" (bn @-> int @-> returning int)
  let bn_write_str = foreign "bn_write_str" (ptr char @-> int @-> bn @-> int @-> returning void)
  
(* *** Groups *)

(* **** G1 *)
    
  module G1 = Typedef(struct let type_name = "g1_t" end)
    
  let g1 = G1.t
    
  let g1_new  = foreign "w_g1_new" (ptr g1 @-> returning void)
  let g1_free = foreign "g1_free" (g1 @-> returning void)
    
  let g1_get_gen   = foreign "g1_get_gen"   (g1 @-> returning void)    
  let g1_get_ord   = foreign "g1_get_ord"   (bn @-> returning void)
  let g1_is_infty  = foreign "g1_is_infty"  (g1 @-> returning bool)
  let g1_set_infty = foreign "g1_set_infty" (g1 @-> returning void)
  let g1_cmp       = foreign "g1_cmp"       (g1 @-> g1 @-> returning int)
  let g1_rand      = foreign "g1_rand"      (g1 @-> returning void)
  let g1_is_valid  = foreign "g1_is_valid"  (g1 @-> returning bool)
  let g1_size_bin  = foreign "g1_size_bin"  (g1 @-> int @-> returning int)
  let g1_read_bin  = foreign "g1_read_bin"  (g1 @-> ptr char @-> int @-> returning void)
  let g1_write_bin = foreign "g1_write_bin" (ptr char @-> int @-> g1 @-> int @-> returning void)
end
