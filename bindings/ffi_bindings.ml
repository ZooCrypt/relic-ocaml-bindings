(* * Bindings for Relic *)

open Ctypes

(* ** Types module *)

module Types (F: Cstubs.Types.TYPE) = struct
  open F
  let sts_ok = constant "STS_OK" int
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
end
                                         
