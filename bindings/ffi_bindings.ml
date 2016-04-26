(* * Bindings for Lean *)
open Ctypes

(* ** Types module *)

module Types (F: Cstubs.Types.TYPE) = struct
  open F
  let sts_ok  = constant "STS_OK" int
end

(* ** Bindings module *)

module Bindings (F : Cstubs.FOREIGN) = struct
  open F
  
(* *** Typedefs *)

  module Typedef (TN : sig val type_name : string end) : sig
    type t
    val t : t Ctypes.typ
    val allocate : ?finalise:(t -> unit) -> unit -> t ptr
  end = struct
    type t = unit ptr
    let t = typedef (ptr void) TN.type_name

    let allocate ?finalise () =
      let finalise = match finalise with
        | Some f -> Some (fun p -> f !@p)
        | None   -> None
      in
      allocate ?finalise t null
  end

(* *** Initialization *)

  let core_init = foreign "core_init" (void @-> returning int)

(* *** Big numbers *)

  module Bn = Typedef(struct let type_name = "bn_t" end)

  let bn = Bn.t

  let bn_null = foreign "bn_null" (bn @-> returning void)
  let bn_new  = foreign "bn_new"  (bn @-> returning void)
  let bn_free = foreign "bn_free" (bn @-> returning void)

  let bn_mod     = foreign "bn_mod"      (bn @-> bn @-> bn @-> returning void)
  let bn_set_dig = foreign "bn_set_dig"  (bn @-> uint64_t @-> returning void)
  let bn_set_2b  = foreign "bn_set_dig"  (bn @-> int @-> returning void)
  let bn_rand    = foreign "bn_rand"     (bn @-> int @-> int @-> returning void)

end
                                         
