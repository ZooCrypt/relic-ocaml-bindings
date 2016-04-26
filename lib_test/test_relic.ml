open OUnit
open Lean

module R = Relic
module F  = Format

let t_XXX =
  "XXX" >:: fun () ->
    assert_bool "a1" true
    
let _ =
  let suite = "relic" >::: [
        t_XXX
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()
