open OUnit

module R = Relic
module F  = Format

let t_bn =
  "bn" >:: fun () ->
   assert_equal (R.core_init ()) R.sts_ok;
   assert_equal (R.pc_param_set_any ()) R.sts_ok;
   for i = 0 to 1000 do
     ignore (R.bn_rand 256)
   done
   
let _ =
  let suite = "relic" >::: [
        t_bn
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()
