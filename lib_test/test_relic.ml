open OUnit

module R = Relic
module F  = Format

let t_bn =
  "bn" >:: fun () ->
   assert_equal (R.core_init ()) R.sts_ok;
   assert_equal (R.pc_param_set_any ()) R.sts_ok;
   for i = 0 to 10 do
     let n = R.bn_rand 256 in
     let a = R.bn_size_str n 2 in
     let b = R.bn_write_str n 10 in
     F.printf "%d -> %s\n" a b;
   done
   ;
   let order = R.bn_write_str (R.g1_ord () ) 10 in
   F.printf "G1 order: %s\n" order;
   let g = R.g1_gen () in
   let u = R.g1_unity () in
   F.printf "%b %b\n" (R.g1_is_unity g) (R.g1_is_unity u);
   F.printf "%b\n" (R.g1_equal g u);
   F.printf "%b\n" (R.g1_equal u u);
   let r = R.g1_rand () in
   F.printf "%b\n" (R.g1_equal u r);
   F.printf "%b\n" (R.g1_is_valid r);
   F.printf "%d\n" (R.g1_size_bin u);
   F.printf "%s\n" (R.g1_write_bin g);
   ()
   
let _ =
  let suite = "relic" >::: [
        t_bn
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()
