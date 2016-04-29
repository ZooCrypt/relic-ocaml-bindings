open OUnit

module R = Relic
module F  = Format

let padding a align = 
  if align > 1 then
    if (a mod align) = 0 then 0
    else align - (a mod align)
  else 0    

let t_bn =
  "bn" >:: fun () ->
   assert_equal (R.core_init ()) R.sts_ok;
   assert_equal (R.pc_param_set_any ()) R.sts_ok;
   assert_equal (R.fp_digs + (padding R.fp_bytes R.align) / (R.fp_digit / 8)) R.fp_size;
   for i = 1 to 5 do
     let n = R.bn_rand 256 in
     let a = R.bn_size_str n 2 in
     let b = R.bn_write_str n 10 in
     F.printf "%d -> %s\n" a b;
   done
   ;
   let order = R.bn_write_str (R.g1_ord ()) 10 in
   F.printf "Curve Type: %d\n" (R.pc_map_type ());
   F.printf "G1 order: %s\n" order;
   F.printf "G1 order size: %d bits\n" (R.bn_size_str (R.g1_ord ()) 2);
   let g = R.g1_gen () in
   let u = R.g1_infty () in
   F.printf "%b %b\n" (R.g1_is_infty g) (R.g1_is_infty u);
   F.printf "g = u: %b\n" (R.g1_equal g u);
   F.printf "u = u: %b\n" (R.g1_equal u u);
   let r = R.g1_rand () in
   F.printf "u = r: %b\n" (R.g1_equal u r);
   F.printf "is_valid r: %b\n" (R.g1_is_valid r);
   F.printf "u size: %d\n" (R.g1_size_bin u);
   F.printf "g size: %d\n" (R.g1_size_bin g);
   F.printf "%d  gen: %S\n" (String.length (R.g1_write_bin g)) (R.g1_write_bin g);
   F.printf "%d -gen: %S\n" (String.length (R.g1_write_bin (R.g1_neg g))) (R.g1_write_bin (R.g1_neg g));
   F.printf " gen: %S\n" (R.g1_write_bin ~compress:true g);
   F.printf "-gen: %S\n" (R.g1_write_bin ~compress:true (R.g1_neg g));
   F.printf " gen2: %S\n" (R.g2_write_bin (R.g2_gen ()));
   F.printf " -gen2: %S\n" (R.g2_write_bin (R.g2_neg (R.g2_gen ())));
   F.printf " gen2: %S\n" (R.g2_write_bin ~compress:true (R.g2_gen ()));
   F.printf " -gen2: %S\n" (R.g2_write_bin ~compress:true (R.g2_neg (R.g2_gen ())));
   F.printf " unit: %S\n" (R.g1_write_bin u);
   F.printf " random: %S\n" (R.g1_write_bin r);
   F.printf "-random: %S\n" (R.g1_write_bin (R.g1_neg r));
   F.printf "%d %d\n" (R.Internal.g1_size_bin r 0) (R.Internal.g1_size_bin r 1);
   F.printf "gen - gen: %S\n" (R.g1_write_bin (R.g1_add g (R.g1_neg g)));
   (* FIXME: It seems g1_read_bin only accepts strings of certain length *)
   let g_str = R.g1_write_bin g in
   let g' = R.g1_read_bin g_str in
   F.printf "g = g': %b\n" (R.g1_equal g g');
   F.printf "g : %d %S\n" (String.length g_str) g_str;
   F.printf "g': %d %S\n" (String.length (R.g1_write_bin g')) (R.g1_write_bin g');
   
   F.printf "param_level: %d\n" (R.pc_param_level ());
   F.printf "FP_DIGS: %d\nFP_BYTES: %d\nFP_DIGIT: %d\nALIGN: %d" R.fp_digs R.fp_bytes R.fp_digit R.align;
   ()
   
let _ =
  let suite = "relic" >::: [
        t_bn
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()

