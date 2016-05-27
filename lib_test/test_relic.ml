open OUnit

module R = Relic
module F  = Format

let test_functions =
  "test_functions" >:: fun () ->
   assert_equal (R.core_init ()) R.sts_ok;
   assert_equal (R.pc_param_set_any ()) R.sts_ok;
   for i = 1 to 5 do
     let n = R.bn_rand 256 in
     let a = R.bn_size_str n 2 in
     let b = R.bn_write_str n 10 in
     F.printf "%d -> %s\n" a b;
   done
   ;
   let n = R.bn_from_uint64 (Unsigned.UInt64.of_int 25) in
   let n2 = R.bn_add n n in
   let n3 = R.bn_sqrt n in
   let n4 = R.bn_one () in
   F.printf "%s (%d HM), %s, %s, %s\n\n" (R.bn_write_str n 10) (R.bn_ham n) (R.bn_write_str n2 10) (R.bn_write_str n3 10) (R.bn_write_str n4 10);

   let prime = R.bn_from_uint64 (Unsigned.UInt64.of_string "70421107") in
   F.printf "is_prime %s? %b\n" (R.bn_write_str prime 10) (R.bn_is_prime prime);
   let (d,u,v) = R.bn_gcd_ext n prime in
   let prod = R.bn_mod (R.bn_mul n u) prime in
   let u = R.bn_mod u prime in
   F.printf "%s * %s (mod %s) = %s\n\n" (R.bn_write_str n 10) (R.bn_write_str u 10) (R.bn_write_str prime 10) (R.bn_write_str prod 10);
   let order = R.bn_write_str (R.g1_ord ()) 10 in
   F.printf "Curve Type: %d\n" (R.pc_map_type ());
   F.printf "G1 order: %s\n" order;
   F.printf "G1 order size: %d bits\n" (R.bn_size_str (R.g1_ord ()) 2);
   F.printf "G2 order: %s\n" (R.bn_write_str (R.g2_ord ()) 10);
   F.printf "Gt order: %s\n" (R.bn_write_str (R.gt_ord ()) 10);
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
   let gt = R.gt_gen () in
   let gt_0 = R.gt_zero () in
   let gt_u = R.gt_unity () in
   F.printf "Is gt = unity? %b\n" (R.gt_is_unity gt);
   F.printf "Is gt_u = unity? %b\n" (R.gt_is_unity gt_u);
   F.printf "Is gt_0 = gt_0? %b\n" (R.gt_equal gt_0 gt_0);
   F.printf "Is gt_0 = gt? %b\n"   (R.gt_equal gt_0 gt);
   let gt_r = R.gt_rand () in
   F.printf "gt_r size: %d\n" (R.gt_size_bin gt_r);
   F.printf "gt_0 size: %d\n" (R.gt_size_bin gt_0);
   F.printf "gt_r: %S\n" (R.gt_write_bin gt_r);
   let gt_r_inv = R.gt_inv gt_r in
   F.printf "gt_0 = gt_r * gt_r_inv? %b\n" (R.gt_equal gt_0 (R.gt_mul gt_r gt_r_inv));
   F.printf "gt_u = gt_r * gt_r_inv? %b\n" (R.gt_equal gt_u (R.gt_mul gt_r gt_r_inv));
   let _gt_r' = R.gt_exp gt_r (R.bn_rand 256) in

   F.printf "e(g1,g2) = gt? %b" (R.gt_equal (R.e_pairing (R.g1_gen ()) (R.g2_gen ())) gt);
   ()


let test_pairing =
  "test_pairing" >:: fun () ->
    assert_equal (R.core_init ()) R.sts_ok;
    assert_equal (R.pc_param_set_any ()) R.sts_ok;
    
  (* Get generators in G1 and G2 *)
    let g1 = R.g1_gen () in
    let g2 = R.g2_gen () in
    
  (* Sample random exponents *)
    let a = R.bn_rand 256 in
    let b = R.bn_rand 256 in
    
    let ab = R.bn_mul a b in
    
    let g1_a = R.g1_mul g1 a in
    let g2_b = R.g2_mul g2 b in
    
  (* Compute pairings *)
    let z1 = R.e_pairing g1_a g2_b in
    let z2 = R.gt_exp (R.e_pairing g1 g2) ab in 
    
  (* Check *)
    if R.gt_equal z1 z2 then F.printf "Pairing test succedded!"
    else assert false

let _ =
  let suite = "relic" >::: [
    (* test_functions; *)
    test_pairing
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()

