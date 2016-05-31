open OUnit
open Core_kernel.Std

module R = Relic
module F = Format
module L = List

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
   F.printf "G2 order: %s\n" (R.bn_write_str (R.g2_ord ()) 2);
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

   F.printf "e(g1,g2) = gt? %b\n" (R.gt_equal (R.e_pairing (R.g1_gen ()) (R.g2_gen ())) gt);
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
    if R.gt_equal z1 z2 then F.printf "Pairing test succedded!\n"
    else assert false


let test_sps =
  "test_sps" >:: fun () ->
    assert_equal (R.core_init ()) R.sts_ok;
    assert_equal (R.pc_param_set_any ()) R.sts_ok;
    
    (* Get generators in G1 and G2 *)
    let g1 = R.g1_gen () in
    let g2 = R.g2_gen () in
    

    (* Key generation *)
    let keyGen () =
      let v = R.bn_rand 256 in
      let w = R.bn_rand 256 in

      let sk = (v,w) in
      let pk = (R.g1_mul g1 v, R.g1_mul g1 w) in
      sk, pk
    in

    (* Sign *)
    let sign (v,w) m =
      let r = R.bn_rand 256 in
      
      let s1 = R.g1_mul g1 r in
      let s2 = R.g2_mul g2 r in
      let s3 = R.g2_add (R.g2_add (R.g2_mul m v) (R.g2_mul g2 (R.bn_mul r r))) (R.g2_mul g2 w) in
      (s1,s2,s3)
    in

    (* Verify *)
    let verif (pv,pw) m (s1,s2,s3) =
      let eq1 = R.gt_equal (R.e_pairing s1 g2) (R.e_pairing g1 s2) in
      let eq2 =
        R.gt_equal
          (R.e_pairing g1 s3)
          (R.gt_mul (R.gt_mul (R.e_pairing pv m) (R.e_pairing pw g2)) (R.e_pairing s1 s2))
      in
      eq1 && eq2
    in

    let sk, pk = keyGen () in

    let msg = R.g2_rand () in
    let (s1,s2,s3) = sign sk msg in

    let msg2 = R.g2_rand () in
    
    (* Check *)
    if (verif pk msg (s1,s2,s3)) && (not (verif pk msg2 (s1,s2,s3))) then
      F.printf "Signature test succedded!\n"
    else assert false

let test_abe =

  (* ABE described in Improved Dual System ABE in Prime-Order Groups via Predicate Encodings *)
  
  "test_abe" >:: fun () ->
    assert_equal (R.core_init ()) R.sts_ok;
    assert_equal (R.pc_param_set_any ()) R.sts_ok;

    (* Util *)

    let mk_list el n =
      let rec aux output n =
        if n = 0 then output
        else aux (el :: output) (n-1)
      in
      aux [] n
    in

    let sample_list ~f k =
      let rec aux list k = 
        if k = 0 then list
        else aux (list @ [f ()]) (k-1)
      in
      aux [] k
    in
    
    let sample_matrix ~f m n =
      let rec aux matrix m = 
        if m = 0 then matrix
        else aux (matrix @ [sample_list ~f n]) (m-1)
      in
      aux [] m
    in
    
    let transpose_matrix list =
      L.fold_left list
        ~init:(L.map (L.hd_exn list) ~f:(fun _ -> []))
        ~f:(fun l_output l -> L.map2_exn l_output l ~f:(fun li e -> li @ [e]))
    in
    
    let vector_times_vector ~add ~mul v1 v2 =
      let prods = L.map2_exn v1 v2 ~f:mul in
      L.fold_left (L.tl_exn prods)
        ~init:(L.hd_exn prods)
        ~f:add
    in
    
    let matrix_times_vector ~add ~mul m v = L.map m ~f:(fun row -> vector_times_vector ~add ~mul row v) in

    let matrix_times_matrix ~add ~mul m1 m2 =
      L.map (transpose_matrix m2) ~f:(fun col -> matrix_times_vector ~add ~mul m1 col)
    in
    
    let matrix_map ~f m = L.map m ~f:(L.map ~f) in

    (* Public parameters *)
    
    let g1 = R.g1_gen () in
    let g2 = R.g2_gen () in
    let p = R.g1_ord () in
    assert ((R.bn_equal p (R.g2_ord ())) && (R.bn_equal p (R.gt_ord ())));
      
    let samp_zp () = R.bn_rand_mod p in
    let zp_inverse a = 
      let (d,u,_v) = R.bn_gcd_ext a p in
      if R.bn_equal d (R.bn_one ()) then R.bn_mod u p
      else failwith ("Inverse of " ^ (R.bn_write_str a 10)  ^ " mod " ^ (R.bn_write_str p 10) ^ " does not exist")
    in

    let bn_add_mod a b = R.bn_mod (R.bn_add a b) p in
    let bn_mul_mod a b = R.bn_mod (R.bn_mul a b) p in
    let bn_neg_mod a = R.bn_mod (R.bn_neg a) p in
    let bn_read_str_mod str = R.bn_mod (R.bn_read_str str ~radix:10) p in

    (* Dual System Groups *)

    let k = 3 in  (* Security based on k-Lin assumption *)
    let dual_system_pairing l1 l2 =
      let gt_list = L.map2_exn l1 l2 ~f:R.e_pairing in
      L.fold_left (L.tl_exn gt_list) ~init:(L.hd_exn gt_list) ~f:R.gt_mul
    in

    let samp_Dk k =
      let diagonal = sample_list ~f:samp_zp k in
      let rec make_matrix matrix counter = function
        | [] -> matrix
        | a :: rest ->
           let new_row = (mk_list (R.bn_zero ()) counter) @ [a] @ (mk_list (R.bn_zero ()) (k - counter - 1)) in
           make_matrix (matrix @ [new_row]) (counter + 1) rest
      in
      (make_matrix [] 0 diagonal) @ [ mk_list (R.bn_one ()) k],
      (L.map diagonal ~f:zp_inverse) @ [bn_neg_mod (R.bn_one ())]
    in

    let sampP n =
      let a_matrix, a_orth = samp_Dk k in
      let b_matrix, b_orth = samp_Dk k in

      let list_W = sample_list ~f:(fun () -> sample_matrix ~f:samp_zp (k+1) (k+1)) n in
      let g1_A = matrix_map a_matrix ~f:(R.g1_mul g1) in
      let g2_B = matrix_map b_matrix ~f:(R.g2_mul g2) in
      let list_WA = L.map list_W ~f:(fun w -> matrix_times_matrix ~add:bn_add_mod ~mul:bn_mul_mod (transpose_matrix w) a_matrix) in
      let list_WB = L.map list_W ~f:(fun w -> matrix_times_matrix ~add:bn_add_mod ~mul:bn_mul_mod w b_matrix) in
      let g1_WA = L.map list_WA ~f:(fun wa -> matrix_map wa ~f:(R.g1_mul g1)) in
      let g2_WB = L.map list_WB ~f:(fun wb -> matrix_map wb ~f:(R.g2_mul g2)) in
      (g1_A, g1_WA, g2_B, g2_WB), (a_orth, b_orth, list_W)
    in

    let sampGT ?(randomness = None) p_list =
      let s_list =
        match randomness with
        | None        -> sample_list ~f:samp_zp k
        | Some s_list -> s_list
      in
      let l = L.map2_exn s_list p_list ~f:(fun s p -> R.gt_exp p s) in
      L.fold_left (L.tl_exn l) ~init:(L.hd_exn l) ~f:R.gt_mul
    in

    let sampG ?(randomness = None) pp =
      let (g1_A, g1_WA, _, _) = pp in
      let s_list =
        match randomness with
        | None        -> sample_list ~f:samp_zp k
        | Some s_list -> s_list
      in
      let prod_As = matrix_times_vector ~add:R.g1_add ~mul:R.g1_mul g1_A s_list in
      let prod_WAs = L.map g1_WA ~f:(fun wa -> matrix_times_vector ~add:R.g1_add ~mul:R.g1_mul g1_A s_list) in
      prod_As :: prod_WAs
    in

    let sampH pp =
      let (_, _, g2_B, g2_WB) = pp in
      let r_list = sample_list ~f:samp_zp k in
      let prod_Br = matrix_times_vector ~add:R.g2_add ~mul:R.g2_mul g2_B r_list in
      let prod_WBr = L.map g2_WB ~f:(fun wb -> matrix_times_vector ~add:R.g2_add ~mul:R.g2_mul g2_B r_list) in
      prod_Br :: prod_WBr
    in

    (* Predicate Encoding *)

    let sE x w =
      let u = L.hd_exn w in
      L.map2_exn ~f:(L.map2_exn ~f:R.g1_add)
        (L.map x ~f:(fun xi -> L.map u ~f:(fun g -> R.g1_mul g xi)))
        (L.tl_exn w)
    in

    let rE y w =
      vector_times_vector
        ~add:(L.map2_exn ~f:R.g2_add)
        ~mul:(fun l a -> L.map l ~f:(fun g -> R.g2_mul g a))
        (L.tl_exn w) y
    in

    let kE _y alpha =
      alpha
    in

    let sD _x y c =
      vector_times_vector ~add:(L.map2_exn ~f:R.g1_add) ~mul:(fun c y -> L.map c ~f:(fun g -> R.g1_mul g y)) c y
    in

    let rD _x _y d =
      d
    in

    (* ABE *)

    let setup n =
      let pp, sp = sampP n in
      let (g1_A, _, _, _) = pp in
      let msk = sample_list ~f:R.g2_rand (k+1) in
      let mu_msk = matrix_times_vector ~add:R.gt_mul ~mul:R.e_pairing (transpose_matrix g1_A) msk in
      (pp, mu_msk), msk
    in

    let enc mpk x m =
      let (pp, mu_msk) = mpk in
      let s_list = sample_list ~f:samp_zp k in
      let g_list = sampG ~randomness:(Some s_list) pp in
      let g'T = sampGT ~randomness:(Some s_list) mu_msk in
      let c0 = L.hd_exn g_list in
      let c1 = sE x (L.tl_exn g_list) in
      let c' = R.gt_mul g'T m in
      (c0, c1, c'), x
    in

    let keyGen mpk msk y =
      let (pp, mu_msk) = mpk in
      let h_list = sampH pp in
      let k0 = L.hd_exn h_list in
      let k1 = L.map2_exn (kE y msk) (rE y (L.tl_exn h_list)) ~f:R.g2_add in
      (k0, k1), y
    in

    let dec mpk sk_y ct_x =
      let (c0, c1, c'), x = ct_x in
      let (k0, k1), y = sk_y in
      let e_g0_msk = R.gt_mul (dual_system_pairing c0 (rD x y k1)) (R.gt_inv (dual_system_pairing (sD x y c1) k0)) in
      R.gt_mul c' (R.gt_inv e_g0_msk)        
    in

    let mpk, msk = setup 4 in
    let x = [ bn_read_str_mod "5"; bn_read_str_mod "4"; bn_read_str_mod "-3" ] in
    let m = R.gt_rand () in

    let ct_x = enc mpk x m in

    let y = [ bn_read_str_mod "2"; bn_read_str_mod "5"; bn_read_str_mod "10" ] in
    let sk_y = keyGen mpk msk y in
    let m' = dec mpk sk_y ct_x in

    let y' = [ bn_read_str_mod "2"; bn_read_str_mod "5"; bn_read_str_mod "11" ] in
    let sk_y' = keyGen mpk msk y' in
    let m'' = dec mpk sk_y' ct_x in

    if R.gt_equal m m' && not (R.gt_equal m m'') then F.printf "ABE test succedded!\n"
    else assert false

let _ =
  let suite = "relic" >::: [
    (* test_functions; *)
    test_pairing;
    test_sps;
    test_abe;
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()

