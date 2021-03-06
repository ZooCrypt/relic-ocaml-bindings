(* * Bindings for Relic *)

open Ctypes

(* ** Types module *)

module Types (F: Cstubs.Types.TYPE) = struct
  open F
  let sts_ok  = constant "RLC_OK" int
  let sts_err = constant "RLC_ERR" int

  let cmp_lt  = constant "RLC_LT" int
  let cmp_eq  = constant "RLC_EQ" int
  let cmp_gt  = constant "RLC_GT" int
  let cmp_ne  = constant "RLC_NE" int

  let bn_positive = constant "RLC_POS" int
  let bn_negative = constant "RLC_NEG" int

  let fp_prime = constant "FP_PRIME" int

  let secg_p160    = constant "SECG_P160" int
  let secg_k160    = constant "SECG_K160" int
  let nist_p192    = constant "NIST_P192" int
  let secg_k192    = constant "SECG_K192" int
  let nist_p224    = constant "NIST_P224" int
  let secg_k224    = constant "SECG_K224" int
  let nist_p256    = constant "NIST_P256" int
  let bsi_p256     = constant "BSI_P256" int
  let secg_k256    = constant "SECG_K256" int
  let nist_p384    = constant "NIST_P384" int
  let nist_p521    = constant "NIST_P521" int
  let bn_p158      = constant "BN_P158" int
  let bn_p254      = constant "BN_P254" int
  let bn_p256      = constant "BN_P256" int
  let b24_p477     = constant "B24_P477" int
  let kss_p508     = constant "KSS_P508" int
  let bn_p638      = constant "BN_P638" int
  let b12_p638     = constant "B12_P638" int
  let ss_p1536     = constant "SS_P1536" int
  let curve_1174   = constant "CURVE_1174" int
  let curve_25519  = constant "CURVE_25519" int
  let curve_383187 = constant "CURVE_383187" int
end

module Bindings (F : Cstubs.FOREIGN) = struct
  open F

(* *** Typedefs *)

  module Typedef_ptr (TN : sig val type_name : string end) : sig
    type t
    val t : t Ctypes.typ
    val allocate : unit -> t ptr
    val to_string : t -> string
  end = struct
    type t = unit ptr
    let t = typedef (ptr void) TN.type_name

    let allocate () = allocate t null
    let to_string p =
      Nativeint.to_string @@ raw_address_of_ptr p
  end

(* *** Initialization *)

  let core_init = foreign "core_init" (void @-> returning int)
  let pc_param_set_any = foreign "pc_param_set_any" (void @-> returning int)
  let ec_param_set_any = foreign "ec_param_set_any" (void @-> returning int)

(* *** Big numbers *)

  module Bn = Typedef_ptr(struct let type_name = "bn_t" end)

  let bn = Bn.t

  let bn_new  = foreign "w_bn_new" (ptr bn @-> returning void)
  let bn_free = foreign "bn_free"  (bn @-> returning void)

  let bn_set_dig = foreign "bn_set_dig"  (bn @-> uint64_t @-> returning void)
  let bn_set_2b  = foreign "bn_set_2b"  (bn @-> int @-> returning void)

  let bn_add     = foreign "bn_add"      (bn @-> bn @-> bn @-> returning void)
  let bn_sub     = foreign "bn_sub"      (bn @-> bn @-> bn @-> returning void)
  let bn_mul     = foreign "bn_mul"      (bn @-> bn @-> bn @-> returning void)
  let bn_div     = foreign "bn_div"      (bn @-> bn @-> bn @-> returning void)
  let bn_neg     = foreign "bn_neg"      (bn @-> bn @-> returning void)
  let bn_abs     = foreign "bn_abs"      (bn @-> bn @-> returning void)
  let bn_sqrt    = foreign "bn_srt"      (bn @-> bn @-> returning void)
  let bn_mod     = foreign "bn_mod"      (bn @-> bn @-> bn @-> returning void)
  let bn_gcd     = foreign "bn_gcd"      (bn @-> bn @-> bn @-> returning void)
  let bn_gcd_ext = foreign "bn_gcd_ext"  (bn @-> bn @-> bn @-> bn @-> bn @-> returning void)
  let bn_lcm     = foreign "bn_lcm"      (bn @-> bn @-> bn @-> returning void)
  let bn_zero    = foreign "bn_zero"     (bn @-> returning void)

  let bn_mxp_basic = foreign "bn_mxp_basic"  (bn @-> bn @-> bn @-> bn @-> returning void)


  let bn_is_zero  = foreign "bn_is_zero"  (bn @-> returning bool)
  let bn_cmp      = foreign "bn_cmp"      (bn @-> bn @-> returning int)
  let bn_is_prime = foreign "bn_is_prime_solov" (bn @-> returning bool)

  let bn_rand      = foreign "bn_rand"      (bn @-> int @-> int @-> returning void)
  let bn_rand_mod  = foreign "bn_rand_mod"  (bn @-> bn @-> returning void)
  let bn_gen_prime = foreign "bn_gen_prime" (bn @-> int @-> returning void)

  let bn_size_str  = foreign "bn_size_str"  (bn @-> int @-> returning int)
  let bn_ham       = foreign "bn_ham"       (bn @-> returning int)
  let bn_write_str = foreign "bn_write_str" (ptr char @-> int @-> bn @-> int @-> returning void)
  let bn_read_str  = foreign "bn_read_str"  (bn @-> ptr char @-> int @-> int @-> returning void)

(* *** Elliptic Curves *)

  module EC = Typedef_ptr(struct let type_name = "ec_t" end)

  let ec = EC.t

  let ec_param_get = foreign "ec_param_get" (void @-> returning int)

  let ec_new  = foreign "w_ec_new" (ptr ec @-> returning void)
  let ec_free = foreign "ec_free"  (ec @-> returning void)

  let ec_get_gen     = foreign "ec_curve_get_gen" (ec @-> returning void) (* Note the word 'curve' in *)
  let ec_get_ord     = foreign "ec_curve_get_ord" (bn @-> returning void) (* the relic function name *)
  let ec_is_infty    = foreign "ec_is_infty"      (ec @-> returning bool)
  let ec_set_infty   = foreign "ec_set_infty"     (ec @-> returning void)
  let ec_cmp         = foreign "ec_cmp"           (ec @-> ec @-> returning int)
  let ec_rand        = foreign "ec_rand"          (ec @-> returning void)
  let ec_is_valid    = foreign "ec_on_curve"      (ec @-> returning bool)
  let ec_size_bin    = foreign "ec_size_bin"      (ec @-> int @-> returning int)
  let ec_read_bin    = foreign "ec_read_bin"      (ec @-> ptr uint8_t @-> int @-> returning void)
  let ec_write_bin   = foreign "ec_write_bin"     (ptr uint8_t @-> int @-> ec @-> int @-> returning void)
  let ec_neg         = foreign "ec_neg"           (ec @-> ec @-> returning void)
  let ec_add         = foreign "ec_add"           (ec @-> ec @-> ec @-> returning void)
  let ec_sub         = foreign "ec_sub"           (ec @-> ec @-> ec @-> returning void)
  let ec_mul         = foreign "ec_mul"           (ec @-> ec @-> bn @-> returning void)
  let ec_norm        = foreign "ec_norm"          (ec @-> ec @-> returning void)
  let ec_mul_gen     = foreign "ec_mul_gen"       (ec @-> bn @-> returning void)
  let ec_mul_sim     = foreign "ec_mul_sim"       (ec @-> ec @-> bn @-> ec @-> bn @-> returning void)
  let ec_mul_gen_sim = foreign "ec_mul_sim_gen"   (ec @-> bn @-> ec @-> bn @-> returning void)


(* *** Pairing Groups *)

  let pc_param_level  = foreign "pc_param_level"  (void @-> returning int)
  let pc_map_is_type1 = foreign "pc_map_is_type1" (void @-> returning bool)
  let pc_map_is_type3 = foreign "pc_map_is_type3" (void @-> returning bool)
  let pc_param_get    = foreign "ep_param_get"    (void @-> returning int) (* It is "ep" and not "pc" *)

(* **** G1 *)

  module G1 = Typedef_ptr(struct let type_name = "g1_t" end)

  let g1 = G1.t

  let g1_new  = foreign "w_g1_new" (ptr g1 @-> returning void)
  let g1_free = foreign "g1_free"  (g1 @-> returning void)

  let g1_get_gen   = foreign "g1_get_gen"   (g1 @-> returning void)
  let g1_get_ord   = foreign "g1_get_ord"   (bn @-> returning void)
  let g1_is_infty  = foreign "g1_is_infty"  (g1 @-> returning bool)
  let g1_set_infty = foreign "g1_set_infty" (g1 @-> returning void)
  let g1_cmp       = foreign "g1_cmp"       (g1 @-> g1 @-> returning int)
  let g1_rand      = foreign "g1_rand"      (g1 @-> returning void)
  let g1_is_valid  = foreign "g1_is_valid"  (g1 @-> returning bool)
  let g1_size_bin  = foreign "g1_size_bin"  (g1 @-> int @-> returning int)
  let g1_read_bin  = foreign "g1_read_bin"  (g1 @-> ptr uint8_t @-> int @-> returning void)
  let g1_write_bin = foreign "g1_write_bin" (ptr uint8_t @-> int @-> g1 @-> int @-> returning void)
  let g1_neg       = foreign "g1_neg"       (g1 @-> g1 @-> returning void)
  let g1_add       = foreign "g1_add"       (g1 @-> g1 @-> g1 @-> returning void)
  let g1_sub       = foreign "g1_sub"       (g1 @-> g1 @-> g1 @-> returning void)
  let g1_mul       = foreign "g1_mul"       (g1 @-> g1 @-> bn @-> returning void)
  let g1_norm      = foreign "g1_norm"      (g1 @-> g1 @-> returning void)
  let g1_mul_gen   = foreign "g1_mul_gen"   (g1 @-> bn @-> returning void)

(* **** G2 *)

  module G2 = Typedef_ptr(struct let type_name = "g2_t" end)

  let g2 = G2.t

  let g2_new  = foreign "w_g2_new" (ptr g2 @-> returning void)
  let g2_free = foreign "g2_free"  (g2 @-> returning void)

  let g2_get_gen   = foreign "g2_get_gen"   (g2 @-> returning void)
  let g2_get_ord   = foreign "g2_get_ord"   (bn @-> returning void)
  let g2_is_infty  = foreign "g2_is_infty"  (g2 @-> returning bool)
  let g2_set_infty = foreign "g2_set_infty" (g2 @-> returning void)
  let g2_cmp       = foreign "g2_cmp"       (g2 @-> g2 @-> returning int)
  let g2_rand      = foreign "g2_rand"      (g2 @-> returning void)
  let g2_is_valid  = foreign "g2_is_valid"  (g2 @-> returning bool)
  let g2_size_bin  = foreign "g2_size_bin"  (g2 @-> int @-> returning int)
  let g2_read_bin  = foreign "g2_read_bin"  (g2 @-> ptr uint8_t @-> int @-> returning void)
  let g2_write_bin = foreign "g2_write_bin" (ptr uint8_t @-> int @-> g2 @-> int @-> returning void)
  let g2_neg       = foreign "g2_neg"       (g2 @-> g2 @-> returning void)
  let g2_add       = foreign "g2_add"       (g2 @-> g2 @-> g2 @-> returning void)
  let g2_sub       = foreign "g2_sub"       (g2 @-> g2 @-> g2 @-> returning void)
  let g2_mul       = foreign "g2_mul"       (g2 @-> g2 @-> bn @-> returning void)
  let g2_norm      = foreign "g2_norm"      (g2 @-> g2 @-> returning void)
  let g2_mul_gen   = foreign "g2_mul_gen"   (g2 @-> bn @-> returning void)

(* **** GT *)

  module Gt = Typedef_ptr(struct let type_name = "my_gt_t" end)

  let gt = Gt.t

  let gt_new  = foreign "w_gt_new" (ptr gt @-> returning void)
  let gt_free = foreign "w_gt_free"  (gt @-> returning void)

  let gt_get_gen   = foreign "w_gt_get_gen"   (gt @-> returning void)
  let gt_get_ord   = foreign "gt_get_ord"     (bn @-> returning void)
  let gt_is_unity  = foreign "w_gt_is_unity"  (gt @-> returning bool)
  let gt_zero      = foreign "w_gt_zero"      (gt @-> returning void)
  let gt_set_unity = foreign "w_gt_set_unity" (gt @-> returning void)
  let gt_cmp       = foreign "w_gt_cmp"       (gt @-> gt @-> returning int)
  let gt_rand      = foreign "w_gt_rand"      (gt @-> returning void)
  let gt_size_bin  = foreign "w_gt_size_bin"  (gt @-> int @-> returning int)
  let gt_read_bin  = foreign "w_gt_read_bin"  (gt @-> ptr uint8_t @-> int @-> returning void)
  let gt_write_bin = foreign "w_gt_write_bin" (ptr uint8_t @-> int @-> gt @-> int @-> returning void)
  let gt_inv       = foreign "w_gt_inv"       (gt @-> gt @-> returning void)
  let gt_mul       = foreign "w_gt_mul"       (gt @-> gt @-> gt @-> returning void)
  let gt_exp       = foreign "w_gt_exp"       (gt @-> gt @-> bn @-> returning void)

(* **** Bilinear map *)

  let pc_map = foreign "w_pc_map"  (gt @-> g1 @-> g2 @-> returning void)

end
