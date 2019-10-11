open Ctypes

module R  = Ffi_bindings.Bindings(Ffi_generated)
module RT = Ffi_bindings.Types(Ffi_generated_types)

let sts_ok = RT.sts_ok
let sts_err = RT.sts_err

let cmp_lt = RT.cmp_lt
let cmp_eq = RT.cmp_eq
let cmp_gt = RT.cmp_gt
let cmp_ne = RT.cmp_ne

let bn_positive = RT.bn_positive
let bn_negative = RT.bn_negative

let fp_bytes = RT.fp_bytes

let secg_p160    = RT.secg_p160
let secg_k160    = RT.secg_k160
let nist_p192    = RT.nist_p192
let secg_k192    = RT.secg_k192
let nist_p224    = RT.nist_p224
let secg_k224    = RT.secg_k224
let nist_p256    = RT.nist_p256
let bsi_p256     = RT.bsi_p256
let secg_k256    = RT.secg_k256
let nist_p384    = RT.nist_p384
let nist_p521    = RT.nist_p521
let bn_p158      = RT.bn_p158
let bn_p254      = RT.bn_p254
let bn_p256      = RT.bn_p256
let b24_p477     = RT.b24_p477
let kss_p508     = RT.kss_p508
let bn_p638      = RT.bn_p638
let b12_p638     = RT.b12_p638
let ss_p1536     = RT.ss_p1536
let curve_1174   = RT.curve_1174
let curve_25519  = RT.curve_25519
let curve_383187 = RT.curve_383187

let core_init = R.core_init
let pc_param_set_any = R.pc_param_set_any
let ec_param_set_any = R.ec_param_set_any

type bn = R.Bn.t
type ec = R.EC.t
type g1 = R.G1.t
type g2 = R.G2.t
type gt = R.Gt.t

module Internal = struct
  let bn_new  = R.bn_new
  let bn_free = R.bn_free

  let bn_set_dig = R.bn_set_dig
  let bn_set_2b  = R.bn_set_2b

  let bn_add     = R.bn_add
  let bn_sub     = R.bn_sub
  let bn_mul     = R.bn_mul
  let bn_div     = R.bn_div
  let bn_neg     = R.bn_neg
  let bn_abs     = R.bn_abs
  let bn_sqrt    = R.bn_sqrt
  let bn_mod     = R.bn_mod
  let bn_gcd     = R.bn_gcd
  let bn_gcd_ext = R.bn_gcd_ext
  let bn_lcm     = R.bn_lcm
  let bn_zero    = R.bn_zero
  let bn_mxp_basic = R.bn_mxp_basic

  let bn_is_zero  = R.bn_is_zero
  let bn_cmp      = R.bn_cmp
  let bn_is_prime = R.bn_is_prime

  let bn_rand      = R.bn_rand
  let bn_rand_mod  = R.bn_rand_mod
  let bn_gen_prime = R.bn_gen_prime

  let bn_size_str  = R.bn_size_str
  let bn_ham       = R.bn_ham
  let bn_write_str = R.bn_write_str
  let bn_read_str  = R.bn_read_str

  let ec_param_get   = R.ec_param_get

  let ec_new         = R.ec_new
  let ec_free        = R.ec_free
  let ec_get_gen     = R.ec_get_gen
  let ec_get_ord     = R.ec_get_ord
  let ec_is_infty    = R.ec_is_infty
  let ec_set_infty   = R.ec_set_infty
  let ec_cmp         = R.ec_cmp
  let ec_rand        = R.ec_rand
  let ec_is_valid    = R.ec_is_valid
  let ec_size_bin    = R.ec_size_bin
  let ec_read_bin    = R.ec_read_bin
  let ec_write_bin   = R.ec_write_bin
  let ec_neg         = R.ec_neg
  let ec_add         = R.ec_add
  let ec_sub         = R.ec_sub
  let ec_mul         = R.ec_mul
  let ec_norm        = R.ec_norm
  let ec_mul_gen     = R.ec_mul_gen
  let ec_mul_sim     = R.ec_mul_sim
  let ec_mul_gen_sim = R.ec_mul_gen_sim

  let pc_param_level  = R.pc_param_level
  let pc_map_is_type1 = R.pc_map_is_type1
  let pc_map_is_type3 = R.pc_map_is_type3
  let pc_param_get    = R.pc_param_get

  let g1_new       = R.g1_new
  let g1_free      = R.g1_free
  let g1_get_gen   = R.g1_get_gen
  let g1_get_ord   = R.g1_get_ord
  let g1_is_infty  = R.g1_is_infty
  let g1_set_infty = R.g1_set_infty
  let g1_cmp       = R.g1_cmp
  let g1_rand      = R.g1_rand
  let g1_is_valid  = R.g1_is_valid
  let g1_size_bin  = R.g1_size_bin
  let g1_read_bin  = R.g1_read_bin
  let g1_write_bin = R.g1_write_bin
  let g1_neg       = R.g1_neg
  let g1_add       = R.g1_add
  let g1_sub       = R.g1_sub
  let g1_mul       = R.g1_mul
  let g1_norm      = R.g1_norm
  let g1_mul_gen   = R.g1_mul_gen

  let g2_new       = R.g2_new
  let g2_free      = R.g2_free
  let g2_get_gen   = R.g2_get_gen
  let g2_get_ord   = R.g2_get_ord
  let g2_is_infty  = R.g2_is_infty
  let g2_set_infty = R.g2_set_infty
  let g2_cmp       = R.g2_cmp
  let g2_rand      = R.g2_rand
  let g2_is_valid  = R.g2_is_valid
  let g2_size_bin  = R.g2_size_bin
  let g2_read_bin  = R.g2_read_bin
  let g2_write_bin = R.g2_write_bin
  let g2_neg       = R.g2_neg
  let g2_add       = R.g2_add
  let g2_sub       = R.g2_sub
  let g2_mul       = R.g2_mul
  let g2_norm      = R.g2_norm
  let g2_mul_gen   = R.g2_mul_gen

  let gt_new       = R.gt_new
  let gt_free      = R.gt_free
  let gt_get_gen   = R.gt_get_gen
  let gt_get_ord   = R.gt_get_ord
  let gt_is_unity  = R.gt_is_unity
  let gt_zero      = R.gt_zero
  let gt_set_unity = R.gt_set_unity
  let gt_cmp       = R.gt_cmp
  let gt_rand      = R.gt_rand
  let gt_size_bin  = R.gt_size_bin
  let gt_read_bin  = R.gt_read_bin
  let gt_write_bin = R.gt_write_bin
  let gt_inv       = R.gt_inv
  let gt_mul       = R.gt_mul
  let gt_exp       = R.gt_exp

  let pc_map = R.pc_map
end

(* ** Big numbers *)

let allocate_bn () =
  let bn_p = R.Bn.allocate () in (* allocate bn_t which is a pointer to a structure *)
  Internal.bn_new bn_p;          (* allocate bn_st, the actual struct with the values *)
  let bn = !@bn_p in
  Gc.finalise Internal.bn_free bn;
  bn

let bn_from_uint64 n =
  let bn = allocate_bn () in
  Internal.bn_set_dig bn n;
  bn

let bn_2powern n =
  let bn = allocate_bn () in
  Internal.bn_set_2b bn n;
  bn

let bn_opp f =
  (fun n n' ->
      let bn = allocate_bn () in
      f bn n n';
      bn
  )

let bn_add = bn_opp Internal.bn_add

let bn_sub = bn_opp Internal.bn_sub

let bn_mul = bn_opp Internal.bn_mul

let bn_div = bn_opp Internal.bn_div

let bn_neg n =
  let bn = allocate_bn () in
  Internal.bn_neg bn n;
  bn

let bn_abs n =
  let bn = allocate_bn () in
  Internal.bn_abs bn n;
  bn

let bn_sqrt n =
  let bn = allocate_bn () in
  Internal.bn_sqrt bn n;
  bn

let bn_mod a m =
  if Internal.bn_is_zero m then failwith "Division by zero"
  else
    let bn = allocate_bn () in
    Internal.bn_mod bn a m;
    bn

let bn_gcd a b =
  let bn = allocate_bn () in
  Internal.bn_gcd bn a b;
  bn

let bn_gcd_ext a b =
  let d = allocate_bn () in
  let u = allocate_bn () in
  let v = allocate_bn () in
  Internal.bn_gcd_ext d u v a b;
  (d, u, v)

let bn_lcm a b =
  let bn = allocate_bn () in
  Internal.bn_lcm bn a b;
  bn

let bn_pow_mod a b m =
  let bn = allocate_bn () in
  Internal.bn_mxp_basic bn a b m;
  bn

let bn_zero () =
  let bn = allocate_bn () in
  Internal.bn_zero bn;
  bn

let bn_one () =
  let bn = allocate_bn () in
  Internal.bn_set_2b bn 0;
  bn

let bn_is_zero n =
  Internal.bn_is_zero n

let bn_cmp n n' =
  Internal.bn_cmp n n'

let bn_equal n n' =
  if (Internal.bn_cmp n n') = cmp_eq then true
  else false

let bn_is_prime n =
  let two = bn_add (bn_one ()) (bn_one ()) in
  if bn_cmp n two = cmp_lt then failwith "bn_is_prime: input must be greater or equal than 2"
  else if bn_cmp n two = cmp_eq then true
  else Internal.bn_is_prime n

let bn_rand ?(positive=true) ~bits =
  let bn = allocate_bn () in
  let sign = if positive then bn_positive else bn_negative in
  Internal.bn_rand bn sign bits;
  bn

let bn_rand_mod m =
  let bn = allocate_bn () in
  Internal.bn_rand_mod bn m;
  bn

let bn_gen_prime ~bits =
  let bn = allocate_bn () in
  Internal.bn_gen_prime bn bits;
  bn

let bn_size_str bn ~radix =
  Internal.bn_size_str bn radix

let bn_ham bn =
  Internal.bn_ham bn

let bn_write_str bn ~radix =
  let n_chars = bn_size_str bn radix in
  let buf = Ctypes.allocate_n char ~count:n_chars in
  let _ = Internal.bn_write_str buf n_chars bn radix in
  Ctypes.coerce (ptr char) string buf

let bn_read_str str ~radix =
  let bn = allocate_bn () in
  let length = String.length str in
  let buf = Ctypes.allocate_n char ~count:length in
  for i = 0 to length-1 do
    buf +@ i <-@ str.[i];
    ()
  done;
  Internal.bn_read_str bn buf length radix;
  bn

(* ** Elliptic Curves *)

let compress_flag compress = if compress then 1 else 0

let curve_of_param param =
  if param = secg_p160         then "SECG_P160"
  else if param = secg_k160    then "SECG_K160"
  else if param = nist_p192    then "NIST_P192"
  else if param = secg_k192    then "SECG_K192"
  else if param = nist_p224    then "NIST_P224"
  else if param = secg_k224    then "SECG_K224"
  else if param = nist_p256    then "NIST_P256"
  else if param = bsi_p256     then "BSI_P256"
  else if param = secg_k256    then "SECG_K256"
  else if param = nist_p384    then "NIST_P384"
  else if param = nist_p521    then "NIST_P521"
  else if param = bn_p158      then "BN_P158"
  else if param = bn_p254      then "BN_P254"
  else if param = bn_p256      then "BN_P256"
  else if param = b24_p477     then "B24_P477"
  else if param = kss_p508     then "KSS_P508"
  else if param = bn_p638      then "BN_P638"
  else if param = b12_p638     then "B12_P638"
  else if param = ss_p1536     then "SS_P1536"
  else if param = curve_1174   then "CURVE_1174"
  else if param = curve_25519  then "CURVE_25519"
  else if param = curve_383187 then "CURVE_383187"
  else "unknown curve"

let ec_param_get () =
  Internal.ec_param_get () |> curve_of_param

let allocate_ec () =
  let ec_p = R.EC.allocate () in
  Internal.ec_new ec_p;
  let ec = !@ec_p in
  Gc.finalise Internal.ec_free ec;
  ec

let ec_gen () =
  let ec = allocate_ec () in
  Internal.ec_get_gen ec;
  ec

let ec_ord () =
  let bn = allocate_bn () in
  Internal.ec_get_ord bn;
  bn

let ec_is_infty ec =
  Internal.ec_is_infty ec

let ec_infty () =
  let ec = allocate_ec () in
  Internal.ec_set_infty ec;
  ec

let ec_equal ec ec' =
  if (Internal.ec_cmp ec ec') = cmp_eq then true
  else false

let ec_rand () =
  let ec = allocate_ec () in
  Internal.ec_rand ec;
  ec

let ec_is_valid ec =
  Internal.ec_is_valid ec

let ec_size_bin ?(compress=false) ec =
  let flag = compress_flag compress in
  Internal.ec_size_bin ec flag

let ec_read_bin str =
  let _checking =
    match String.length str with
    | 1 ->
       if str.[0] != '\000' then
         failwith "Invalid string: first byte expected to be \000"
       else ()
    | a when a = (fp_bytes + 1) ->
       if str.[0] != '\002' && str.[0] != '\003' then
         failwith "Invalid string: first byte expected to be either \002 or \003"
       else ()
    | a when a = (2 * fp_bytes + 1) ->
       if str.[0] != '\004' then
         failwith "Invalid string: first byte expected to be \004"
       else ()
    | a -> failwith ("Invalid string: " ^ (string_of_int a) ^ " is not one of the accepted lengths:" ^
       " 1, " ^ (string_of_int (fp_bytes + 1)) ^ ", " ^ (string_of_int (2*fp_bytes + 1)))
  in
  let ec = allocate_ec () in
  let length = String.length str in
  let buf = Ctypes.allocate_n char ~count:length in
  for i = 0 to length-1 do
    buf +@ i <-@ str.[i];
    ()
  done;
  let buf = from_voidp uint8_t (to_voidp buf) in
  Internal.ec_read_bin ec buf length;
  ec

let ec_write_bin ?(compress=false) ec =
  let flag = compress_flag compress in
  let n_chars = Internal.ec_size_bin ec flag in
  let buf = Ctypes.allocate_n char ~count:n_chars in
  let buf = from_voidp uint8_t (to_voidp buf) in
  let _ = Internal.ec_write_bin buf n_chars ec flag in
  let buf = from_voidp char (to_voidp buf) in
  Ctypes.string_from_ptr buf ~length:n_chars

let ec_neg ec =
  let res = allocate_ec () in
  Internal.ec_neg res ec;
  res

let ec_add ec ec' =
  let res = allocate_ec () in
  Internal.ec_add res ec ec';
  res

let ec_sub ec ec' =
  let res = allocate_ec () in
  Internal.ec_sub res ec ec';
  res

let ec_mul ec k =
  let res = allocate_ec () in
  Internal.ec_mul res ec k;
  res

let ec_norm ec =
  let res = allocate_ec () in
  Internal.ec_norm res ec;
  res

let ec_mul_gen k =
  let res = allocate_ec () in
  Internal.ec_mul_gen res k;
  res

let ec_mul_sim (ec,k) (ec',k') =
  let res = allocate_ec () in
  Internal.ec_mul_sim res ec k ec' k';
  res

let ec_mul_gen_sim k (ec',k') =
  let res = allocate_ec () in
  Internal.ec_mul_gen_sim res k ec' k';
  res


(* ** Pairing Groups *)

let pc_param_level () = Internal.pc_param_level ()

let pc_map_type () =
  if Internal.pc_map_is_type1 ()      then 1
  else if Internal.pc_map_is_type3 () then 3
  else failwith "Unknown type"

let pc_param_get () =
  Internal.pc_param_get () |> curve_of_param

(* *** G1 *)

let allocate_g1 () =
  let g1_p = R.G1.allocate () in
  Internal.g1_new g1_p;
  let g1 = !@g1_p in
  Gc.finalise Internal.g1_free g1;
  g1

let g1_gen () =
  let g1 = allocate_g1 () in
  Internal.g1_get_gen g1;
  g1

let g1_ord () =
  let bn = allocate_bn () in
  Internal.g1_get_ord bn;
  bn

let g1_is_infty g1 =
  Internal.g1_is_infty g1

let g1_infty () =
  let g1 = allocate_g1 () in
  Internal.g1_set_infty g1;
  g1

let g1_equal g1 g1' =
  if (Internal.g1_cmp g1 g1') = cmp_eq then true
  else false

let g1_rand () =
  let g1 = allocate_g1 () in
  Internal.g1_rand g1;
  g1

let g1_is_valid g1 =
  Internal.g1_is_valid g1

let g1_size_bin ?(compress=false) g1 =
  let flag = compress_flag compress in
  Internal.g1_size_bin g1 flag

let g1_read_bin str =
  let _checking =
    match String.length str with
    | 1 ->
       if str.[0] != '\000' then
         failwith "Invalid string: first byte expected to be \000"
       else ()
    | a when a = (fp_bytes + 1) ->
       if str.[0] != '\002' && str.[0] != '\003' then
         failwith "Invalid string: first byte expected to be either \002 or \003"
       else ()
    | a when a = (2 * fp_bytes + 1) ->
       if str.[0] != '\004' then
         failwith "Invalid string: first byte expected to be \004"
       else ()
    | a -> failwith ("Invalid string: " ^ (string_of_int a) ^ " is not one of the accepted lengths:" ^
       " 1, " ^ (string_of_int (fp_bytes + 1)) ^ ", " ^ (string_of_int (2*fp_bytes + 1)))
  in
  let g1 = allocate_g1 () in
  let length = String.length str in
  let buf = Ctypes.allocate_n char ~count:length in
  for i = 0 to length-1 do
    buf +@ i <-@ str.[i];
    ()
  done;
  let buf = from_voidp uint8_t (to_voidp buf) in
  Internal.g1_read_bin g1 buf length;
  g1

let g1_write_bin ?(compress=false) g1 =
  let flag = compress_flag compress in
  let n_chars = Internal.g1_size_bin g1 flag in
  let buf = Ctypes.allocate_n char ~count:n_chars in
  let buf = from_voidp uint8_t (to_voidp buf) in
  let _ = Internal.g1_write_bin buf n_chars g1 flag in
  let buf = from_voidp char (to_voidp buf) in
  Ctypes.string_from_ptr buf ~length:n_chars

let g1_neg g1 =
  let res = allocate_g1 () in
  Internal.g1_neg res g1;
  res

let g1_add g1 g1' =
  let res = allocate_g1 () in
  Internal.g1_add res g1 g1';
  res

let g1_sub g1 g1' =
  let res = allocate_g1 () in
  Internal.g1_sub res g1 g1';
  res

let g1_mul g1 k =
  let res = allocate_g1 () in
  Internal.g1_mul res g1 k;
  res

let g1_norm g1 =
  let res = allocate_g1 () in
  Internal.g1_norm res g1;
  res

let g1_mul_gen k =
  let res = allocate_g1 () in
  Internal.g1_mul_gen res k;
  res


(* *** G2 *)

let allocate_g2 () =
  let g2_p = R.G2.allocate () in
  Internal.g2_new g2_p;
  let g2 = !@g2_p in
  Gc.finalise Internal.g2_free g2;
  g2

let g2_gen () =
  let g2 = allocate_g2 () in
  Internal.g2_get_gen g2;
  g2

let g2_ord () =
  let bn = allocate_bn () in
  Internal.g2_get_ord bn;
  bn

let g2_is_infty g2 =
  Internal.g2_is_infty g2

let g2_infty () =
  let g2 = allocate_g2 () in
  Internal.g2_set_infty g2;
  g2

let g2_equal g2 g2' =
  if (Internal.g2_cmp g2 g2') = cmp_eq then true
  else false

let g2_rand () =
  let g2 = allocate_g2 () in
  Internal.g2_rand g2;
  g2

let g2_is_valid g2 =
  Internal.g2_is_valid g2

let g2_size_bin ?(compress=false) g2 =
  let flag = compress_flag compress in
  Internal.g2_size_bin g2 flag

let g2_read_bin str =
  let g2 = allocate_g2 () in
  let length = String.length str in
  let buf = Ctypes.allocate_n char ~count:length in
  for i = 0 to length-1 do
    buf +@ i <-@ str.[i];
    ()
  done;
  let buf = from_voidp uint8_t (to_voidp buf) in
  Internal.g2_read_bin g2 buf length;
  g2

let g2_write_bin ?(compress=false) g2 =
  let flag = compress_flag compress in
  let n_chars = Internal.g2_size_bin g2 flag in
  let buf = Ctypes.allocate_n char ~count:n_chars in
  let buf = from_voidp uint8_t (to_voidp buf) in
  let _ = Internal.g2_write_bin buf n_chars g2 flag in
  let buf = from_voidp char (to_voidp buf) in
  Ctypes.string_from_ptr buf ~length:n_chars

let g2_neg g2 =
  let res = allocate_g2 () in
  Internal.g2_neg res g2;
  res

let g2_add g2 g2' =
  let res = allocate_g2 () in
  Internal.g2_add res g2 g2';
  res

let g2_sub g2 g2' =
  let res = allocate_g2 () in
  Internal.g2_sub res g2 g2';
  res

let g2_mul g2 k =
  let res = allocate_g2 () in
  Internal.g2_mul res g2 k;
  res

let g2_norm g2 =
  let res = allocate_g2 () in
  Internal.g2_norm res g2;
  res

let g2_mul_gen k =
  let res = allocate_g2 () in
  Internal.g2_mul_gen res k;
  res


(* *** Gt *)

let allocate_gt () =
  let gt_p = R.Gt.allocate () in
  Internal.gt_new gt_p;
  let gt = !@gt_p in
  Gc.finalise Internal.gt_free gt;
  gt

let gt_gen () =
  let gt = allocate_gt () in
  Internal.gt_get_gen gt;
  gt

let gt_ord () =
  let bn = allocate_bn () in
  Internal.gt_get_ord bn;
  bn

let gt_is_unity gt =
  Internal.gt_is_unity gt

let gt_zero () =
  let gt = allocate_gt () in
  Internal.gt_zero gt;
  gt

let gt_unity () =
  let gt = allocate_gt () in
  Internal.gt_set_unity gt;
  gt

let gt_equal gt gt' =
  if (Internal.gt_cmp gt gt') = cmp_eq then true
  else false

let gt_rand () =
  let gt = allocate_gt () in
  Internal.gt_rand gt;
  gt

let gt_size_bin ?(compress=false) gt =
  let flag = compress_flag compress in
  Internal.gt_size_bin gt flag

let gt_read_bin str =
  let gt = allocate_gt () in
  let length = String.length str in
  let buf = Ctypes.allocate_n char ~count:length in
  for i = 0 to length-1 do
    buf +@ i <-@ str.[i];
    ()
  done;
  let buf = from_voidp uint8_t (to_voidp buf) in
  Internal.gt_read_bin gt buf length;
  gt

let gt_write_bin ?(compress=false) gt =
  let flag = compress_flag compress in
  let n_chars = Internal.gt_size_bin gt flag in
  let buf = Ctypes.allocate_n char ~count:n_chars in
  let buf = from_voidp uint8_t (to_voidp buf) in
  let _ = Internal.gt_write_bin buf n_chars gt flag in
  let buf = from_voidp char (to_voidp buf) in
  Ctypes.string_from_ptr buf ~length:n_chars

let gt_inv gt =
  let res = allocate_gt () in
  Internal.gt_inv res gt;
  res

let gt_mul gt gt' =
  let res = allocate_gt () in
  Internal.gt_mul res gt gt';
  res

let gt_exp gt k =
  let res = allocate_gt () in
  Internal.gt_exp res gt k;
  res

(* *** Bilinear map *)

let e_pairing g1 g2 =
  let gt = allocate_gt () in
  Internal.pc_map gt g1 g2;
  gt
