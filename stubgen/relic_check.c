#include <stdio.h>
#include <assert.h>

#include <relic/relic.h>

int pairing_check(){
  bn_t n, a, b, ab;
  g1_t g;
  g2_t h;
  gt_t gt, z1, z2;

  bn_null(n);
  bn_null(a);
  bn_null(b);
  bn_null(ab);
  g1_null(g);
  g2_null(h);
  gt_null(gt);
  gt_null(z1);
  gt_null(z2);

  bn_new(n);
  bn_new(a);
  bn_new(b);
  bn_new(ab);
  g1_new(g);
  g2_new(h);
  gt_new(gt);
  gt_new(z1);
  gt_new(z2);

  g1_get_gen(g);
  g2_get_gen(h);

  /* gt = e(g1, g2)  */
  pc_map(gt, g, h);

  g1_get_ord(n);

  bn_rand(a, BN_POS, 2 * pc_param_level());
  bn_mod(a, a, n);

  bn_rand(b, BN_POS, 2 * pc_param_level());
  bn_mod(b, b, n);


  bn_mul(ab, a, b);
  bn_mod(ab, ab, n);

  g1_mul_gen(g, a);
  g2_mul_gen(h, b);

  /* z1 = e(g1^a, g2^b) */
  pc_map(z1, g, h);

  /* z2 = e(g1, g2)^(a*b) */
  gt_exp(z2, gt, ab);

  if (gt_cmp(z1, z2) == CMP_EQ){
    printf ("%s\n", "Test succeeded!");
  } else {
    printf ("%s\n", "Test failed!");
  }

  bn_free(n);
  bn_free(a);
  bn_free(b);
  bn_free(ab);
  g1_free(g);
  g2_free(h);
  gt_free(gt);
  gt_free(z1);
  gt_free(z2);
  return 0;
}

int main(void) {
  assert(core_init() == STS_OK);
  assert(pc_param_set_any() == STS_OK);

  pairing_check();
  return 0;
}
