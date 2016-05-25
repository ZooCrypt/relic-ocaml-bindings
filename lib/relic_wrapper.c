#include <relic/relic.h>

// wrapper around bn_new macro that changes pointer in-place
void w_bn_new(bn_t* p){
  bn_new(*p);
}

// wrapper around g1_new macro that changes pointer in-place
void w_g1_new(g1_t* p){
  g1_new(*p);
}

// wrapper around g2_new macro that changes pointer in-place
void w_g2_new(g2_t* p){
  g2_new(*p);
}

// wrapper around gt macros:

void w_gt_new(gt_t* p){
  gt_new((*p));  // Inner parenthesis are very important because gt_new is a macro.
};

void w_gt_free(gt_t* p){
  gt_free((*p));
}

void w_gt_get_gen(gt_t *p){
  gt_get_gen(*p);
}

int w_gt_is_unity(gt_t *p){
  return gt_is_unity(*p);
}

void w_gt_zero(gt_t *p){
  gt_zero(*p);
}

void w_gt_set_unity(gt_t *p){
  gt_set_unity(*p);
}


int w_gt_cmp(gt_t *p1, gt_t *p2){
  return gt_cmp((*p1), (*p2));
}

void w_gt_rand(gt_t *p){
  gt_rand(*p);
}

int w_gt_size_bin(gt_t *p, int f){
  return gt_size_bin((*p), f);
}

void w_gt_read_bin(gt_t* p, char *c, int d){
  gt_read_bin((*p), (*c), d);
}

void w_gt_write_bin(char *c, int f1, gt_t *p, int f2){
  gt_write_bin((c), f1, (*p), f2);
}

void w_gt_inv(gt_t *p1, gt_t *p2){
  gt_inv((*p1), (*p2));
}

void w_gt_mul(gt_t *p1, gt_t *p2, gt_t *p3){
  gt_mul((*p1), (*p2), (*p3));
}

void w_gt_exp(gt_t *p1, gt_t *p2, bn_t n){
  gt_exp((*p1), (*p2), n);
}
