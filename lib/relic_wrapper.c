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

typedef gt_t* my_gt_t;

void w_gt_new(my_gt_t* p){    
  my_gt_t q = malloc(sizeof(gt_t));
  *p = q;
  gt_new((**p)); // Inner parenthesis are very important because gt_new is a macro.
};

void w_gt_free(my_gt_t p){
  gt_free((*p));
  if (p != NULL){
    free(p);
  }
}

void w_gt_get_gen(my_gt_t p){
  gt_get_gen(*p);
}

int w_gt_is_unity(my_gt_t p){
  return gt_is_unity(*p);
}

void w_gt_zero(my_gt_t p){
  gt_zero(*p);
}

void w_gt_set_unity(my_gt_t p){
  gt_set_unity(*p);
}


int w_gt_cmp(my_gt_t p1, my_gt_t p2){
  return gt_cmp((*p1), (*p2));
}

void w_gt_rand(my_gt_t p){
  gt_rand(*p);
}

int w_gt_size_bin(my_gt_t p, int f){
  return gt_size_bin((*p), f);
}

void w_gt_read_bin(my_gt_t p, uint8_t *c, int d){
  gt_read_bin((*p), c, d);
}

void w_gt_write_bin(uint8_t *c, int f1, my_gt_t p, int f2){
  gt_write_bin(c, f1, (*p), f2);
}

void w_gt_inv(my_gt_t p1, my_gt_t p2){
  gt_inv((*p1), (*p2));
}

void w_gt_mul(my_gt_t p1, my_gt_t p2, my_gt_t p3){
  gt_mul((*p1), (*p2), (*p3));
}

void w_gt_exp(my_gt_t p1, my_gt_t p2, bn_t n){
  gt_exp((*p1), (*p2), n);
}
