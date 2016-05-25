#include <relic/relic.h>

void w_bn_new(bn_t*);

void w_g1_new(g1_t*);

void w_g2_new(g2_t*);

void w_gt_new(gt_t*);

void w_gt_free(gt_t*);

void w_gt_get_gen(gt_t*);

int w_gt_is_unity(gt_t*);

void w_gt_zero(gt_t*);

void w_gt_set_unity(gt_t*);

int w_gt_cmp(gt_t*, gt_t*);

void w_gt_rand(gt_t*);

int w_gt_size_bin(gt_t*, int);

void w_gt_read_bin(gt_t*, char*, int);

void w_gt_write_bin(char*, int, gt_t*, int);

void w_gt_inv(gt_t*, gt_t*);

void w_gt_mul(gt_t*, gt_t*, gt_t*);

void w_gt_exp(gt_t*, gt_t*, bn_t);
