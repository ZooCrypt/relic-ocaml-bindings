#include <relic/relic.h>

void w_bn_new(bn_t*);

void w_g1_new(g1_t*);

void w_g2_new(g2_t*);

typedef gt_t* my_gt_t;

void w_gt_new(my_gt_t*);

void w_gt_free(my_gt_t);

void w_gt_get_gen(my_gt_t);

int w_gt_is_unity(my_gt_t);

void w_gt_zero(my_gt_t);

void w_gt_set_unity(my_gt_t);

int w_gt_cmp(my_gt_t, my_gt_t);

void w_gt_rand(my_gt_t);

int w_gt_size_bin(my_gt_t, int);

void w_gt_read_bin(my_gt_t, uint8_t*, int);

void w_gt_write_bin(uint8_t*, int, my_gt_t, int);

void w_gt_inv(my_gt_t, my_gt_t);

void w_gt_mul(my_gt_t, my_gt_t, my_gt_t);

void w_gt_exp(my_gt_t, my_gt_t, bn_t);

void w_pc_map(my_gt_t, g1_t, g2_t);
