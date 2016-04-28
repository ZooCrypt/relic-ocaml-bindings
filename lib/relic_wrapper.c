#include <relic/relic.h>

// wrapper around bn_new macro that changes pointer in-place
void w_bn_new(bn_t* p) {
  bn_new(*p);
}

// wrapper around g1_new macro that changes pointer in-place
void w_g1_new(g1_t* p) {
  g1_new(*p);
}

// wrapper around g2_new macro that changes pointer in-place
void w_g2_new(g2_t* p) {
  g2_new(*p);
}

// wrapper around gt_new macro that changes pointer in-place
void w_gt_new(gt_t* p) {
  gt_new(*p);
}
