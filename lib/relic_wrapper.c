#include <relic/relic.h>

// wrapper around bn_new macro that changes pointer in-place
void w_bn_new(bn_t* p) {
  bn_new(*p);
}
