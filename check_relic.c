#include <stdio.h>
#include "relic.h"

int main()
{
   if (core_init() != STS_OK) {
     core_clean();
     return 1;
   }

   int ok;
   int level;
   char* ok_msg = NULL;

   ok = ec_param_set_any ();
   if (ok == 0){ ok_msg = "True"; } else { ok_msg = "False"; };
   level = ec_param_level ();
   printf("EC Curve:");
   ec_param_print();
   printf("Security Level:%d\nOk: %s\n\n", level, ok_msg);

   bn_t ord;
   bn_new(ord);
   ec_curve_get_ord(ord);

   int length;
   length = bn_size_str(ord, 10);
   char order_str[length];
   bn_write_str(order_str, length, ord, 10);

   printf("Order: %s\n", order_str);

   ok = pc_param_set_any ();
   if (ok == 0){ ok_msg = "True"; } else { ok_msg = "False"; };
   level = pc_param_level ();
   printf("Pairings Curve:");
   pc_param_print();
   printf("Security Level:%d\nOk: %s\n", level, ok_msg);

   return 0;
}
