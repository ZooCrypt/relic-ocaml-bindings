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
   char* ok_msg;

   ok = ec_param_set_any ();
   if (ok == 0){ ok_msg = "True"; } else { ok_msg = "False"; };
   level = ec_param_level ();
   printf("EC Curve:");
   ec_param_print();
   printf("Security Level:%d\nOk: %s\n\n", level, ok_msg);

   ok = pc_param_set_any ();
   if (ok == 0){ ok_msg = "True"; } else { ok_msg = "False"; };
   level = pc_param_level ();
   printf("Pairings Curve:");
   pc_param_print();
   printf("Security Level:%d\nOk: %s\n", level, ok_msg);

   return 0;
}
