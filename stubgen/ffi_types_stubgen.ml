let () =
  print_endline "#include <relic/relic.h>";
  Cstubs.Types.write_c Format.std_formatter (module Ffi_bindings.Types)
