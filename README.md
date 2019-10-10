To install relic, clone the repository from https://github.com/relic-toolkit/relic and follow the instructions below:

```
mkdir build
cd build
cmake build --ALLOC=DYNAMIC --SHLIB=on ../
```

The flag --ALLOC=DYNAMIC may not work. Check that the file include/relic_conf.h contains the line "#define ALLOC   DYNAMIC" and fix is if necessary.

If you wish to compile the library for a different security level, modify value of the variable "FP_PRIME" in file include/relic_conf.h (the default value is 256).
The default curve chosen for EC is BSI-P256, and the default curve chosen for pairings is BN-P256.
If the value of FP_PRIME is set to 255, the EC curve will be CURVE_25519.

```
make
sudo make install
```

The installation works for commit 0e239a842b89126080e998e3836f83aff1078576 of relic.


To compile and test these bindings, execute the following commands:

```
oasis setup
./configure
make
./test_relic.native
```

Note: after installing relic, you can check the default curves with our 'check_relic.c' file:
```
gcc -I/usr/local/include/relic check_relic.c -lrelic -o check_relic.out && ./check_relic.out
```