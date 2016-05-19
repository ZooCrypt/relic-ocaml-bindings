To install relic, clone the repository from https://github.com/relic-toolkit/relic and follow the instructions below:

```
mkdir build
cd build
cmake build --ALLOC=DYNAMIC --SHLIB=on ../
```

The flag --ALLOC=DYNAMIC may not work. Check that the file include/relic_conf.h contains the line "#define ALLOC   DYNAMIC" and fix is if necessary.

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
