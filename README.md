To install relic, clone the repository from https://github.com/relic-toolkit/relic and follow the instructions below:

```
mkdir build
cd build
cmake build --ALLOC=DYNAMIC --SHLIB=on ../
make
sudo make install
```

To compile and test these bindings, execute the following commands:

```
oasis setup
./configure
make
./test_relic.native
```
