mkdir build
cd build
cmake build --ALLOC=DYNAMIC --SHLIB=on ../
make
sudo make install