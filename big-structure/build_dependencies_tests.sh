cd /dependencies_for_de-stress/EvoEF2/ &&\ 
g++ -O3 --fast-math -o EvoEF2 src/*.cpp

cd /dependencies_for_de-stress/rosetta/source/ &&\ 
./scons.py -j30 mode=release bin
