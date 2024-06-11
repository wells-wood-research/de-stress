cd /dependencies_for_de-stress/EvoEF2/ &&\ 
g++ -O3 --fast-math -o EvoEF2 src/*.cpp

echo "How many jobs do you want to run in order to compile Rosetta?"
read numofjobs
cd /dependencies_for_de-stress/rosetta/source/ &&\ 
./scons.py -j$numofjobs mode=release bin
