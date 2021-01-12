#!/bin/bash
cd dependencies_for_isambard/ &&
git clone https://github.com/tommyhuangthu/EvoEF2.git &&
cd EvoEF2/ &&
g++ -O3 --fast-math -o EvoEF2 src/*.cpp
 

