cd /dependencies_for_de-stress/EvoEF2/ &&\
    g++ -O3 --fast-math -o EvoEF2 src/*.cpp
cd /dependencies_for_de-stress/rosetta_src_2020.08.61146_bundle/main/source/ &&\
    ./scons.py -j2 mode=release  bin/score_jd2.linuxgccrelease
