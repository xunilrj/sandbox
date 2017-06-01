#!/bin/bash
REGENERATE=$1
if [ "$REGENERATE" == "regenerate" ] ; then
	rm ./.build -rf
fi
if [ ! -d .build ]; then
    mkdir ./.build
    mkdir ./.build/unix-debug
    mkdir ./.build/unix-release
fi

cd .build/unix-debug
conan install ../../ -s build_type=Debug --build=missing
cmake ../../ -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Debug
cmake --build .
cd ../../
cd .build/unix-release
conan install ../../ -s build_type=Release --build=missing
cmake ../../ -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release
cmake --build .
cd ../../