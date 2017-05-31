rm .build -rf
mkdir .build
cd .build
mkdir unix-make-release
cd unix-make-release
conan install ../.. --build missing
cmake ../.. -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release  -DCMAKE_TOOLCHAIN_FILE=./conanbuildinfo.cmake

cd ..
mkdir unix-make-debug
conan install ../.. --build missing
cmake ../.. -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Debug  -DCMAKE_TOOLCHAIN_FILE=./conanbuildinfo.cmake
