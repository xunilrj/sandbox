mkdir .build/msvc-x64-debug -Force
cd .build/msvc-x64-debug
conan install ../../ -s build_type=Debug --build=missing
cmake ../../ -G "Visual Studio 15 Win64" -DCMAKE_BUILD_TYPE=Debug
cmake --build .
cd ../../
mkdir .build/msvc-x64-release -Force
cd .build/msvc-x64-release
conan install ../../ -s build_type=Release --build=missing
cmake ../../ -G "Visual Studio 15 Win64" -DCMAKE_BUILD_TYPE=Release
cmake --build .
cd ../../