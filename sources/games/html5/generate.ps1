Set-Alias vcpkg C:\github\vcpkg\vcpkg.exe
vcpkg install uwebsockets:x64-windows

rm build -Force -Recurse -ErrorAction SilentlyContinue
mkdir build 
pushd build
CMake .. -G "Visual Studio 15 2017 Win64" -DCMAKE_TOOLCHAIN_FILE=C:\github\vcpkg\scripts\buildsystems\vcpkg.cmake
popd