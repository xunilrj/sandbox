mkdir build
pushd build
cmake .. -DCMAKE_TOOLCHAIN_FILE=D:/github/vcpkg/scripts/buildsystems/vcpkg.cmake -DVCPKG_TARGET_TRIPLET=x64-windows
popd