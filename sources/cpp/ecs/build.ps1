mkdir .build -EA SilentlyContinue
pushd .build
    cmake .. -G "Visual Studio 16 2019"
    cmake --build .
popd