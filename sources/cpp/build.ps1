mkdir .build -EA SilentlyContinue | Out-Null
pushd .build
    cmake .. -G "Visual Studio 16 2019"
    cmake --build .
popd