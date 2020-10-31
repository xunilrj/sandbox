#! /bin/bash
clang++-10 main.cpp -ldl
pushd MySharedLibRust
cargo build
cp target/debug/libMySharedLib.so ../MySharedLib.so
popd