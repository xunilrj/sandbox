param($Path)
rm ./$Path.wasm -EA SilentlyContinue
rm ./$Path.wat -EA SilentlyContinue
clang --target=wasm32-unknown-unknown-wasm ./$Path -c -o ./$Path.o -O3
lld -flavor wasm ./$Path.o -o ./$Path.wasm --no-entry --export-dynamic -allow-undefined
#rm ./$Path.o
D:/github/wasm-toolchain/wabt/bin/wasm2wat.exe ./$Path.wasm -v > ./$Path.wat