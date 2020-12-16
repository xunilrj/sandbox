#!/bin/bash
pushd app
cargo build --features "derive_debug" --target wasm32-unknown-unknown --release
popd

rm server/content/app.wasm
cp target/wasm32-unknown-unknown/release/app.wasm server/content/app.wasm
cp target/wasm32-unknown-unknown/release/async_test.wasm server/content/async_test.wasm

# just to show size
/mnt/d/github/wabt/bin/wasm-strip ./target/wasm32-unknown-unknown/release/app.wasm
ls -l target/wasm32-unknown-unknown/release/app.wasm --block-size=KB

