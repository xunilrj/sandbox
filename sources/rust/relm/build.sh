#!/bin/bash
cargo build --target wasm32-unknown-unknown --release
#/mnt/d/github/wabt/bin/wasm-strip ./target/wasm32-unknown-unknown/release/relm.wasm
rm server/content/app.wasm
cp target/wasm32-unknown-unknown/release/app.wasm server/content/app.wasm