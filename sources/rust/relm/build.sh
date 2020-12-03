#!/bin/bash
cargo build --target wasm32-unknown-unknown --release
/mnt/d/github/wabt/bin/wasm-strip ./target/wasm32-unknown-unknown/release/relm.wasm
cp target/wasm32-unknown-unknown/release/relm.wasm server/content/relm.wasm