#!/bin/sh

set -e

rustup target add x86_64-pc-windows-gnu
rustup target add x86_64-unknown-linux-musl

rm -rf bin
cargo clean

RUSTFLAGS='-C link-arg=-s' cargo build --release --target x86_64-pc-windows-gnu
RUSTFLAGS='-C link-arg=-s' cargo build --release --target x86_64-unknown-linux-musl

mkdir bin
cp target/x86_64-pc-windows-gnu/release/tool.exe bin/tool-x86_64-windows.exe
cp target/x86_64-unknown-linux-musl/release/tool bin/tool-x86_64-linux

cargo clean
