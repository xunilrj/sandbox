#! /bin/bash
# https://www.valgrind.org/docs/manual/ms-manual.html

cargo build --release

./target/release/epoll &
PID=$!
ab -c 20 -n 1000000 -r http://localhost:8888/ > ./target/release/ab.20.txt
kill $PID

./target/release/epoll &
PID=$!
ab -c 200 -n 1000000 -r http://localhost:8888/ > ./target/release/ab.200.txt
kill $PID