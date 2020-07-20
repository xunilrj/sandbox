#! /bin/bash
# https://www.valgrind.org/docs/manual/ms-manual.html

cargo build --release
valgrind --tool=massif --massif-out-file=./target/release/massif.out ./target/release/epoll &
PID=$!
ab -c 20 -n 1000000 -r http://localhost:8888/
ms_print ./target/release/massif.out > ./target/release/massif.txt
kill $PID