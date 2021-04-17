#! /bin/bash
for i in $(seq 0.01 0.001 0.02); do
    echo -n "$i "
    cargo -q run --release -- dp data/ks_10000_0 $i true
done