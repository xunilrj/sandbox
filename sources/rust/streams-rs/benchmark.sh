#!/bin/bash
TIMESTAMP=$(date +"%Y%m%d-%H%M")

cargo bench --bench my_benchmark -- --save-baseline "$TIMESTAMP"
FIRST=$(find "./target/criterion/xadd/" -maxdepth 1 -name "2020*"|sort|head -n 1)
LAST=$(find "./target/criterion/xadd/" -maxdepth 1 -name "2020*"|sort|tail -n 1)
critcmp "$(basename $FIRST)" "$(basename $LAST)"