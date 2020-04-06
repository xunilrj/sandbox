#!/bin/bash

DIR=./mnist
FILE=./resources_mnist.tar.gz
URL=https://courses.edx.org/assets/courseware/v1/cc7c0cbadc1a2e9924d17fb14cc5ecc6/asset-v1:MITx+6.86x+1T2020+type@asset+block/resources_mnist.tar.gz

[[ ! -f "$FILE" && ! -d "$DIR" ]] && { wget "$URL"; }
[[ ! -f "$FILE" && ! -d "$DIR" ]] && { echo "ERROR DOWNLOADING: $URL"; exit 1; }
[[ -f "$FILE" && ! -d "$DIR" ]] && { tar -zxvf "$FILE"; }
[[ -f "$FILE" ]] && { rm "$FILE"; }

cd $DIR
cd part1
while inotifywait -e modify $(ls); do clear; python "test.py";  done
