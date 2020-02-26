#!/bin/bash

DIR=./sentiment_analysis
FILE=./resources_sentiment_analysis.tar.gz
URL=https://courses.edx.org/assets/courseware/v1/0b1085c09fa355e23c7a8eefd8a17085/asset-v1:MITx+6.86x+1T2020+type@asset+block/resources_sentiment_analysis.tar.gz

[[ ! -f "$FILE" && ! -d "$DIR" ]] && { wget "$URL"; }
[[ ! -f "$FILE" && ! -d "$DIR" ]] && { echo "ERROR DOWNLOADING: $URL"; exit 1; }
[[ -f "$FILE" && ! -d "$DIR" ]] && { tar -zxvf "$FILE"; exit 0; }
[[ -f "$FILE" ]] && { rm "$FILE"; }

cd $DIR
while inotifywait -e modify $(ls); do clear; python "test.py";  done
