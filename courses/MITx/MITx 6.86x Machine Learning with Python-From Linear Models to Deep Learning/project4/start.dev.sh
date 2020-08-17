#!/bin/bash

DIR=./netflix
FILE=./resources_netflix.tar.gz
URL=https://courses.edx.org/assets/courseware/v1/d548226c37482952193ff6f363761b54/asset-v1:MITx+6.86x+1T2020+type@asset+block/resources_netflix.tar.gz

[[ ! -f "$FILE" && ! -d "$DIR" ]] && { wget "$URL"; }
[[ ! -f "$FILE" && ! -d "$DIR" ]] && { echo "ERROR DOWNLOADING: $URL"; exit 1; }
[[ -f "$FILE" && ! -d "$DIR" ]] && { tar -zxvf "$FILE"; }
[[ -f "$FILE" ]] && { rm "$FILE"; }

cd $DIR
while inotifywait -e modify $(ls); do clear; python "test.py";  done
