#!/bin/bash
./build.sh
pushd server
npx snowpack dev
popd