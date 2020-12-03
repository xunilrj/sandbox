#!/bin/bash
clear && pushd runtime && cargo expand --lib login::view; popd