#! /bin/bash

export glibc_install="$(pwd)/sourceware/glibc/build/install"
pushd /root/github
git clone git://sourceware.org/git/glibc.git sourceware/glibc

cd sourceware/glibc
git checkout glibc-2.32
mkdir build
cd build
../configure --prefix "$glibc_install"
make -j `nproc`
make install -j `nproc`

cd install
tar -cvf ../glibc.tar .
cp /root/github/sourceware/glibc/build/glibc.tar /root/github/xunilrj/sandbox/sources/kubernetes/devtest/k8s/glibc.tar

popd