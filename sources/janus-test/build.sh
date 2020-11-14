#! /bin/bash

sudo apt-get update

# List of packages to install before building Janus
packagelist=(
    git
    libmicrohttpd-dev
    libjansson-dev
    libssl-dev
    libsrtp2-dev
    libsofia-sip-ua-dev
    libglib2.0-dev
    libopus-dev
    libogg-dev
    libcurl4-openssl-dev
    liblua5.3-dev
    libconfig-dev
    pkg-config
    gengetopt
    libtool
    automake
    gtk-doc-tools
    cmake
    meson
)
sudo apt-get install ${packagelist[@]} -y

# install_libnice.sh
git clone https://gitlab.freedesktop.org/libnice/libnice
pushd libnice
meson builddir
ninja -C builddir
ninja test -C builddir
sudo ninja -C builddir install
popd

# install_libsrtp.sh
wget https://github.com/cisco/libsrtp/archive/v2.2.0.tar.gz
tar xfv v2.2.0.tar.gz
pushd libsrtp-2.2.0
./configure --prefix=/usr --enable-openssl
make shared_library && sudo make install
popd

# install_usrsctp.sh
git clone https://github.com/sctplab/usrsctp
pushd usrsctp
./bootstrap
./configure --prefix=/usr && make && sudo make install
popd

# install_libwebsockets.sh
git clone https://github.com/warmcat/libwebsockets.git
pushd libwebsockets
mkdir build
cd build
# See https://github.com/meetecho/janus-gateway/issues/732 re: LWS_MAX_SMP
cmake -DLWS_MAX_SMP=1 -DCMAKE_INSTALL_PREFIX:PATH=/usr -DCMAKE_C_FLAGS="-fpic" ..
make && sudo make install
popd

# install_mqtt.sh
git clone https://github.com/eclipse/paho.mqtt.c.git
pushd paho.mqtt.c
sudo prefix=/usr make install
popd

# install_rabbitmqc.sh
git clone https://github.com/alanxz/rabbitmq-c
pushd rabbitmq-c
git submodule init
git submodule update
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=/usr ..
make && sudo make install
popd

# janus
git clone https://github.com/meetecho/janus-gateway.git
pushd janus-gateway
sh autogen.sh
./configure --prefix=/opt/janus
make
make install
make configs
popd

/opt/janus/bin/janus --help