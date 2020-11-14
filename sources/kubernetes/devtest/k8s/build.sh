#! /bin/bash

rm root.tar 2> /dev/null
rm root.tar.xz 2> /dev/null
tar -cf root.tar -T /dev/null

tar_dependencies () {
    if [ ! -z "$2" ]; then
        mkdir $(dirname $2)
        pushd $(dirname $2)
        cp "$1" "$2"
        popd
        tar -rf root.tar "$2" 2> /dev/null
        rm -rf $(dirname $2)
    fi
    if [ -z "$2" ]; then
        tar -rf root.tar "$1" 2> /dev/null
    fi
    
    deps=$(ldd "$1")
    for line in $deps; do
        if [ -f "$line" ]; then
            tar -rf root.tar "$line" 2> /dev/null
            
            fullpath=$(realpath "$line")
            if [ -f "$fullpath" ]; then
                tar -rf root.tar "$fullpath" 2> /dev/null
            fi
        fi
    done
}

tar_dependencies "/bin/bash"
tar_dependencies "/bin/ls"
tar_dependencies "/bin/mkdir"
tar_dependencies "/usr/bin/wget"
tar_dependencies "/root/.cargo/bin/bat" "/bin/bat"

mkdir bin && pushd bin && ln -s /bin/bash sh && popd
tar -rf root.tar bin/sh
rm -rf bin
echo "Files that will be copied:"

tar -tvf root.tar
xz root.tar

docker build . -t k8s.server