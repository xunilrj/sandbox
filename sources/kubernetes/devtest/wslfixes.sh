#! /bin/bash

# https://github.com/kubernetes-sigs/kind/issues/1879
# https://github.com/microsoft/WSL/issues/4189#issuecomment-518277265
# https://github.com/docker/for-linux/issues/219
sudo mkdir /sys/fs/cgroup/systemd -p
sudo mount -t cgroup -o none,name=systemd cgroup /sys/fs/cgroup/systemd

#dockerd
set DOCKERRUNNING (ps aux | grep dockerd | grep -v grep)
if [ -z "$DOCKERRUNNING" ]
echo "Starting Docker daemon"
sudo dockerd >/dev/null 2>&1 &
disown
end

curl -fsSL -o get_helm.sh https://raw.githubusercontent.com/helm/helm/master/scripts/get-helm-3
chmod 700 get_helm.sh
./get_helm.sh
rm get_helm.sh