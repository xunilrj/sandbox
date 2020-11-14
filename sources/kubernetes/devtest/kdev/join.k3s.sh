#! /bin/bash
K3S_URL="https://127.0.0.1:6443"
K3S_TOKEN=$(cat /var/lib/rancher/k3s/server/token)
sudo k3s agent --server "$K3S_URL" --token "$K3S_TOKEN"