#!/bin/bash

CURRENTDIR=$(pwd)
KDEVDIR=$(realpath $(dirname $0))

echo "Current Dir: $CURRENTDIR"
echo "KDev Dir: $KDEVDIR"

# Install dependencies

# Docker volume is correctly configured?
DOCKERVOLUMEEXISTS=$(docker volume inspect k3d-k3s-default-images|grep "Error: No such volume")
if [ ! -z "$DOCKERVOLUMEEXISTS" ]; then
    echo "Creating volume1"
    docker volume create k3d-k3s-default-images --label app=k3d --label k3d.cluster=k3s-default --opt type=none --opt device=/ --opt o=bind &> /dev/null
fi
DOCKERVOLUMEDEVICE=$(docker volume inspect k3d-k3s-default-images -f "{{.Options.device}}"|grep '<no value>')
if [ "$DOCKERVOLUMEDEVICE" == "<no value>" ]; then
    echo "Creating volume2"
    docker volume delete k3d-k3s-default-images &> /dev/null
    docker volume create k3d-k3s-default-images --label app=k3d --label k3d.cluster=k3s-default --opt type=none --opt device=/ --opt o=bind &> /dev/null
fi

INSTALLEDK3D=$(which k3d)
if [ -z "$INSTALLEDK3D" ]; then
    wget -q -O - https://raw.githubusercontent.com/rancher/k3d/main/install.sh | bash &> /dev/null
fi

K3DCLUSTER=$(k3d cluster list | wc -l)
if [ "$" != "2" ]; then 
    k3d cluster create &> /dev/null
fi

INSTALLEDHELM=$(which helm)
if [ -z "$INSTALLEDHELM" ]; then
    echo "Installing heml"
    curl -fsSL -o get_helm.sh https://raw.githubusercontent.com/helm/helm/master/scripts/get-helm-3 &> /dev/null
    chmod 700 get_helm.sh &> /dev/null
    ./get_helm.sh &> /dev/null
    rm ./get_helm.sh &> /dev/null
fi

INSTALLEDSKAFFOLD=$(which skaffold)
if [ -z "$INSTALLEDSKAFFOLD" ]; then
    curl -Lo skaffold https://storage.googleapis.com/skaffold/releases/latest/skaffold-linux-amd64 &> /dev/null
    sudo install skaffold /usr/local/bin/ &> /dev/null
    rm skaffold &> /dev/null
fi

# Starting setup
echo "Cleaning /tmp/kdev/$1"
rm "/tmp/kdev/$1" -rf &> /dev/null

echo "Copying files to /tmp/kdev/$1"
mkdir "/tmp/kdev/$1" -p &> /dev/null
pushd "/tmp/kdev/$1" &> /dev/null
cp "$KDEVDIR/Dockerfile" Dockerfile -f &> /dev/null
cp "$KDEVDIR/root.tar" root.tar -f &> /dev/null
cp "$KDEVDIR/skaffold.yaml.template" skaffold.yaml -f &> /dev/null
cp "$KDEVDIR/k8sdevimage.yaml.template" k8sdevimage.yaml -f &> /dev/null
sed -i "s/#PODNAME#/$1/g" k8sdevimage.yaml &> /dev/null

SUBNET=$(docker network inspect -f '{{range .IPAM.Config}}{{.Subnet}}{{end}}' k3d-k3s-default)
SUBNET0=$(echo "$SUBNET"|cut -f 1 -d '.')
SUBNET1=$(echo "$SUBNET"|cut -f 2 -d '.')
cp "$KDEVDIR/metallb.config.yaml.template" metallb.config.yaml &> /dev/null
sed -i "s/#SUBNET0#/$SUBNET0/g" metallb.config.yaml &> /dev/null
sed -i "s/#SUBNET1#/$SUBNET1/g" metallb.config.yaml &> /dev/null

echo "Applying changes to K8s"
# https://mauilion.dev/posts/kind-metallb/
kubectl create namespace metallb-system &> /dev/null
kubectl apply -f https://raw.githubusercontent.com/metallb/metallb/v0.9/manifests/metallb.yaml &> /dev/null
kubectl apply -f metallb.config.yaml &> /dev/null
kubectl apply -f "$KDEVDIR/volumes.yaml" &> /dev/null
skaffold run &> /dev/null

# routing
NODEIP=$(kubectl get node k3d-k3s-default-server-0 -o=jsonpath="{.status.addresses[?(@.type=='InternalIP')].address}")
NODEPORT=$(kubectl get svc nginx-nginx-ingress -o=jsonpath="{.spec.ports[?(@.name=='http')].nodePort}")
HOSTSLINE=$(cat /etc/hosts|grep k8s.local)
if [ -z "$HOSTSLINE" ]; then
    echo "$NODEIP k8s.local" >> /etc/hosts
fi

helm repo add nginx-stable https://helm.nginx.com/stable &> /dev/null
helm install nginx nginx-stable/nginx-ingress &> /dev/null

echo "Logging into $1-pod"
sleep 1 && echo -n "."
sleep 1 && echo -n "."
sleep 1 && echo -n "."
sleep 1 && echo -n "."
sleep 1 && echo "."
kubectl exec -it "$1-pod" -- bash -c "cd '$CURRENTDIR'; $SHELL"

popd &> /dev/null
