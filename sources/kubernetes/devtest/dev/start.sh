#! /bin/bash

mkdir .cluster -p
pushd .cluster

# create cluster if needed
KINDCLUSTER=$(kind get clusters 2>&1)
if [ "$KINDCLUSTER" = "No kind clusters found." ];
then
    echo "No kind cluster found. Creating one..."
    PARENTPWD=$(pwd)
    PARENTPWD=$(realpath "$PARENTPWD/../..")
    PARENTPWD=$(echo "${PARENTPWD}" | sed -e 's/\//\\\//g' )
    echo "s/#PWD#/$PARENTPWD/g"
    cp ../kind-config.yaml.template kind-config.yaml
    sed -i "s/#PWD#/$PARENTPWD/g" kind-config.yaml
    kind create cluster --config kind-config.yaml
    
    SUBNET=$(docker network inspect -f '{{range .IPAM.Config}}{{.Subnet}}{{end}}' kind)
    SUBNET0=$(echo "$SUBNET"|cut -f 1 -d '.')
    SUBNET1=$(echo "$SUBNET"|cut -f 2 -d '.')
    cp ../metallb.config.yaml.template metallb.config.yaml
    sed -i "s/#SUBNET0#/$SUBNET0/g" metallb.config.yaml
    sed -i "s/#SUBNET1#/$SUBNET1/g" metallb.config.yaml
    # https://mauilion.dev/posts/kind-metallb/
    kubectl create namespace metallb-system
    kubectl apply -f https://raw.githubusercontent.com/metallb/metallb/v0.9/manifests/metallb.yaml
    kubectl apply -f metallb.config.yaml
    
    helm repo add nginx-stable https://helm.nginx.com/stable
    helm install nginx nginx-stable/nginx-ingress
fi

echo "Creating/Updating volumes..."
cp ../volumes.yaml.template volumes.yaml
kubectl apply -f volumes.yaml

svcname=".$1"

mkdir $svcname -p
pushd $svcname
cp ../../container.start.sh start.sh
cp ../../Dockerfile.template Dockerfile
sed -i 's/#DOCKERFROM#/ekidd\/rust-musl-builder:stable/g' Dockerfile
cp ../../k8s.yaml.template k8s.yaml
sed -i 's/#PODNAME#/$1/g' k8s.yaml
cp ../../skaffold.yaml.template skaffold.yaml

skaffold run

popd

popd