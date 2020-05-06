# References

## Courses

## Articles

Hands-on: Creating a simple web server in Kubernetes  
https://canviz.com/hands-on-creating-a-simple-web-server-in-kubernetes  
  
Kubernetes Services simply visually explained  
https://medium.com/swlh/kubernetes-services-simply-visually-explained-2d84e58d70e5  

Kubernetes Ingress simply visually explained  
https://codeburst.io/kubernetes-ingress-simply-visually-explained-d9cad44e4419  

Kubernetes Istio simply visually explained  
https://itnext.io/kubernetes-istio-simply-visually-explained-58a7d158b83f  

Kubernetes Serverless simply visually explained  
https://itnext.io/kubernetes-serverless-simply-visually-explained-ccf7be05a689  

Kubernetes without Nodes  
https://itnext.io/kubernetes-without-nodes-caedd172f940

# VIP yamls

## nginx Ingress

kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/nginx-0.24.1/deploy/mandatory.yaml
kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/nginx-0.24.1/deploy/provider/cloud-generic.yaml

## Kong for Kubernetes

https://github.com/Kong/kubernetes-ingress-controller
kubectl apply -f https://bit.ly/k4k8s