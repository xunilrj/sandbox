#!/bin/bash
#strace -ff nc -l 8080 
echo "1"
netstat -aplunt | grep :8080 | awk '{print $5}' | cut -d: -f2 | xargs -I{} fuser -k -n tcp {}
netstat -aplunt | grep :8080 
echo "2"
netstat -na | grep :8080  | awk '{print $5}' | cut -d: -f2 | xargs -I{} fuser -k -n tcp {}
netstat -na | grep :8080 
echo "3"
lsof -i :8080
echo "4"
lsof -i tcp | grep 8080
echo "5"
fuser 8080/tcp

# socklist | grep 8080
echo "6"
iptables -t filter -S | grep 8080
echo "7"
iptables -t nat -S | grep 8080
echo "8"
iptables -t mangle -S | grep 8080
# conntrack -L | grep 8080