#!/bin/bash
printf "GET / HTTP/1.0\r\n\r\n" | nc 127.0.0.1 8080 -q 5