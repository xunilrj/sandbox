#!/bin/bash

# Client Certificate
openssl req -newkey rsa:2048 -nodes -keyout clientkey.pem -x509 -days 365 -out clientcertificate.pem

# Server Certificate
openssl req -newkey rsa:2048 -nodes -keyout key.pem -x509 -days 365 -out certificate.pem