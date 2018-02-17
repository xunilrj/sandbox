#!/usr/bin/python3.4 -u

"""

This code finds a free port

"""

import socket

# find a free port
def get_free_port():
    """get a single random port"""
    sock = socket.socket()
    sock.bind(('', 0))
    port = sock.getsockname()[1]
    sock.close()
    return port

print(get_free_port())