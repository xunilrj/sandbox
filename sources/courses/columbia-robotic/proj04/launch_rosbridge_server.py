#!/usr/bin/python3.4 -u

"""

This code does 2 things:

1. It finds a free port and starts the rosbridge websocket server using that value
2. Configures the proxy server so that the client can connect to it

It is possible that in the time between finding the free port and launching the server, some
other process starts using the port.

So if the server does not start due to the port not being free, just run this program again.

"""

import json
import os
import socket
import subprocess
import sys
import traceback
import urllib

from pprint import pprint
from urllib.parse import urlencode
from urllib.request import Request, urlopen
from urllib.error import HTTPError, URLError

# find a free port
def get_free_port():
    """get a single random port"""
    sock = socket.socket()
    sock.bind(('', 0))
    port = sock.getsockname()[1]
    sock.close()
    return port

def get_env_var(name):
    val = os.getenv(name)
    if val is None:
        print("Error: Could not find env var {}; please contact Vocareum Support".format(name))
        sys.exit(1)

def configure_proxy(server_port, proxy_id, proxy_port, proxy_token):
    # send the token + id
    url = "http://localhost:" + str(proxy_port) + "/configure"
    post_data = { 'id': proxy_id, 'port': server_port, 'token': proxy_token }

    errmsg = None
    retval = False
    try:
        headers = {'content-type': 'application/json'}
        request = Request(url, data = json.dumps(post_data).encode('utf8'), headers = headers)
        response = urlopen(request)
        if response.status == 200:
            retval = True
        else:
            errmsg = "Proxy Configuration Error: Status = " + str(response.status)
    except urllib.error.HTTPError as e:
        errmsg = "HTTP Error: {}".format(e)
    except urllib.error.URLError as e:
        errmsg = "URL Error: {}".format(e)
    except:
        # exc_type, exc_value, exc_traceback = sys.exc_info()
        # pprint("Traceback: {}".format(traceback.format_exception(exc_type, exc_value, exc_traceback)))
        # print('generic exception: ' + traceback.format_exc())
        errmsg = "Proxy Configuration Error"

    return retval, errmsg

if __name__ == '__main__':
    # get env vars
    proxy_id = os.getenv("VOC_PROXY_ID")
    proxy_port = os.getenv("VOC_PROXY_WRAPPER_PORT")
    proxy_token = os.getenv("VOC_PROXY_TOKEN")

    if proxy_id is None or proxy_port is None or proxy_token is None:
        print("Fatal Error: environment not setup properly")
        sys.exit(1)
    
    server_port = get_free_port()
    print("Port for WebSocketServer: {}".format(server_port))
    
    retval, errmsg = configure_proxy(server_port, proxy_id, proxy_port, proxy_token)
    if retval == False:
        print("Error: Could not configure Proxy Server: {}".format(errmsg))
        print("       If running this script again does not help, please contact Vocareum Technical Support")
        sys.exit(1)

    args = [ 'roslaunch', 'rosbridge_server', 'rosbridge_websocket.launch', 'port:=' + str(server_port) ]
    print("Running cmd: {}".format(' '.join(args)))
    subprocess.call(args)