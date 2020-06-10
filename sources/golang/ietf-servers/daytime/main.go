package main

import (
	"fmt"
	"net"
	"os"
	"time"
)

func main() {
	service := ":13"
	tcpAddr, err := net.ResolveTCPAddr("tcp", service)
	checkError(err)

	listener, err := net.ListenTCP("tcp", tcpAddr)
	checkError(err)

	for {
		conn, _ := listener.Accept()
		if err != nil {
			continue
		}

		daytime := time.Now().String()
		conn.Write([]byte(daytime))

		conn.Close()
	}
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
