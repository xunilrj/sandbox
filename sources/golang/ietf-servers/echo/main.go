package main

import (
	"fmt"
	"net"
	"os"
	"time"
)

func main() {
	service := ":7"
	tcpAddr, err := net.ResolveTCPAddr("tcp4", service)
	checkError(err)

	listener, err := net.ListenTCP("tcp", tcpAddr)
	checkError(err)

	for {
		conn, err := listener.Accept()
		if err != nil {
			continue
		}
		go handleClient(conn)
	}
}

func handleClient(conn net.Conn) {
	defer conn.Close()

	var buf [512]byte
	for {
		err := conn.SetDeadline(time.Now().Add(2 * time.Second))
		if err != nil {
			break
		}

		n, err := conn.Read(buf[0:])
		if err != nil {
			break
		}

		_, err2 := conn.Write(buf[0:n])
		if err2 != nil {
			break
		}
	}
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}
