package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"net"
	"os"
	"strconv"
	"strings"
	"time"
)

type setData struct {
	Keys   []string
	Values []string
}
type getData struct {
	keys     []string
	response chan setData
}

func main() {
	listenAt := os.Args[1]

	done := make(chan bool, 2)
	dictionaryChannel := make(chan interface{}, 10)

	go dictionaryActor(dictionaryChannel)
	go listenGossip(listenAt, dictionaryChannel, done)
	go sendGossip(listenAt, dictionaryChannel, done)

	scanner := bufio.NewScanner(os.Stdin)
	fmt.Print("> ")
	for scanner.Scan() {
		line := scanner.Text()

		if line[0] == '?' {
			query := strings.Trim(line, "?")

			q := getData{keys: []string{query}}
			q.response = make(chan setData)

			dictionaryChannel <- q
			r := <-q.response

			fmt.Println(r.Keys[0], "=", r.Values[0])
			fmt.Print("> ")
			continue
		}

		parts := strings.Split(line, "=")

		if len(parts) != 2 {
			fmt.Println("Syntax Error: KEY=VALUE")
			fmt.Print("> ")
			continue
		}

		key := strings.Trim(parts[0], " ")
		value := strings.Trim(parts[1], " ")

		r := setData{Keys: []string{key}, Values: []string{value}}
		dictionaryChannel <- r

		fmt.Print("> ")
	}

	<-done
	<-done
}

func dictionaryActor(requests <-chan interface{}) {
	dic := map[string]string{}

	for {
		req := <-requests

		switch v := req.(type) {
		case getData:
			{
				if len(v.keys) == 0 {
					for key := range dic {
						v.keys = append(v.keys, key)
					}
				}

				resp := setData{}
				resp.Keys = make([]string, len(v.keys))
				resp.Values = make([]string, len(v.keys))
				for i, k := range v.keys {
					resp.Keys[i] = k
					resp.Values[i] = dic[k]
				}

				v.response <- resp
			}
		case setData:
			{
				for i, k := range v.Keys {
					dic[k] = v.Values[i]
				}
			}
		}
	}
}

func listenGossip(listenAt string, dic chan<- interface{}, done chan<- bool) {
	udpAddr, err := net.ResolveUDPAddr("udp", listenAt)
	checkError(err)

	conn, err := net.ListenUDP("udp", udpAddr)
	checkError(err)

	var buf [512]byte
	for {
		size, _, err2 := conn.ReadFromUDP(buf[0:])
		if err2 != nil {
			break
		}

		r := setData{}
		err = json.Unmarshal(buf[0:size], &r)
		checkError(err)

		dic <- r
	}

	done <- true
}

func sendGossip(listenAt string, dic chan<- interface{}, done chan<- bool) {
	ticker := time.NewTicker(5 * time.Second)
	for {
		<-ticker.C

		data := getData{}
		data.response = make(chan setData)
		dic <- data
		r := <-data.response
		close(data.response)

		jsonBytes, err := json.Marshal(r)
		checkError(err)

		seq := sequence(1200, 1210)

		for _, port := range seq {
			portStr := ":" + strconv.Itoa(port)

			if portStr == listenAt {
				continue
			}

			udpAddr, err := net.ResolveUDPAddr("udp", "255.255.255.255"+portStr)
			checkError(err)

			conn, err := net.DialUDP("udp", nil, udpAddr)
			checkError(err)

			_, err = conn.Write(jsonBytes)
			checkError(err)
		}
	}
	done <- true
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error ", err.Error())
		os.Exit(1)
	}
}

func sequence(min, max int) []int {
	a := make([]int, max-min+1)
	for i := range a {
		a[i] = min + i
	}
	return a
}
