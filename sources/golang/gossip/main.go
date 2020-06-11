package main

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"os"
	"os/signal"
	"strconv"
	"strings"
	"syscall"
	"time"
)

type dicData struct {
	Value     string
	Timestamp time.Time
}
type setData struct {
	Keys   []string
	Values []dicData
}
type getData struct {
	Keys     []string
	Response chan setData
}
type watchData struct {
	getData
}

func setAppend(requests chan<- interface{}, key string, value string) {
	q := getData{Keys: []string{key}}
	q.Response = make(chan setData)

	requests <- q
	r := <-q.Response

	dic := map[string]bool{value: true}

	values := strings.Split(r.Values[0].Value, ",")
	for _, v := range values {
		if len(v) == 0 {
			continue
		}
		dic[v] = true
	}

	r.Values[0].Value = ""
	for v := range dic {
		r.Values[0].Value += v + ","
	}

	r.Values[0].Timestamp = time.Now()

	requests <- setData{Keys: []string{key}, Values: []dicData{r.Values[0]}}
}

func parseLine(line string) (interface{}, error) {
	if strings.HasPrefix(line, ".query") {
		query := strings.Trim(line, ".query")
		query = strings.Trim(line, " ")
		return getData{[]string{query}, make(chan setData)}, nil
	}
	if strings.HasPrefix(line, ".watch") {
		query := strings.Trim(line, ".watch")
		query = strings.Trim(line, " ")
		return watchData{getData{[]string{query}, make(chan setData)}}, nil
	}

	parts := strings.Split(line, "=")

	if len(parts) != 2 {
		return nil, errors.New("Syntax error")
	}

	key := strings.Trim(parts[0], " ")
	value := strings.Trim(parts[1], " ")
	d := dicData{value, time.Now()}

	return setData{Keys: []string{key}, Values: []dicData{d}}, nil
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
		result, err := parseLine(line)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Fatal error ", err.Error())
			continue
		}

		switch cmd := result.(type) {
		case watchData:
			fmt.Println("start watch")
			ticker := time.NewTicker(1 * time.Second)
			sigs := make(chan os.Signal, 1)
			signal.Notify(sigs, syscall.SIGINT)
			running := true
			for running {
				select {
				case <-ticker.C:
					dictionaryChannel <- cmd
					r := <-cmd.Response
					fmt.Println(r.Keys[0], "=", r.Values[0].Value)
				case <-sigs:
					running = false
				}
			}
			fmt.Println("end watch")
		case getData:
			dictionaryChannel <- cmd
			r := <-cmd.Response

			fmt.Println(r.Keys[0], "=", r.Values[0].Value)
		case setData:
			dictionaryChannel <- cmd
		}

		fmt.Print("> ")
	}

	<-done
	<-done
}

func dictionaryActor(requests <-chan interface{}) {
	dic := map[string]dicData{}

	for {
		req := <-requests

		switch v := req.(type) {
		case getData:
			{
				if len(v.Keys) == 0 {
					v.Keys = getKeys(dic)
				}

				resp := setData{}
				resp.Keys = make([]string, len(v.Keys))
				resp.Values = make([]dicData, len(v.Keys))
				for i, k := range v.Keys {
					resp.Keys[i] = k
					resp.Values[i] = dic[k]
				}

				v.Response <- resp
			}
		case setData:
			{
				for i, k := range v.Keys {
					value := v.Values[i]
					current, found := dic[k]
					if !found || value.Timestamp.After(current.Timestamp) {
						dic[k] = v.Values[i]
					}
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
	defer conn.Close()

	var buf [512]byte
	for {
		size, addr, err2 := conn.ReadFromUDP(buf[0:])
		if err2 != nil {
			break
		}

		setAppend(dic, "peers", addr.String())

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

		allData := getAllData(dic)
		jsonBytes, err := json.Marshal(allData)
		if err != nil {
			continue //TODO somehow log
		}

		//TODO keep peers ips in dic
		for _, port := range sequence(1200, 1210) {
			portStr := ":" + strconv.Itoa(port)

			if portStr == listenAt {
				continue
			}

			broadcastUDP(portStr, jsonBytes)
		}
	}

	done <- true //TODO integrate signal mgt
}

func getAllData(dic chan<- interface{}) setData {
	data := getData{}
	data.Response = make(chan setData)
	defer close(data.Response)

	dic <- data
	r := <-data.Response

	return r
}

func broadcastUDP(portStr string, bytes []byte) error {
	udpAddr, err := net.ResolveUDPAddr("udp", "255.255.255.255"+portStr)
	if err != nil {
		return err
	}

	conn, err := net.DialUDP("udp", nil, udpAddr)
	if err != nil {
		return err
	}
	defer conn.Close()

	_, err = conn.Write(bytes)
	if err != nil {
		return err
	}

	return nil
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

func getKeys(m map[string]dicData) []string {
	i := 0
	keys := make([]string, len(m))
	for k := range m {
		keys[i] = k
		i++
	}
	return keys
}
