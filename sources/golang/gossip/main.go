package main

import (
	"bufio"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"net"
	"os"
	"os/signal"
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
	get getData
}

func setGet(requests chan<- interface{}, key string) map[string]bool {
	q := getData{Keys: []string{key}}
	q.Response = make(chan setData)

	requests <- q
	r := <-q.Response

	dic := map[string]bool{}

	values := strings.Split(r.Values[0].Value, ",")
	for _, v := range values {
		if len(v) == 0 {
			continue
		}
		dic[v] = true
	}

	return dic
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
		query := strings.Replace(line, ".watch", "", 1)
		query = strings.Trim(query, " ")
		return getData{[]string{query}, make(chan setData)}, nil
	}
	if strings.HasPrefix(line, ".watch") {
		query := strings.Replace(line, ".watch", "", 1)
		query = strings.Trim(query, " ")
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

func runWatchData(dictionaryChannel chan<- interface{}, cmd watchData) {
	fmt.Println("start watch")
	ticker := time.NewTicker(1 * time.Second)
	sigs := make(chan os.Signal, 1)
	signal.Notify(sigs, syscall.SIGINT)
	defer signal.Reset(syscall.SIGINT)

	running := true
	for running {
		select {
		case <-ticker.C:
			dictionaryChannel <- cmd.get
			r := <-cmd.get.Response
			fmt.Println(r.Keys[0], "=", r.Values[0].Value)
		case <-sigs:
			running = false
		}
	}
	fmt.Println("end watch")
}

func runGetData(dictionaryChannel chan<- interface{}, cmd getData) {
	dictionaryChannel <- cmd
	r := <-cmd.Response

	fmt.Println(r.Keys[0], "=", r.Values[0].Value)
}

func main() {
	listenPort := flag.String("listen", "1200", "udp port this instance will listen")
	seedAddr := flag.String("seed", "", "ip:port of a known cluster node")

	flag.Parse()

	if len(*listenPort) == 0 {
		fmt.Println("ERROR: --listen flag is mandatory")
		os.Exit(1)
	}

	*listenPort = ":" + *listenPort

	done := make(chan bool, 2)
	dictionaryChannel := make(chan interface{}, 10)

	go dictionaryActor(dictionaryChannel)

	udp, err := openUDP(*listenPort)
	checkError(err)
	if len(*seedAddr) != 0 {
		*seedAddr = ":" + *seedAddr
		broadcastUDP(udp, *seedAddr, []byte{})
	}

	go listenGossip(udp, dictionaryChannel, done)
	go sendGossip(udp, *listenPort, dictionaryChannel, done)

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
			runWatchData(dictionaryChannel, cmd)
		case getData:
			runGetData(dictionaryChannel, cmd)
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
					v.Keys = getKeys(dic, func(k string) bool {
						return !strings.HasPrefix(k, "_")
					})
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

func openUDP(listenAt string) (*net.UDPConn, error) {
	udpAddr, err := net.ResolveUDPAddr("udp", listenAt)
	if err != nil {
		return nil, err
	}

	conn, err := net.ListenUDP("udp", udpAddr)
	if err != nil {
		return nil, err
	}

	return conn, nil
}

func listenGossip(udp *net.UDPConn, dic chan<- interface{}, done chan<- bool) {
	var buf [512]byte
	for {
		size, addr, err := udp.ReadFromUDP(buf[0:])
		if err != nil {
			break
		}

		setAppend(dic, "_peers", addr.String())

		r := setData{}
		err = json.Unmarshal(buf[0:size], &r)
		if err != nil {
			continue //TODO log
		}

		dic <- r
	}

	done <- true
}

func sendGossip(udp *net.UDPConn, listenAt string, dic chan<- interface{}, done chan<- bool) {
	ticker := time.NewTicker(5 * time.Second)

	for {
		<-ticker.C

		allData := getAllData(dic)
		jsonBytes, err := json.Marshal(allData)
		if err != nil {
			continue //TODO somehow log
		}

		for to := range setGet(dic, "_peers") {
			broadcastUDP(udp, to, jsonBytes)
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

func broadcastUDP(udp *net.UDPConn, toAddr string, bytes []byte) error {
	addr, err := net.ResolveUDPAddr("udp", toAddr)
	if err != nil {
		return err
	}

	_, err2 := udp.WriteToUDP(bytes, addr)
	if err2 != nil {
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

func getKeys(m map[string]dicData, filter func(k string) bool) []string {
	i := 0
	keys := make([]string, len(m))
	for k := range m {
		if filter(k) {
			keys[i] = k
		}
		i++
	}
	return keys
}
