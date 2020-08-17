package main

import (
	"fmt"
	"log"
	"time"

	"github.com/gocql/gocql"
)

// a
type PrintConnectObserver struct {
}

// a
func (o PrintConnectObserver) ObserveConnect(x gocql.ObservedConnect) {
	fmt.Println("ObserveConnect")
}

func (o PrintConnectObserver) ObserveDisconnect(e gocql.ObservedDisconnect) {
	fmt.Println("ObserveDisconnect.Host: ", e.Host)
	fmt.Println("ObserveDisconnect.Start: ", e.Start)
	fmt.Println("ObserveDisconnect.End: ", e.End)
	fmt.Println("ObserveDisconnect.Err: ", e.Err)
}

func PerformOperations() {
	cluster := gocql.NewCluster("172.18.0.3")
	cluster.Authenticator = gocql.PasswordAuthenticator{
		Username: "some_username",
		Password: "some_password",
	}
	cluster.Keyspace = "tracking"
	cluster.Timeout = 5 * time.Second
	cluster.ProtoVersion = 4
	cluster.ConnectObserver = PrintConnectObserver{}

	session, err := cluster.CreateSession()
	if err != nil {
		log.Fatalf("Could not connect to cassandra cluster: %v", err)
	}

	var name string
	var phone string
	if err := session.Query("select first_name, last_name from tracking.tracking_data").Scan(&name, &phone); err != nil {
		if err != gocql.ErrNotFound {
			log.Fatalf("Query failed: %v", err)
		}
	}
	log.Printf("Name: %v", name)
	log.Printf("Phone: %v", phone)

	session.Close()
}

func main() {
	PerformOperations()
}
