package main

import (
	"fmt"
	"runtime"
	"time"

	"github.com/Originate/exocom/go/exorelay/test-fixtures/helpers"
	"github.com/Originate/exocom/go/exosocket"
)

// Uses the Send and WaitForActivity API
func main() {
	exoRelay := helpers.ConnectExoRelay()
	sentMessage, err := exoRelay.Send(exosocket.MessageOptions{ActivityID: "123", Name: "ping"})
	if err != nil {
		panic(err)
	}
	go func() {
		receivedMessage, err := exoRelay.WaitForActivity(sentMessage.ActivityID, time.Second)
		if err != nil {
			panic(err)
		}
		if receivedMessage.Name == "pong" {
			_, err := exoRelay.Send(exosocket.MessageOptions{Name: "pong received"})
			if err != nil {
				panic(err)
			}
		} else {
			panic(fmt.Errorf("Expected to receive a pong message, received: %v", receivedMessage))
		}
	}()
	runtime.Goexit()
}
