package main

import (
	"fmt"
	"runtime"
	"strings"
	"time"

	"github.com/Originate/exocom/go/exorelay/test-fixtures/helpers"
	"github.com/Originate/exocom/go/exosocket"
)

// Uses the Send and WaitForActivity API wher WaitForActivity returns an error
// because no message is received
func main() {
	exoRelay := helpers.ConnectExoRelay()
	sentMessage, err := exoRelay.Send(exosocket.MessageOptions{ActivityID: "123", Name: "ping"})
	if err != nil {
		panic(err)
	}
	go func() {
		receivedMessage, err := exoRelay.WaitForActivity(sentMessage.ActivityID, time.Second)
		if err != nil {
			if strings.Contains(err.Error(), "Did not receive a reply for") {
				_, err = exoRelay.Send(exosocket.MessageOptions{Name: "pong not received"})
				if err != nil {
					panic(err)
				}
			} else {
				panic(err)
			}
		} else {
			panic(fmt.Errorf("Expected to not receive a reply, received: %v", receivedMessage))
		}
	}()
	runtime.Goexit()
}
