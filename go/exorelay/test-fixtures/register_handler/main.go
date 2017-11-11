package main

import (
	"fmt"
	"runtime"
	"time"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exorelay/test-fixtures/helpers"
)

// Uses the registerHandler API
func main() {
	exoRelay := helpers.ConnectExoRelay()
	exoRelay.RegisterHandler("ping", func(request exorelay.Request) {
		err := request.Reply("pong", nil)
		if err != nil {
			fmt.Printf("Failed to send reply: %v", err)
		}
	})
	exoRelay.RegisterHandler("complex ping", func(request exorelay.Request) {
		searchMessage, err := request.Send("search", nil)
		if err != nil {
			fmt.Printf("Failed to send search: %v", err)
		}
		resultMessage, err := request.WaitForActivity(searchMessage.ActivityID, time.Second*5)
		if err != nil || resultMessage.Name != "result" {
			_, err = request.Send("complex ping error", nil)
			if err != nil {
				fmt.Printf("Failed to send complex ping error: %v", err)
			}
			return
		}
		err = request.Reply("complex pong", resultMessage.Payload)
		if err != nil {
			fmt.Printf("Failed to send complex pong: %v", err)
		}
	})
	runtime.Goexit()
}
