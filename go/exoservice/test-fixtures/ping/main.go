package main

import (
	"fmt"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exoservice"
)

func main() {
	exoservice.Bootstrap(exorelay.MessageHandlerMapping{
		"ping": func(request exorelay.Request) {
			err := request.Reply("pong", nil)
			if err != nil {
				fmt.Printf("Failed to send reply: %v", err)
			}
		},
	})
}
