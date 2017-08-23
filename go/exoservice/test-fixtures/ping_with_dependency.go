package exoserviceTestFixtures

import (
	"fmt"
	"time"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exoservice"
)

// PingWithDependencyTestFixture is a test fixture which responds to "ping" messages with "pong" messages
type PingWithDependencyTestFixture struct{}

// GetMessageHandler returns a message hangler
func (p *PingWithDependencyTestFixture) GetMessageHandler() exoservice.MessageHandlerMapping {
	return exoservice.MessageHandlerMapping{
		"ping": func(request exoservice.Request) {
			searchMessageActivityID, err := request.Send(exorelay.MessageOptions{Name: "search"})
			if err != nil {
				panic(fmt.Sprintf("Failed to send search: %v", err))
			}
			message, err := request.WaitForActivity(searchMessageActivityID, time.Second*5)
			if err != nil || message.Name != "result" {
				_, err = request.Send(exorelay.MessageOptions{Name: "ping error"})
				if err != nil {
					panic(fmt.Sprintf("Failed to send ping error: %v", err))
				}
				return
			}
			err = request.Reply(exorelay.MessageOptions{Name: "pong", Payload: message.Payload})
			if err != nil {
				panic(fmt.Sprintf("Failed to send pong: %v", err))
			}
		},
	}
}
