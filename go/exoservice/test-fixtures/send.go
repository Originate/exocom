package exoserviceTestFixtures

import (
	"fmt"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exoservice"
)

// SendTestFixture is a test fixture which sends "ping_received" messages when it receives "ping" messages
type SendTestFixture struct{}

// GetMessageHandler returns a message hangler
func (r *SendTestFixture) GetMessageHandler() exoservice.MessageHandlerMapping {
	return exoservice.MessageHandlerMapping{
		"ping": func(request exoservice.Request) {
			_, err := request.Send(exorelay.MessageOptions{Name: "pong"})
			if err != nil {
				panic(fmt.Sprintf("Failed to send message: %v", err))
			}
		},
		"ping it": func(request exoservice.Request) {
			_, err := request.Send(exorelay.MessageOptions{Name: "pong it"})
			if err != nil {
				panic(fmt.Sprintf("Failed to send message: %v", err))
			}
		},
	}
}
