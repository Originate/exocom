package exoserviceTestFixtures

import (
	"fmt"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exoservice"
	"github.com/Originate/exocom/go/structs"
)

// PingTextFixture is a test fixture which responds to "ping" messages with "pong" messages
type PingTextFixture struct {
	ReceivedMessages []structs.Message
}

// Setup creates an ExoService instance for the given config
func (r *PingTextFixture) Setup(config exorelay.Config) *exoservice.ExoService {
	return &exoservice.ExoService{
		Config: config,
		MessageHandlers: exoservice.MessageHandlerMapping{
			"ping": func(_ structs.MessagePayload, helpers exoservice.SendHelpers) {
				err := helpers.Reply(exorelay.MessageOptions{Name: "pong"})
				if err != nil {
					fmt.Println("Failed to send reply")
				}
			},
		},
	}
}
