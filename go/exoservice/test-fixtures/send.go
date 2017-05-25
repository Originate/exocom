package exoserviceTestFixtures

import (
	"fmt"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exoservice"
	"github.com/Originate/exocom/go/structs"
)

// SendTextFixture is a test fixture which sends "ping_received" messages when it receives "ping" messages
type SendTextFixture struct {
	ReceivedMessages []structs.Message
}

// Setup creates an ExoService instance for the given config
func (r *SendTextFixture) Setup(config exorelay.Config) *exoservice.ExoService {
	return &exoservice.ExoService{
		Config: config,
		MessageHandlers: exoservice.MessageHandlerMapping{
			"ping": func(_ structs.MessagePayload, helpers exoservice.SendHelpers) {
				err := helpers.Send(exorelay.MessageOptions{Name: "ping_received"})
				if err != nil {
					panic(fmt.Sprintf("Failed to send message: %v", err))
				}
			},
		},
	}
}
