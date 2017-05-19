package exorelay_test_fixtures

import (
	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/structs"
)

type ReceivingMessagesTestFixture struct {
	ReceivedMessages []structs.Message
}

func (r *ReceivingMessagesTestFixture) Setup(exoRelay *exorelay.ExoRelay) {
	messageChannel := exoRelay.GetMessageChannel()
	go func() {
		for {
			message, ok := <-messageChannel
			if !ok {
				break // channel closed
			}
			r.ReceivedMessages = append(r.ReceivedMessages, message)
		}
	}()
}

func (r *ReceivingMessagesTestFixture) GetReceivedMessages() []structs.Message {
	return r.ReceivedMessages
}
