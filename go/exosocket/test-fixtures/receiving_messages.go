package fixtures

import (
	"sync"

	"github.com/Originate/exocom/go/exosocket"
	"github.com/Originate/exocom/go/structs"
	"github.com/Originate/exocom/go/utils"
)

// ReceivingMessagesTestFixture is a test fixture which saves the messages it receives
type ReceivingMessagesTestFixture struct {
	receivedMessagesMutex sync.RWMutex
	receivedMessages      []structs.Message
}

// Setup setups up the test fixture for the given exorelay instance
func (r *ReceivingMessagesTestFixture) Setup(exoSocket *exosocket.ExoSocket) {
	messageChannel := exoSocket.GetMessageChannel()
	go func() {
		for {
			message, ok := <-messageChannel
			if !ok {
				break // channel closed
			}
			r.receivedMessagesMutex.Lock()
			r.receivedMessages = append(r.receivedMessages, message)
			r.receivedMessagesMutex.Unlock()
		}
	}()
}

// GetReceivedMessages return the received messages
func (r *ReceivingMessagesTestFixture) GetReceivedMessages() []structs.Message {
	r.receivedMessagesMutex.RLock()
	defer r.receivedMessagesMutex.RUnlock()
	return r.receivedMessages
}

// WaitForMessageWithName waits to receive a message with the given name
func (r *ReceivingMessagesTestFixture) WaitForMessageWithName(name string) (structs.Message, error) {
	return utils.WaitForMessageWithName(r, name)
}
