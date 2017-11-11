package exorelay

import (
	"fmt"
	"sync"
	"time"

	"github.com/Originate/exocom/go/exosocket"
	"github.com/Originate/exocom/go/structs"
)

// ExoRelay is the mid level API to Exocom
type ExoRelay struct {
	exoSocket            *exosocket.ExoSocket
	messageHandlers      MessageHandlerMapping
	messageHandlersMutex sync.RWMutex
	replyChannels        ReplyChannelMapping
	replyChannelsMutex   sync.RWMutex
}

// NewExoRelay creates a new ExoRelay instance with the given config
func NewExoRelay(config exosocket.Config, messageBuffersize int) *ExoRelay {
	return &ExoRelay{
		exoSocket:       exosocket.NewExoSocket(config, messageBuffersize),
		replyChannels:   ReplyChannelMapping{},
		messageHandlers: MessageHandlerMapping{},
	}
}

// Connect connects to ExoCom and starts listening for messages
func (e *ExoRelay) Connect() error {
	err := e.exoSocket.Connect()
	if err != nil {
		return err
	}
	go e.listenForMessages()
	return nil
}

// Close disconnects from ExoCom
func (e *ExoRelay) Close() error {
	return e.exoSocket.Close()
}

// RegisterHandler adds a handler for the given message name
func (e *ExoRelay) RegisterHandler(messageName string, handler MessageHandler) {
	e.messageHandlersMutex.Lock()
	e.messageHandlers[messageName] = handler
	e.messageHandlersMutex.Unlock()
}

// Send sends a message
func (e *ExoRelay) Send(messageOptions exosocket.MessageOptions) (*structs.Message, error) {
	return e.exoSocket.Send(messageOptions)
}

// WaitForActivity returns the first message received with the given activityID
func (e *ExoRelay) WaitForActivity(activityID string, timeout time.Duration) (*structs.Message, error) {
	messageChannel := make(chan *structs.Message)
	e.addReplyChannel(activityID, messageChannel)
	defer e.removeReplyChannel(activityID)
	select {
	case message := <-messageChannel:
		return message, nil
	case <-time.After(timeout):
		return nil, fmt.Errorf("Did not receive a reply for '%s' after %s", activityID, timeout.String())
	}
}

// Helpers

func (e *ExoRelay) addReplyChannel(activityID string, messageChannel chan *structs.Message) {
	e.replyChannelsMutex.Lock()
	e.replyChannels[activityID] = messageChannel
	e.replyChannelsMutex.Unlock()
}

func (e *ExoRelay) buildRequest(message structs.Message) Request {
	return Request{
		Message: message,
		Reply: func(name string, payload structs.MessagePayload) error {
			_, err := e.exoSocket.Send(exosocket.MessageOptions{
				Name:       name,
				Payload:    payload,
				ActivityID: message.ActivityID,
				Auth:       message.Auth,
				IsSecurity: message.IsSecurity,
			})
			return err
		},
		Send: func(name string, payload structs.MessagePayload) (*structs.Message, error) {
			return e.exoSocket.Send(exosocket.MessageOptions{
				Name:    name,
				Payload: payload,
				Auth:    message.Auth,
			})
		},
		WaitForActivity: e.WaitForActivity,
	}
}

func (e *ExoRelay) listenForMessages() {
	messageChannel := e.exoSocket.GetMessageChannel()
	for {
		message, ok := <-messageChannel
		if !ok {
			return
		}
		go e.sendToMessageHandler(message)
		go e.sendToReplyChannel(message)
	}
}

func (e *ExoRelay) removeReplyChannel(activityID string) {
	e.replyChannelsMutex.Lock()
	delete(e.replyChannels, activityID)
	e.replyChannelsMutex.Unlock()
}

func (e *ExoRelay) sendToMessageHandler(message structs.Message) {
	e.messageHandlersMutex.RLock()
	handler, ok := e.messageHandlers[message.Name]
	e.messageHandlersMutex.RUnlock()
	if ok {
		handler(e.buildRequest(message))
	}
}

func (e *ExoRelay) sendToReplyChannel(message structs.Message) {
	e.replyChannelsMutex.RLock()
	replyChannel, ok := e.replyChannels[message.ActivityID]
	e.replyChannelsMutex.RUnlock()
	if ok {
		replyChannel <- &message
	}
}
