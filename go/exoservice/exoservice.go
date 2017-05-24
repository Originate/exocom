package exoservice

import (
	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/structs"
)

// SendHelpers contains helper functions for sending messages
type SendHelpers struct {
	Reply func(exorelay.MessageOptions) error
}

// MessageHandler is the function signature for handling a message
type MessageHandler func(structs.MessagePayload, SendHelpers)

// MessageHandlerMapping is a map from message name to MessageHandler
type MessageHandlerMapping map[string]MessageHandler

// ExoService is the high level Go API to talk to Exocom
type ExoService struct {
	Config          exorelay.Config
	MessageHandlers MessageHandlerMapping
	exoRelay        exorelay.ExoRelay
}

// Connect brings an ExoService instance online
func (e *ExoService) Connect() error {
	e.exoRelay = exorelay.ExoRelay{Config: e.Config}
	err := e.exoRelay.Connect()
	if err != nil {
		return err
	}
	go e.listenForMessages()
	return nil
}

// Helpers

func (e *ExoService) listenForMessages() {
	messageChannel := e.exoRelay.GetMessageChannel()
	for {
		message, ok := <-messageChannel
		if !ok {
			break // channel closed
		}
		e.receiveMessage(message)
	}
}

func (e *ExoService) receiveMessage(message structs.Message) {
	if e.MessageHandlers == nil {
		return
	}
	if e.MessageHandlers[message.Name] == nil {
		return
	}
	e.MessageHandlers[message.Name](message.Payload, SendHelpers{
		Reply: e.buildSendHelper(message.ID),
	})
}

func (e *ExoService) buildSendHelper(responseTo string) func(exorelay.MessageOptions) error {
	return func(options exorelay.MessageOptions) error {
		_, err := e.exoRelay.Send(exorelay.MessageOptions{
			Name:       options.Name,
			Payload:    options.Payload,
			ResponseTo: responseTo,
		})
		return err
	}
}
