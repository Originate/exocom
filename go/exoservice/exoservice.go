package exoservice

import (
	"errors"
	"fmt"
	"os"
	"strconv"
	"sync"
	"time"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/structs"
)

// Request contains payload and helper functions for sending messages
type Request struct {
	Payload         structs.MessagePayload
	Reply           func(exorelay.MessageOptions) error
	Send            func(exorelay.MessageOptions) (string, error)
	WaitForActivity func(string, time.Duration) (*structs.Message, error)
}

// MessageHandler is the function signature for handling a message
type MessageHandler func(Request)

// MessageHandlerMapping is a map from message name to MessageHandler
type MessageHandlerMapping map[string]MessageHandler

// ReplyChannelMapping is a map from message activityId to a channel that should be sent messages
type ReplyChannelMapping map[string]chan *structs.Message

// Bootstrap brings an ExoService online using environment variables and the
// given messageHandlers
func Bootstrap(messageHandlers MessageHandlerMapping) {
	port, err := strconv.Atoi(os.Getenv("EXOCOM_PORT"))
	if err != nil {
		panic(fmt.Sprintf("Invalid port: %s", os.Getenv("EXOCOM_PORT")))
	}
	config := exorelay.Config{
		Host: os.Getenv("EXOCOM_HOST"),
		Port: port,
		Role: os.Getenv("ROLE"),
	}
	service := ExoService{}
	err = service.Connect(config)
	if err != nil {
		panic(fmt.Sprintf("Error connecting: %v", err))
	}
	fmt.Println("online at port", port)
	err = service.ListenForMessages(messageHandlers)
	if err != nil {
		panic(fmt.Sprintf("Error listening for messages: %v", err))
	}
}

// ExoService is the high level Go API to talk to Exocom
type ExoService struct {
	closed             bool
	exoRelay           *exorelay.ExoRelay
	messageHandlers    MessageHandlerMapping
	replyChannels      ReplyChannelMapping
	replyChannelsMutex sync.RWMutex
}

// Connect brings an ExoService instance online
func (e *ExoService) Connect(config exorelay.Config) error {
	e.exoRelay = &exorelay.ExoRelay{Config: config}
	err := e.exoRelay.Connect()
	e.closed = false
	return err
}

// Close takes this ExoService instance offline
func (e *ExoService) Close() error {
	if e.closed {
		return nil
	}
	err := e.exoRelay.Close()
	e.closed = true
	return err
}

// ListenForMessages blocks while the instance listens for and responds to messages
// returns when the message channel closes
func (e *ExoService) ListenForMessages(messageHandlers MessageHandlerMapping) error {
	if e.exoRelay == nil {
		return errors.New("Please call Connect() first")
	}
	e.messageHandlers = messageHandlers
	e.replyChannels = map[string]chan *structs.Message{}
	messageChannel := e.exoRelay.GetMessageChannel()
	for {
		message, ok := <-messageChannel
		if !ok {
			return nil
		}
		go e.receiveMessage(message)
		e.replyChannelsMutex.RLock()
		replyChannel, ok := e.replyChannels[message.ActivityID]
		e.replyChannelsMutex.RUnlock()
		if ok {
			replyChannel <- &message
		}
	}
}

// Helpers

func (e *ExoService) receiveMessage(message structs.Message) {
	if e.messageHandlers == nil {
		return
	}
	if e.messageHandlers[message.Name] == nil {
		return
	}
	e.messageHandlers[message.Name](Request{
		Payload: message.Payload,
		Reply: func(options exorelay.MessageOptions) error {
			_, err := e.exoRelay.Send(exorelay.MessageOptions{
				Name:       options.Name,
				Payload:    options.Payload,
				ActivityID: message.ActivityID,
				Auth:       message.Auth,
			})
			return err
		},
		Send: func(options exorelay.MessageOptions) (string, error) {
			sentMessage, err := e.exoRelay.Send(exorelay.MessageOptions{
				Name:    options.Name,
				Payload: options.Payload,
				Auth:    message.Auth,
			})
			if err != nil {
				return "", err
			}
			return sentMessage.ActivityID, nil
		},
		WaitForActivity: func(activityID string, timeout time.Duration) (*structs.Message, error) {
			messageChannel := make(chan *structs.Message)
			e.replyChannelsMutex.Lock()
			e.replyChannels[activityID] = messageChannel
			e.replyChannelsMutex.Unlock()
			defer func() {
				e.replyChannelsMutex.Lock()
				delete(e.replyChannels, activityID)
				e.replyChannelsMutex.Unlock()
			}()
			select {
			case message := <-messageChannel:
				return message, nil
			case <-time.After(timeout):
				return nil, fmt.Errorf("Did not receive a reply for '%s' after %s", activityID, timeout.String())
			}
		},
	})
}
