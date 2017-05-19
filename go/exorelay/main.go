package exorelay

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"

	"github.com/Originate/exocom/go/structs"
	uuid "github.com/satori/go.uuid"

	"golang.org/x/net/websocket"
)

// Config contains the configuration values for ExoRelay instances
type Config struct {
	Host string
	Port string
	Role string
}

// ExoRelay is the Go API to talk to Exocom
type ExoRelay struct {
	config         Config
	socket         *websocket.Conn
	messageChannel chan structs.Message
}

// New creates a new ExoRelay instance
func New(config Config) *ExoRelay {
	return &ExoRelay{
		config:         config,
		messageChannel: make(chan structs.Message),
	}
}

// Connect brings an ExoRelay instance online
func (exoRelay *ExoRelay) Connect() error {
	socket, err := websocket.Dial("ws://"+exoRelay.config.Host+":"+exoRelay.config.Port, "", "origin:")
	if err != nil {
		return err
	}
	exoRelay.socket = socket
	go exoRelay.listenForMessages()
	return exoRelay.Send("exocom.register-service", map[string]interface{}{"clientName": exoRelay.config.Role})
}

// GetMessageChannel returns a channel which can be used read incoming messages
func (exoRelay *ExoRelay) GetMessageChannel() chan structs.Message {
	return exoRelay.messageChannel
}

// Send sends the event with the given name and payload
func (exoRelay *ExoRelay) Send(eventName string, payload map[string]interface{}) error {
	if eventName == "" {
		return errors.New("ExoRelay#Send cannot send empty messages")
	}
	serializedBytes, err := json.Marshal(&structs.Message{
		ID:      uuid.NewV4().String(),
		Name:    eventName,
		Payload: payload,
		Sender:  exoRelay.config.Role,
	})
	if err != nil {
		return err
	}

	return websocket.Message.Send(exoRelay.socket, serializedBytes)
}

// Helpers

func (exoRelay *ExoRelay) listenForMessages() {
	for {
		message, err := exoRelay.readMessage()
		if err == io.EOF {
			break
		} else if err != nil {
			fmt.Println("Error reading message from websocket:", err)
			break
		} else {
			exoRelay.messageChannel <- message
		}
	}
}

func (exoRelay *ExoRelay) readMessage() (structs.Message, error) {
	var bytes []byte
	if err := websocket.Message.Receive(exoRelay.socket, &bytes); err != nil {
		return structs.Message{}, err
	}

	var unmarshaled structs.Message
	err := json.Unmarshal(bytes, &unmarshaled)
	if err != nil {
		return structs.Message{}, err
	}

	return unmarshaled, nil
}
