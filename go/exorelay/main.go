package exorelay

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strconv"

	"github.com/Originate/exocom/go/structs"
	uuid "github.com/satori/go.uuid"

	"golang.org/x/net/websocket"
)

// Config contains the configuration values for ExoRelay instances
type Config struct {
	Host string
	Port int
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
func (e *ExoRelay) Connect() error {
	socket, err := websocket.Dial("ws://"+e.config.Host+":"+strconv.Itoa(e.config.Port), "", "origin:")
	if err != nil {
		return err
	}
	e.socket = socket
	go e.listenForMessages()
	_, err = e.Send("exocom.register-service", map[string]interface{}{"clientName": e.config.Role})
	return err
}

// GetMessageChannel returns a channel which can be used read incoming messages
func (e *ExoRelay) GetMessageChannel() chan structs.Message {
	return e.messageChannel
}

// Send sends the event with the given name and payload
// returns the outgoing message id when sent successfully
func (e *ExoRelay) Send(eventName string, payload map[string]interface{}) (string, error) {
	id := uuid.NewV4().String()
	if eventName == "" {
		return "", errors.New("ExoRelay#Send cannot send empty messages")
	}
	serializedBytes, err := json.Marshal(&structs.Message{
		ID:      id,
		Name:    eventName,
		Payload: payload,
		Sender:  e.config.Role,
	})
	if err != nil {
		return "", err
	}
	return id, websocket.Message.Send(e.socket, serializedBytes)
}

// Helpers

func (e *ExoRelay) listenForMessages() {
	for {
		message, err := e.readMessage()
		if err == io.EOF {
			break
		} else if err != nil {
			fmt.Println("Error reading message from websocket:", err)
			break
		} else {
			e.messageChannel <- message
		}
	}
}

func (e *ExoRelay) readMessage() (structs.Message, error) {
	var bytes []byte
	if err := websocket.Message.Receive(e.socket, &bytes); err != nil {
		return structs.Message{}, err
	}

	var unmarshaled structs.Message
	err := json.Unmarshal(bytes, &unmarshaled)
	if err != nil {
		return structs.Message{}, err
	}

	return unmarshaled, nil
}
