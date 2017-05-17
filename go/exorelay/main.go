package exorelay

import (
	"encoding/json"
	"errors"

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
	config Config
	socket *websocket.Conn
}

// New creates a new ExoRelay instance
func New(config Config) *ExoRelay {
	return &ExoRelay{
		config: config,
	}
}

// Connect brings an ExoRelay instance online
func (exoRelay *ExoRelay) Connect() error {
	socket, err := websocket.Dial("ws://"+exoRelay.config.Host+":"+exoRelay.config.Port, "", "origin:")
	if err != nil {
		return err
	}
	exoRelay.socket = socket
	return exoRelay.Send("exocom.register-service", map[string]interface{}{"clientName": exoRelay.config.Role})
}

// Send sends the event with the given name and payload
func (exoRelay *ExoRelay) Send(eventName string, payload map[string]interface{}) error {
	if eventName == "" {
		return errors.New("ExoRelay#Send cannot send empty messages")
	}
	serializedBytes, err := json.Marshal(&structs.Message{
		Id:      uuid.NewV4().String(),
		Name:    eventName,
		Payload: payload,
		Sender:  exoRelay.config.Role,
	})
	if err != nil {
		return err
	}

	return websocket.Message.Send(exoRelay.socket, serializedBytes)
}
