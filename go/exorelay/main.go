package exorelay

import (
	"encoding/json"

	"github.com/Originate/exocom/go/structs"

	"golang.org/x/net/websocket"
)

// ExoRelay is the Go API to talk to Exocom
type ExoRelay struct {
	url    string
	socket *websocket.Conn
}

// New creates a new ExoRelay instance
func New(url string, config map[string]interface{}) *ExoRelay {
	return &ExoRelay{
		url: url,
	}
}

// Connect brings an ExoRelay instance online
func (exoRelay *ExoRelay) Connect() error {
	socket, err := websocket.Dial(exoRelay.url, "", "origin:")
	if err != nil {
		return err
	}
	exoRelay.socket = socket
	return exoRelay.Send("exocom.register-service", nil)
}

// Send sends the event with the given name and payload
func (exoRelay *ExoRelay) Send(eventName string, payload map[string]interface{}) error {
	serializedBytes, err := json.Marshal(&structs.Message{
		Name:    eventName,
		Payload: payload,
	})
	if err != nil {
		return err
	}

	return websocket.Message.Send(exoRelay.socket, serializedBytes)
}
