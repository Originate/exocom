package exorelay

import (
	"encoding/json"
	"fmt"

	"github.com/Originate/exocom/go/structs"
	"github.com/Originate/exocom/go/utils"
	uuid "github.com/satori/go.uuid"

	"github.com/gorilla/websocket"
	"github.com/pkg/errors"
)

// Config contains the configuration values for ExoRelay instances
type Config struct {
	Host string
	Port int
	Role string
}

// MessageOptions contains the user input values for a message
type MessageOptions struct {
	Name       string
	Payload    structs.MessagePayload
	ActivityID string
	Auth       structs.MessageAuth
	IsSecurity bool
}

// ExoRelay is the low level Go API to talk to Exocom
type ExoRelay struct {
	Config                      Config
	shouldReconnectOnDisconnect bool
	socket                      *websocket.Conn
	messageChannel              chan structs.Message
}

// Connect brings an ExoRelay instance online
func (e *ExoRelay) Connect() error {
	if e.Config.Port == 0 {
		e.Config.Port = 80
	}
	exocomURL := fmt.Sprintf("ws://%s:%d/services", e.Config.Host, e.Config.Port)
	socket, err := utils.ConnectWithRetry(exocomURL, 100)
	if err != nil {
		return err
	}
	e.shouldReconnectOnDisconnect = true
	e.socket = socket
	e.messageChannel = make(chan structs.Message)
	go e.listenForMessages()
	_, err = e.Send(MessageOptions{
		Name: "exocom.register-service",
	})
	return err
}

// Close takes this ExoRelay instance offline
func (e *ExoRelay) Close() error {
	e.shouldReconnectOnDisconnect = false
	return e.socket.Close()
}

// GetMessageChannel returns a channel which can be used read incoming messages
func (e *ExoRelay) GetMessageChannel() chan structs.Message {
	return e.messageChannel
}

// Send sends a message with the given options
func (e *ExoRelay) Send(options MessageOptions) (*structs.Message, error) {
	var activityID string
	if e.socket == nil {
		return &structs.Message{}, errors.New("ExoRelay#Send not connected to Exocom")
	}
	id := uuid.NewV4().String()
	if options.Name == "" {
		return &structs.Message{}, errors.New("ExoRelay#Send cannot send empty messages")
	}
	if options.ActivityID == "" {
		activityID = uuid.NewV4().String()
	} else {
		activityID = options.ActivityID
	}
	message := &structs.Message{
		ID:         id,
		Name:       options.Name,
		Payload:    options.Payload,
		ActivityID: activityID,
		Sender:     e.Config.Role,
		Auth:       options.Auth,
		IsSecurity: options.IsSecurity,
	}
	serializedBytes, err := json.Marshal(message)
	if err != nil {
		return &structs.Message{}, err
	}
	return message, e.socket.WriteMessage(websocket.TextMessage, serializedBytes)
}

// Helpers

func (e *ExoRelay) listenForMessages() {
	utils.ListenForMessages(e.socket, func(message structs.Message) error {
		e.messageChannel <- message
		return nil
	}, func(err error) {
		fmt.Println(errors.Wrap(err, "Exorelay listening for messages"))
	})
	if e.shouldReconnectOnDisconnect {
		fmt.Println("Disconnected from exocom reconnecting...")
		err := e.Connect()
		if err != nil {
			fmt.Println("Unable to reconnect to exocom", err)
		}
	}
}
