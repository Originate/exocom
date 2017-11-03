package exosocket

import (
	"encoding/json"
	"fmt"
	"net/url"
	"sync"

	"github.com/Originate/exocom/go/structs"
	"github.com/Originate/exocom/go/utils"
	uuid "github.com/satori/go.uuid"

	"github.com/gorilla/websocket"
	"github.com/pkg/errors"
)

// ExoSocket is the low level API to Exocom
type ExoSocket struct {
	config                           Config
	shouldReconnectOnDisconnectMutex sync.Mutex
	shouldReconnectOnDisconnect      bool
	socket                           *websocket.Conn
	messageChannel                   chan structs.Message
}

// NewExoSocket returns an ExoSocket with the given config
func NewExoSocket(config Config, messageBufferSize int) *ExoSocket {
	return &ExoSocket{
		config:         config,
		messageChannel: make(chan structs.Message, messageBufferSize),
	}
}

// Connect brings an ExoSocket instance online
func (e *ExoSocket) Connect() error {
	exocomURL := url.URL{
		Scheme: "ws",
		Host:   fmt.Sprintf("%s:%s", e.config.Host, e.config.Port),
		Path:   "services",
	}
	socket, err := utils.ConnectWithRetry(exocomURL.String(), 100)
	if err != nil {
		return err
	}
	e.shouldReconnectOnDisconnectMutex.Lock()
	e.shouldReconnectOnDisconnect = true
	e.shouldReconnectOnDisconnectMutex.Unlock()
	e.socket = socket
	go e.listenForMessages()
	_, err = e.Send(MessageOptions{
		Name: "exocom.register-service",
	})
	return err
}

// Close takes this ExoRelay instance offline
func (e *ExoSocket) Close() error {
	e.shouldReconnectOnDisconnectMutex.Lock()
	e.shouldReconnectOnDisconnect = false
	e.shouldReconnectOnDisconnectMutex.Unlock()
	return e.socket.Close()
}

// GetMessageChannel returns a channel which can be used read incoming messages
func (e *ExoSocket) GetMessageChannel() chan structs.Message {
	return e.messageChannel
}

// Send sends a message with the given options
func (e *ExoSocket) Send(options MessageOptions) (*structs.Message, error) {
	if e.socket == nil {
		return &structs.Message{}, errors.New("ExoRelay#Send not connected to Exocom")
	}
	if options.Name == "" {
		return &structs.Message{}, errors.New("ExoRelay#Send cannot send empty messages")
	}
	activityID := options.ActivityID
	if activityID == "" {
		activityID = uuid.NewV4().String()
	}
	message := &structs.Message{
		ID:         uuid.NewV4().String(),
		Name:       options.Name,
		Payload:    options.Payload,
		ActivityID: activityID,
		Sender:     e.config.Role,
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

func (e *ExoSocket) listenForMessages() {
	utils.ListenForMessages(e.socket, func(message structs.Message) error {
		e.messageChannel <- message
		return nil
	}, func(err error) {
		fmt.Println(errors.Wrap(err, "Exorelay listening for messages"))
	})
	e.shouldReconnectOnDisconnectMutex.Lock()
	shouldReconnect := e.shouldReconnectOnDisconnect
	e.shouldReconnectOnDisconnectMutex.Unlock()
	if shouldReconnect {
		fmt.Println("Disconnected from exocom reconnecting...")
		err := e.Connect()
		if err != nil {
			fmt.Println("Unable to reconnect to exocom", err)
		}
	}
}
