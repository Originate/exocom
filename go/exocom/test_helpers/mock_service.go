package testHelpers

import (
	"encoding/json"
	"fmt"

	"github.com/gorilla/websocket"
	"github.com/pkg/errors"

	"github.com/Originate/exocom/go/structs"
	"github.com/Originate/exocom/go/utils"
)

// MockService is a mock of a real service that would connect to exocom
type MockService struct {
	exocomPort       int
	role             string
	socket           *websocket.Conn
	ReceivedMessages []structs.Message
}

// NewMockService returns a new MockService
func NewMockService(exocomPort int, role string) *MockService {
	return &MockService{
		role:             role,
		exocomPort:       exocomPort,
		ReceivedMessages: []structs.Message{},
	}
}

// Connect connects the mock service to exocom
func (m *MockService) Connect() error {
	url := fmt.Sprintf("ws://%s:%d/services", "localhost", m.exocomPort)
	var err error
	m.socket, err = utils.ConnectWithRetry(url, 100)
	if err != nil {
		return err
	}
	go m.websocketHandler()
	serializedBytes, err := json.Marshal(structs.Message{
		Name:       "exocom.register-service",
		Payload:    map[string]string{"clientName": m.role},
		ActivityID: "123",
	})
	if err != nil {
		return err
	}
	return m.socket.WriteMessage(websocket.TextMessage, serializedBytes)
}

//Send allows the mock service to send a websocket message to exocom
func (m *MockService) Send(message structs.Message) error {
	serializedBytes, err := json.Marshal(message)
	if err != nil {
		return err
	}
	return m.socket.WriteMessage(websocket.TextMessage, serializedBytes)
}

// Close disconnects the mock service from exocom
func (m *MockService) Close() error {
	if m.socket != nil {
		err := m.socket.Close()
		m.socket = nil
		return err
	}
	return nil
}

//GetReceivedMessages gets the messages this mock service has received
func (m *MockService) GetReceivedMessages() []structs.Message {
	if m.ReceivedMessages == nil {
		return []structs.Message{}
	}
	return m.ReceivedMessages
}

//WaitForMessageWithName tells this mock service to wait to receive a message with the given name
func (m *MockService) WaitForMessageWithName(name string) (structs.Message, error) {
	return utils.WaitForMessageWithName(m, name)
}

func (m *MockService) websocketHandler() {
	utils.ListenForMessages(m.socket, func(message structs.Message) error {
		m.ReceivedMessages = append(m.ReceivedMessages, message)
		return nil
	}, func(err error) {
		fmt.Println(errors.Wrap(err, "Exocom listening for messages"))
	})
}