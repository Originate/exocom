package connection

import (
	"fmt"

	"github.com/Originate/exocom/go/structs"
	"github.com/gorilla/websocket"
)

// Manager manages all websocket connections
type Manager struct {
	errorChannel    chan error
	messageChannel  chan structs.Message
	registerChannel chan string
	services        map[string]*Service
}

// NewManager returns a new Manager
func NewManager(errorChannel chan error, messageChannel chan structs.Message, registerChannel chan string) *Manager {
	return &Manager{
		errorChannel:    errorChannel,
		messageChannel:  messageChannel,
		registerChannel: registerChannel,
		services:        map[string]*Service{},
	}
}

// AddWebsocket adds the websocket under the manager's control
func (m *Manager) AddWebsocket(socket *websocket.Conn) {
	BootstrapService(m, socket)
}

// GetClients returns all the connections
func (m *Manager) GetClients() (result []Client) {
	for role := range m.services {
		result = append(result, Client{Role: role})
	}
	return
}

// SendMessage sends the given message to the service with the given role
func (m *Manager) SendMessage(role string, message structs.Message) error {
	if m.services[role] == nil {
		return fmt.Errorf("No connected service for role '%s'", role)
	}
	return m.services[role].Send(message)
}

// Helpers

func (m *Manager) logError(err error) {
	m.errorChannel <- err
}

func (m *Manager) onMessage(message structs.Message) {
	m.messageChannel <- message
}

func (m *Manager) registerService(service *Service) {
	m.services[service.role] = service
	m.registerChannel <- service.role
}

func (m *Manager) deregisterService(service *Service) {
	delete(m.services, service.role)
}
