package connection

import (
	"fmt"
	"sync"

	"github.com/Originate/exocom/go/structs"
	"github.com/gorilla/websocket"
)

// Manager manages all websocket connections
type Manager struct {
	deregisterChannel chan string
	errorChannel      chan error
	messageChannel    chan structs.Message
	registerChannel   chan string
	services          map[string]*Service
	servicesMutex     sync.RWMutex
}

// NewManager returns a new Manager
func NewManager(options ManagerOptions) *Manager {
	return &Manager{
		deregisterChannel: options.DeregisterChannel,
		errorChannel:      options.ErrorChannel,
		messageChannel:    options.MessageChannel,
		registerChannel:   options.RegisterChannel,
		services:          map[string]*Service{},
	}
}

// AddWebsocket adds the websocket under the manager's control
func (m *Manager) AddWebsocket(socket *websocket.Conn) {
	BootstrapService(m, socket)
}

// GetClients returns all the connections
func (m *Manager) GetClients() (result []Client) {
	m.servicesMutex.RLock()
	for role := range m.services {
		result = append(result, Client{Role: role})
	}
	m.servicesMutex.RUnlock()
	return
}

// SendMessage sends the given message to the service with the given role
func (m *Manager) SendMessage(role string, message structs.Message) error {
	m.servicesMutex.RLock()
	defer m.servicesMutex.RUnlock()
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
	m.servicesMutex.Lock()
	m.services[service.role] = service
	m.servicesMutex.Unlock()
	m.registerChannel <- service.role
}

func (m *Manager) deregisterService(service *Service) {
	m.servicesMutex.Lock()
	delete(m.services, service.role)
	m.servicesMutex.Unlock()
	m.deregisterChannel <- service.role
}
