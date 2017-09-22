package connection

import (
	"fmt"
	"sync"
	"time"

	"github.com/Originate/exocom/go/structs"
	"github.com/gorilla/websocket"
)

// Manager manages all websocket connections
type Manager struct {
	deregisterChannel           chan string
	errorChannel                chan error
	messageChannel              chan structs.Message
	registerChannel             chan string
	servicesByRoleAndID         NestedServiceMapping
	servicesByRoleAndActivityID NestedServiceMapping
	servicesMutex               sync.RWMutex
	activityIDCache             map[string]time.Time
}

// NewManager returns a new Manager
func NewManager(options ManagerOptions) *Manager {
	manager := &Manager{
		deregisterChannel:           options.DeregisterChannel,
		errorChannel:                options.ErrorChannel,
		messageChannel:              options.MessageChannel,
		registerChannel:             options.RegisterChannel,
		servicesByRoleAndID:         NestedServiceMapping{},
		servicesByRoleAndActivityID: NestedServiceMapping{},
		activityIDCache:             map[string]time.Time{},
	}
	manager.startActivityIDCleanup(options.ActivityIDCleanupInterval)
	return manager
}

// AddWebsocket adds the websocket under the manager's control
func (m *Manager) AddWebsocket(socket *websocket.Conn) {
	BootstrapService(m, socket)
}

// GetClients returns all the connections
func (m *Manager) GetClients() (result []Client) {
	m.servicesMutex.RLock()
	for role, roleMapping := range m.servicesByRoleAndID {
		instances := len(roleMapping)
		if instances != 0 {
			result = append(result, Client{Role: role, Instances: instances})
		}
	}
	m.servicesMutex.RUnlock()
	return
}

// SendMessage sends the given message to the service with the given role
func (m *Manager) SendMessage(role string, message structs.Message) error {
	service := m.getServiceToSendMessageTo(role, message)
	if service == nil {
		return fmt.Errorf("No connected service for role '%s'", role)
	}
	return service.Send(message)
}

// Helpers

func (m *Manager) logError(err error) {
	m.errorChannel <- err
}

func (m *Manager) onMessage(service *Service, message structs.Message) {
	m.servicesMutex.Lock()
	m.servicesByRoleAndActivityID.Set(service.role, message.ActivityID, service)
	m.activityIDCache[message.ActivityID] = time.Now()
	m.servicesMutex.Unlock()
	m.messageChannel <- message
}

func (m *Manager) registerService(service *Service) {
	m.servicesMutex.Lock()
	m.servicesByRoleAndID.Set(service.role, service.id, service)
	m.servicesMutex.Unlock()
	m.registerChannel <- service.role
}

func (m *Manager) deregisterService(service *Service) {
	m.servicesMutex.Lock()
	m.servicesByRoleAndActivityID.DeleteForService(service.role, service)
	m.servicesByRoleAndID.Delete(service.role, service.id)
	m.servicesMutex.Unlock()
	m.deregisterChannel <- service.role
}

func (m *Manager) getServiceToSendMessageTo(role string, message structs.Message) *Service {
	m.servicesMutex.Lock()
	m.activityIDCache[message.ActivityID] = time.Now()
	service := m.servicesByRoleAndActivityID.Get(role, message.ActivityID)
	m.servicesMutex.Unlock()
	if service != nil {
		return service
	}
	m.servicesMutex.RLock()
	service = m.servicesByRoleAndID.GetRandom(role)
	m.servicesMutex.RUnlock()
	if service != nil {
		m.servicesMutex.Lock()
		m.servicesByRoleAndActivityID.Set(service.role, message.ActivityID, service)
		m.servicesMutex.Unlock()
	}
	return service
}

func (m *Manager) startActivityIDCleanup(cleanupInterval time.Duration) {
	go func() {
		for {
			time.Sleep(cleanupInterval)
			m.clearCache(cleanupInterval)
		}
	}()
}

func (m *Manager) clearCache(cleanupInterval time.Duration) {
	m.servicesMutex.Lock()
	keysToDelete := []string{}
	for activityID, timestamp := range m.activityIDCache {
		if time.Since(timestamp) > cleanupInterval {
			keysToDelete = append(keysToDelete, activityID)
		}
	}
	for _, ID := range keysToDelete {
		m.servicesByRoleAndActivityID.DeleteNestedKey(ID)
		delete(m.activityIDCache, ID)
	}
	m.servicesMutex.Unlock()
}
