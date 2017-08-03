package routing

import (
	"github.com/Originate/exocom/go/exocom/src/translation"
	"github.com/Originate/exocom/go/exocom/src/types"
	"github.com/Originate/exocom/go/structs"
)

// Manager manages what roles should be sent messages
type Manager struct {
	messageNameToReceiverMapping map[string]types.ReceiverMapping
	routes                       types.Routes
}

// NewManager returns a Manager for the given routes
func NewManager(routes types.Routes) *Manager {
	result := &Manager{
		messageNameToReceiverMapping: map[string]types.ReceiverMapping{},
		routes: routes,
	}
	result.intitializeMessageNameToReceiverMapping()
	return result
}

// CanSend returns whether or not a service with the given role can send a
// message with the given name
func (m *Manager) CanSend(role, messageName string) bool {
	for _, sendableMessageName := range m.routes[role].Sends {
		if sendableMessageName == messageName {
			return true
		}
	}
	return false
}

// GetRoutes returns the managers routes
func (m *Manager) GetRoutes() types.Routes {
	return m.routes
}

// GetSubscribersFor returns a receiver mapping for the given mapping
func (m *Manager) GetSubscribersFor(message structs.Message) types.ReceiverMapping {
	publicMessageName := translation.GetPublicMessageName(&translation.GetPublicMessageNameOptions{
		Namespace:           m.routes[message.Sender].InternalNamespace,
		Role:                message.Sender,
		InternalMessageName: message.Name,
	})
	roles, hasKey := m.messageNameToReceiverMapping[publicMessageName]
	if hasKey {
		return roles
	}
	return types.ReceiverMapping{}
}

// Helpers

func (m *Manager) intitializeMessageNameToReceiverMapping() {
	for role, route := range m.routes {
		for _, messageName := range route.Receives {
			publicMessageName := translation.GetPublicMessageName(&translation.GetPublicMessageNameOptions{
				InternalMessageName: messageName,
				Role:                role,
				Namespace:           route.InternalNamespace,
			})
			internalMessageName := translation.GetInternalMessageName(&translation.GetInternalMessageNameOptions{
				Namespace:         m.routes[role].InternalNamespace,
				PublicMessageName: publicMessageName,
			})
			if m.messageNameToReceiverMapping[publicMessageName] == nil {
				m.messageNameToReceiverMapping[publicMessageName] = types.ReceiverMapping{}
			}
			m.messageNameToReceiverMapping[publicMessageName][role] = internalMessageName
		}
	}
}
