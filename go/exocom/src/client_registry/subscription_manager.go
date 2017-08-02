package clientRegistry

import "github.com/Originate/exocom/go/exocom/src/translation"

// Subscriber is the combination of a client name and internal namesapce
type Subscriber struct {
	Role              string
	InternalNamespace string
}

// SubscriptionManager manages what clients should be sent messages
type SubscriptionManager struct {
	MessageNameToSubscribers map[string][]Subscriber
	Routing                  Routes
}

// NewSubscriptionManager returns a SubscriptionManager for the given routes
func NewSubscriptionManager(routes Routes) *SubscriptionManager {
	result := new(SubscriptionManager)
	result.MessageNameToSubscribers = map[string][]Subscriber{}
	result.Routing = routes
	return result
}

// GetSubscribersFor returns all subscribers for the given message name
func (s *SubscriptionManager) GetSubscribersFor(messageName string) []Subscriber {
	subscribers, hasKey := s.MessageNameToSubscribers[messageName]
	if hasKey {
		return subscribers
	}
	return []Subscriber{}
}

// AddAll adds subscriptions for all messages the given client receives
// (as defined in the routes)
func (s *SubscriptionManager) AddAll(role string) {
	for _, messageName := range s.Routing[role].Receives {
		s.add(messageName, role)
	}
}

// RemoveAll removes subscriptions for all messages the given client receives
// (as defined in the routes)
func (s *SubscriptionManager) RemoveAll(role string) {
	for _, messageName := range s.Routing[role].Receives {
		s.remove(messageName, role)
	}
}

// Helpers

func (s *SubscriptionManager) add(internalMessageName, role string) {
	clientInternalNamespace := s.Routing[role].InternalNamespace
	publicMessageName := translation.GetPublicMessageName(&translation.GetPublicMessageNameOptions{
		InternalMessageName: internalMessageName,
		Role:                role,
		Namespace:           clientInternalNamespace,
	})
	if s.MessageNameToSubscribers[publicMessageName] == nil {
		s.MessageNameToSubscribers[publicMessageName] = []Subscriber{}
	}
	s.MessageNameToSubscribers[publicMessageName] = append(
		s.MessageNameToSubscribers[publicMessageName],
		Subscriber{Role: role, InternalNamespace: clientInternalNamespace})
}

func (s *SubscriptionManager) remove(messageName, role string) {
	clientInternalNamespace := s.Routing[role].InternalNamespace
	publicMessageName := translation.GetPublicMessageName(&translation.GetPublicMessageNameOptions{
		InternalMessageName: messageName,
		Role:                role,
		Namespace:           clientInternalNamespace,
	})
	if s.MessageNameToSubscribers[publicMessageName] == nil {
		return
	}
	for index, subscription := range s.MessageNameToSubscribers[publicMessageName] {
		if subscription.Role == role {
			s.MessageNameToSubscribers[publicMessageName] = append(s.MessageNameToSubscribers[publicMessageName][:index], s.MessageNameToSubscribers[publicMessageName][index+1:]...)
			return
		}
	}
}
