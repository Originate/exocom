package clientRegistry

import "encoding/json"

// Client is the combination of a client name, service type and internal namespace
type Client struct {
	ClientName        string `json:"clientName"`
	ServiceType       string `json:"serviceType"`
	InternalNamespace string `json:"internalNamespace"`
}

// Route is an entry in the routes
type Route struct {
	Receives          []string `json:"receives"`
	Sends             []string `json:"sends"`
	InternalNamespace string   `json:"internalNamespace"`
}

// ClientRegistry manages which clients are connected to exocom,
// and what messages each client can send and receive
type ClientRegistry struct {
	Routing       map[string]Route
	Clients       map[string]Client
	subscriptions *SubscriptionManager
}

// NewClientRegistry returns a new ClientRegistry with the given routing
func NewClientRegistry(serviceRoutes string) (*ClientRegistry, error) {
	result := new(ClientRegistry)
	result.Clients = map[string]Client{}
	var err error
	result.Routing, err = parseServiceRoutes([]byte(serviceRoutes))
	result.subscriptions = NewSubscriptionManager(result.Routing)
	return result, err
}

// CanSend returns whether or not the client can send a message with the given name
func (r *ClientRegistry) CanSend(clientName, messageName string) bool {
	for _, sendableMessageName := range r.Routing[clientName].Sends {
		if sendableMessageName == messageName {
			return true
		}
	}
	return false
}

// GetSubscribersFor returns all subscribers for the given message name
func (r *ClientRegistry) GetSubscribersFor(messageName string) []Subscriber {
	return r.subscriptions.GetSubscribersFor(messageName)
}

// RegisterClient adds the client with the given name
func (r *ClientRegistry) RegisterClient(clientName string) {
	r.subscriptions.AddAll(clientName)
	r.Clients[clientName] = Client{
		ClientName:        clientName,
		ServiceType:       clientName,
		InternalNamespace: r.Routing[clientName].InternalNamespace,
	}
}

// DeregisterClient removes the client with the given name
func (r *ClientRegistry) DeregisterClient(clientName string) {
	r.subscriptions.RemoveAll(clientName)
	delete(r.Clients, clientName)
}

// Helpers

type rawRoute struct {
	Role      string
	Receives  []string
	Sends     []string
	Namespace string
}

func parseServiceRoutes(bytes []byte) (map[string]Route, error) {
	var unmarshaled []rawRoute
	err := json.Unmarshal(bytes, &unmarshaled)
	if err != nil {
		return map[string]Route{}, err
	}
	parsed := map[string]Route{}
	for _, data := range unmarshaled {
		if data.Sends == nil {
			data.Sends = []string{}
		}
		if data.Receives == nil {
			data.Receives = []string{}
		}
		parsed[data.Role] = Route{
			Receives:          data.Receives,
			Sends:             data.Sends,
			InternalNamespace: data.Namespace,
		}
	}
	return parsed, nil
}
