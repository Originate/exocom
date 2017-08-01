package clientRegistry

// Client is the combination of a client name, service type and internal namespace
type Client struct {
	Role              string `json:"role"`
	ServiceType       string `json:"serviceType"`
	InternalNamespace string `json:"internalNamespace"`
}

// Clients is a map from client name to Client
type Clients map[string]Client

// Route is an entry in the routes
type Route struct {
	Receives          []string `json:"receives"`
	Sends             []string `json:"sends"`
	InternalNamespace string   `json:"internalNamespace"`
}

// Routes is a map from client name to Route
type Routes map[string]Route

// ClientRegistry manages which clients are connected to exocom,
// and what messages each client can send and receive
type ClientRegistry struct {
	Routing       Routes
	Clients       Clients
	subscriptions *SubscriptionManager
}

// NewClientRegistry returns a new ClientRegistry with the given routing
func NewClientRegistry(routes Routes) *ClientRegistry {
	result := new(ClientRegistry)
	result.Clients = Clients{}
	result.Routing = routes
	result.subscriptions = NewSubscriptionManager(routes)
	return result
}

// CanSend returns whether or not the client can send a message with the given name
func (r *ClientRegistry) CanSend(role, messageName string) bool {
	for _, sendableMessageName := range r.Routing[role].Sends {
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
func (r *ClientRegistry) RegisterClient(role string) {
	r.subscriptions.AddAll(role)
	r.Clients[role] = Client{
		Role:              role,
		ServiceType:       role,
		InternalNamespace: r.Routing[role].InternalNamespace,
	}
}

// DeregisterClient removes the client with the given name
func (r *ClientRegistry) DeregisterClient(role string) {
	r.subscriptions.RemoveAll(role)
	delete(r.Clients, role)
}
