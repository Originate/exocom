package types

// Route is an entry in the routes
type Route struct {
	Receives          []string `json:"receives"`
	Sends             []string `json:"sends"`
	InternalNamespace string   `json:"internalNamespace"`
}
