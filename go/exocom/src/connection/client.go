package connection

// Client is the combination of a role and service type
type Client struct {
	Role        string `json:"role"`
	ServiceType string `json:"serviceType"`
}
