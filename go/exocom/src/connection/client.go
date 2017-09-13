package connection

// Client represents a service connected to exocom
type Client struct {
	Role      string `json:"role"`
	Instances int    `json:"instances"`
}
