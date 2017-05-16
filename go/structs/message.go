package structs

// Message defines the structure of websocket packets representing messages
type Message struct {
	Name    string
	Payload map[string]interface{}
}
