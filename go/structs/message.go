package structs

// Message defines the structure of websocket packets representing messages
type Message struct {
	ID      string                 `json:"id"`
	Name    string                 `json:"name"`
	Payload map[string]interface{} `json:"payload"`
	Sender  string                 `json:"sender"`
}
