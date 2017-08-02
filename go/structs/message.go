package structs

import "time"

// Message defines the structure of websocket packets representing messages
type Message struct {
	ActivityID   string         `json:"activityId"`
	ID           string         `json:"id"`
	Name         string         `json:"name"`
	Payload      MessagePayload `json:"payload"`
	ResponseTime time.Duration  `json:"responseTime"`
	Sender       string         `json:"sender"`
	SessionID    string         `json:"sessionId"`
	Timestamp    time.Time      `json:"timestamp"`
}

// MessagePayload defines the structure of Message.Payload
type MessagePayload interface{}
