package structs

import "time"

// Message defines the structure of websocket packets representing messages
type Message struct {
	ActivityID   string         `json:"activityId"`
	Auth         MessageAuth    `json:"auth"`
	ID           string         `json:"id"`
	IsSecurity   bool           `json:"isSecurity"`
	Name         string         `json:"name"`
	Payload      MessagePayload `json:"payload"`
	ResponseTime time.Duration  `json:"responseTime"`
	Sender       string         `json:"sender"`
	Timestamp    time.Time      `json:"timestamp"`
}

// MessagePayload defines the structure of Message.Payload
type MessagePayload interface{}

// MessageAuth defines the structure of Message.Auth
type MessageAuth interface{}
