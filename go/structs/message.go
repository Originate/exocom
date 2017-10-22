package structs

import (
	"encoding/json"
	"time"
)

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

// GetPayloadAsMessage returns the payload converted to a Message
func (m *Message) GetPayloadAsMessage() (*Message, error) {
	output := &Message{}
	bytes, err := json.Marshal(m.Payload)
	if err != nil {
		return nil, err
	}
	err = json.Unmarshal(bytes, output)
	if err != nil {
		return nil, err
	}
	return output, nil
}
