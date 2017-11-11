package exorelay

import (
	"time"

	"github.com/Originate/exocom/go/structs"
)

// Request contains payload and helper functions for sending messages
type Request struct {
	Message         structs.Message
	Reply           func(name string, payload structs.MessagePayload) error
	Send            func(name string, payload structs.MessagePayload) (*structs.Message, error)
	WaitForActivity func(string, time.Duration) (*structs.Message, error)
}
