package exosocket

import (
	"github.com/Originate/exocom/go/structs"
)

// MessageOptions contains the user input values for a message
type MessageOptions struct {
	Name       string
	Payload    structs.MessagePayload
	ActivityID string
	Auth       structs.MessageAuth
	IsSecurity bool
}
