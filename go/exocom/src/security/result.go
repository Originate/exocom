package security

import "github.com/Originate/exocom/go/structs"

// Result is the result of a secuirty manager parsing a
// given message
type Result struct {
	MessageToSend  *structs.Message
	WarningMessage string
}
