package exosecurity

import "github.com/Originate/exocom/go/structs"

// MessageAuthorizer is a function which returns whether or not a message is authorized
type MessageAuthorizer func(*structs.Message, DataRequester) bool
