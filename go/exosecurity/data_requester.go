package exosecurity

import "github.com/Originate/exocom/go/structs"

// DataRequester is a function which sends a message and returns the first response
type DataRequester func(*structs.Message) *structs.Message
