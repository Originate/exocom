package connection

import "github.com/Originate/exocom/go/structs"

// ManagerOptions is the options passed to NewManager
type ManagerOptions struct {
	DeregisterChannel chan string
	ErrorChannel      chan error
	MessageChannel    chan structs.Message
	RegisterChannel   chan string
}
