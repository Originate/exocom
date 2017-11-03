package frontendbridge

import (
	"github.com/Originate/exocom/go/structs"

	"github.com/gorilla/websocket"
)

// Connection represents a connection
type Connection struct {
	socket *websocket.Conn
	auth   structs.MessageAuth
}
