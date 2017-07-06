package utils

import (
	"encoding/json"
	"net"

	"github.com/Originate/exocom/go/structs"
	"github.com/gorilla/websocket"
	"github.com/pkg/errors"
)

// ListenForMessages continuously reads from the given websocket calling the
// given function on each received message
func ListenForMessages(socket *websocket.Conn, onMessage func(structs.Message) error, onError func(error)) {
	for {
		_, bytes, err := socket.ReadMessage()
		if err != nil {
			if opErr, ok := err.(*net.OpError); ok {
				if opErr.Err.Error() == "use of closed network connection" {
					return
				}
			}
			if websocket.IsCloseError(err, websocket.CloseAbnormalClosure) {
				return
			}
			onError(errors.Wrap(err, "Error reading from socket"))
		}
		message, err := messageFromJSON(bytes)
		if err != nil {
			onError(errors.Wrap(err, "Error unmarshaling message"))
		}
		err = onMessage(message)
		if err != nil {
			onError(errors.Wrap(err, "Error returned by on message handler"))
		}
	}
}

// Helpers

func messageFromJSON(bytes []byte) (structs.Message, error) {
	var unmarshaled structs.Message
	err := json.Unmarshal(bytes, &unmarshaled)
	if err != nil {
		return structs.Message{}, err
	}

	return unmarshaled, nil
}
