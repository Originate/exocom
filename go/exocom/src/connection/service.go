package connection

import (
	"encoding/json"
	"fmt"

	"github.com/Originate/exocom/go/structs"
	"github.com/Originate/exocom/go/utils"
	"github.com/gorilla/websocket"
	"github.com/pkg/errors"
)

// Service manages a single websocket connection
type Service struct {
	manager *Manager
	role    string
	socket  *websocket.Conn
}

// BootstrapService creates a new Service which will alert the manager when the
// service registers (by sending its first message) and deregisters (by disconnecting)
func BootstrapService(manager *Manager, socket *websocket.Conn) {
	service := Service{
		manager: manager,
		socket:  socket,
	}
	go service.listen()
}

// Send sends the given message on the websocket
func (s *Service) Send(message structs.Message) error {
	serializedBytes, err := json.Marshal(message)
	if err != nil {
		return err
	}
	return s.socket.WriteMessage(websocket.TextMessage, serializedBytes)
}

// Helpers

func (s *Service) handleRegisterMessage(message structs.Message) {
	var err error
	s.role, err = parseRegisterMessagePayload(message)
	if err == nil {
		s.manager.registerService(s)
	} else {
		s.manager.logError(err)
	}
}

func (s *Service) listen() {
	utils.ListenForMessages(s.socket, func(message structs.Message) error {
		if message.Name == "exocom.register-service" {
			s.handleRegisterMessage(message)
		} else {
			s.manager.onMessage(message)
		}
		return nil
	}, func(err error) {
		fmt.Println(errors.Wrap(err, "Exocom listening for messages"))
	})
	if s.role != "" {
		s.manager.deregisterService(s)
	}
}

func parseRegisterMessagePayload(message structs.Message) (string, error) {
	if objectPayload, ok := message.Payload.(map[string]interface{}); ok {
		if role, ok := objectPayload["clientName"].(string); ok {
			return role, nil
		}
	}
	return "", fmt.Errorf("Invalid register message payload: %v", message.Payload)
}
