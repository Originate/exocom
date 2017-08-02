package frontendbridge

import (
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/structs"
	"github.com/Originate/exocom/go/utils"
	"github.com/pkg/errors"
	uuid "github.com/satori/go.uuid"

	"github.com/gorilla/websocket"
)

var upgrader = websocket.Upgrader{}

// FrontendBridge handles communication between an Exosphere backend and
// a browser front-end using websockets
type FrontendBridge struct {
	server    http.Server
	socketMap map[string]*websocket.Conn
	exoRelay  exorelay.ExoRelay
	open      bool
}

// Open sets up connection to exocom and a server to listen for connections from clients
func (w *FrontendBridge) Open(config exorelay.Config, clientPort int) error {
	err := w.connectToExocom(config)
	if err != nil {
		return err
	}
	err = w.listenForClients(clientPort)
	if err != nil {
		return err
	}
	w.open = true
	return nil
}

// ConnectToExocom sets up an ExoRelay to communicate with exocom
func (w *FrontendBridge) connectToExocom(config exorelay.Config) error {
	w.exoRelay = exorelay.ExoRelay{Config: config}
	err := w.exoRelay.Connect()
	if err != nil {
		return err
	}
	go w.listenForExocomMessages()
	return nil
}

func (w *FrontendBridge) listenForExocomMessages() {
	messageChannel := w.exoRelay.GetMessageChannel()
	for {
		message, ok := <-messageChannel
		if !ok {
			break // channel closed
		}
		socket := w.socketMap[message.SessionID]
		if socket == nil {
			delete(w.socketMap, message.SessionID)
			continue
		}
		sessionID := message.SessionID
		message.SessionID = ""
		serializedBytes, err := json.Marshal(message)
		if err != nil {
			fmt.Printf("Error serializing message for session %s: %s", sessionID, err.Error())
			continue
		}
		err = socket.WriteMessage(websocket.TextMessage, serializedBytes)
		if err != nil {
			fmt.Printf("Error sending message for session %s: %s", sessionID, err.Error())
			continue
		}
	}
}

// ListenForClients sets up a server to listen for incoming websocket connections
func (w *FrontendBridge) listenForClients(clientPort int) error {
	var handler http.HandlerFunc = func(resWriter http.ResponseWriter, req *http.Request) {
		conn, err := upgrader.Upgrade(resWriter, req, nil)
		if err != nil {
			fmt.Println("Error upgrading request to websocket:", err)
			return
		}
		w.websocketHandler(conn)
	}
	w.server = http.Server{
		Handler: handler,
		Addr:    fmt.Sprintf(":%d", clientPort),
	}
	w.socketMap = make(map[string]*websocket.Conn)
	go w.server.ListenAndServe()
	return nil
}

// Close closes the server, all the open sockets, and nils the socketMap
func (w *FrontendBridge) Close() error {
	for _, socket := range w.socketMap {
		if socket != nil {
			err := socket.Close()
			if err != nil {
				return err
			}
		}
	}
	err := w.server.Close()
	if err != nil {
		return err
	}
	err = w.exoRelay.Close()
	if err != nil {
		return err
	}
	w.socketMap = nil
	w.open = false
	return nil
}

func (w *FrontendBridge) websocketHandler(socket *websocket.Conn) {
	sessionID := uuid.NewV4().String()
	w.socketMap[sessionID] = socket
	utils.ListenForMessages(socket, func(message structs.Message) error {
		_, err := w.exoRelay.Send(exorelay.MessageOptions{
			Name:       message.Name,
			Payload:    message.Payload,
			ActivityID: message.ActivityID,
			SessionID:  sessionID,
		})
		if err != nil {
			return errors.Wrap(err, "Error sending message to websocket:")
		}
		return nil
	}, func(err error) {
		fmt.Println(errors.Wrap(err, "FrontendBridge listening for messages"))
	})
	delete(w.socketMap, sessionID)
	err := socket.Close()
	if err != nil && w.open {
		fmt.Println("Error closing websocket:", err)
	}
}
