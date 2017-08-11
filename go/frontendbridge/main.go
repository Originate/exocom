package frontendbridge

import (
	"encoding/json"
	"fmt"
	"net/http"
	"reflect"
	"sync"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/structs"
	"github.com/Originate/exocom/go/utils"
	"github.com/pkg/errors"
	uuid "github.com/satori/go.uuid"

	"github.com/gorilla/websocket"
)

var upgrader = websocket.Upgrader{}

// Connection represents a connection
type Connection struct {
	socket *websocket.Conn
	auth   structs.MessageAuth
}

// FrontendBridge handles communication between an Exosphere backend and
// a browser front-end using websockets
type FrontendBridge struct {
	server           http.Server
	connectionsMutex sync.Mutex
	connections      []*Connection
	exoRelay         exorelay.ExoRelay
	open             bool
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

func (w *FrontendBridge) addConnection(auth structs.MessageAuth, socket *websocket.Conn) {
	w.connectionsMutex.Lock()
	w.connections = append(w.connections, &Connection{
		auth:   auth,
		socket: socket,
	})
	w.connectionsMutex.Unlock()
}

func (w *FrontendBridge) getConnection(auth structs.MessageAuth) *Connection {
	w.connectionsMutex.Lock()
	var result *Connection
	for _, connection := range w.connections {
		if reflect.DeepEqual(auth, connection.auth) {
			result = connection
			break
		}
	}
	w.connectionsMutex.Unlock()
	return result
}

func (w *FrontendBridge) removeConnection(auth structs.MessageAuth) {
	w.connectionsMutex.Lock()
	for i, connection := range w.connections {
		if reflect.DeepEqual(auth, connection.auth) {
			w.connections = append(w.connections[:i-1], w.connections[i+1:]...)
			break
		}
	}
	w.connectionsMutex.Unlock()
}

func (w *FrontendBridge) listenForExocomMessages() {
	messageChannel := w.exoRelay.GetMessageChannel()
	for {
		message, ok := <-messageChannel
		if !ok {
			break // channel closed
		}
		connection := w.getConnection(message.Auth)
		if connection == nil {
			continue
		}
		auth := message.Auth
		message.Auth = nil
		serializedBytes, err := json.Marshal(message)
		if err != nil {
			fmt.Printf("Error serializing message for auth %s: %s", auth, err.Error())
			continue
		}
		err = connection.socket.WriteMessage(websocket.TextMessage, serializedBytes)
		if err != nil {
			fmt.Printf("Error sending message for auth %s: %s", auth, err.Error())
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
	w.connections = []*Connection{}
	w.connectionsMutex = sync.Mutex{}
	go w.server.ListenAndServe()
	return nil
}

// Close closes the server, all the open sockets, and nils the socketMap
func (w *FrontendBridge) Close() error {
	for _, connection := range w.connections {
		if connection.socket != nil {
			err := connection.socket.Close()
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
	w.connections = nil
	w.open = false
	return nil
}

func (w *FrontendBridge) websocketHandler(socket *websocket.Conn) {
	auth := map[string]interface{}{"sessionId": uuid.NewV4().String()}
	w.addConnection(auth, socket)
	utils.ListenForMessages(socket, func(message structs.Message) error {
		_, err := w.exoRelay.Send(exorelay.MessageOptions{
			Name:       message.Name,
			Payload:    message.Payload,
			ActivityID: message.ActivityID,
			Auth:       auth,
		})
		if err != nil {
			return errors.Wrap(err, "Error sending message to websocket:")
		}
		return nil
	}, func(err error) {
		fmt.Println(errors.Wrap(err, "FrontendBridge listening for messages"))
	})
	w.removeConnection(auth)
	err := socket.Close()
	if err != nil && w.open {
		fmt.Println("Error closing websocket:", err)
	}
}
