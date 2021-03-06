package frontendbridge

import (
	"encoding/json"
	"fmt"
	"net/http"
	"reflect"
	"sync"

	"github.com/Originate/exocom/go/exosocket"
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
	connections      []*Connection
	connectionsMutex sync.RWMutex
	exoSocket        *exosocket.ExoSocket
	exoSocketMutex   sync.Mutex
	open             bool
	openMutex        sync.RWMutex
	server           http.Server
}

// NewFrontendBridge returns a FrontendBridge instance
func NewFrontendBridge(config exosocket.Config, messageBufferSize int, clientPort string) *FrontendBridge {
	w := &FrontendBridge{
		exoSocket:   exosocket.NewExoSocket(config, messageBufferSize),
		connections: []*Connection{},
	}
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
		Addr:    fmt.Sprintf(":%s", clientPort),
	}
	return w
}

// Open sets up connection to exocom and a server to listen for connections from clients
func (w *FrontendBridge) Open() error {
	err := w.exoSocket.Connect()
	if err != nil {
		return err
	}
	go w.listenForExocomMessages()
	go w.listenForClients()
	w.open = true
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
	w.connectionsMutex.RLock()
	var result *Connection
	for _, connection := range w.connections {
		if reflect.DeepEqual(auth, connection.auth) {
			result = connection
			break
		}
	}
	w.connectionsMutex.RUnlock()
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
	messageChannel := w.exoSocket.GetMessageChannel()
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
func (w *FrontendBridge) listenForClients() {
	err := w.server.ListenAndServe()
	if err != nil && err.Error() != "http: Server closed" {
		fmt.Println("Error listening:", err)
	}
}

// Close closes the server, all the open sockets, and nils the socketMap
func (w *FrontendBridge) Close() error {
	w.connectionsMutex.Lock()
	for _, connection := range w.connections {
		if connection.socket != nil {
			err := connection.socket.Close()
			if err != nil {
				return err
			}
		}
	}
	w.connections = nil
	w.connectionsMutex.Unlock()
	err := w.server.Close()
	if err != nil {
		return err
	}
	err = w.exoSocket.Close()
	if err != nil {
		return err
	}
	w.openMutex.Lock()
	w.open = false
	w.openMutex.Unlock()
	return nil
}

func (w *FrontendBridge) websocketHandler(socket *websocket.Conn) {
	auth := map[string]interface{}{"sessionId": uuid.NewV4().String()}
	w.addConnection(auth, socket)
	utils.ListenForMessages(socket, func(message structs.Message) error {
		w.exoSocketMutex.Lock()
		_, err := w.exoSocket.Send(exosocket.MessageOptions{
			Name:       message.Name,
			Payload:    message.Payload,
			ActivityID: message.ActivityID,
			Auth:       auth,
		})
		w.exoSocketMutex.Unlock()
		if err != nil {
			return errors.Wrap(err, "Error sending message to websocket:")
		}
		return nil
	}, func(err error) {
		fmt.Println(errors.Wrap(err, "FrontendBridge listening for messages"))
	})
	w.removeConnection(auth)
	err := socket.Close()
	if err != nil {
		w.openMutex.RLock()
		if w.open {
			fmt.Println("Error closing websocket:", err)
		}
		w.openMutex.RUnlock()
	}
}
