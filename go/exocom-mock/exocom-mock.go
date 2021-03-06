package exocomMock

import (
	"encoding/json"
	"fmt"
	"net/http"
	"sync"

	"github.com/Originate/exocom/go/structs"
	"github.com/Originate/exocom/go/utils"
	"github.com/gorilla/websocket"
	"github.com/pkg/errors"
)

// MockReplyData defines the data needed to mock a reply to a specific request
type MockReplyData struct {
	Name    string
	Payload structs.MessagePayload
}

// ExoComMock is a mock implementation of ExoRelay,
// to be used for testing
type ExoComMock struct {
	receivedMessages      []structs.Message
	receivedMessagesMutex sync.RWMutex
	server                http.Server
	socket                *websocket.Conn
	socketMutex           sync.RWMutex
	mockReplyMapping      map[string]MockReplyData
}

var upgrader = websocket.Upgrader{}

// New creates a new ExoComMock instance
func New() *ExoComMock {
	result := &ExoComMock{
		mockReplyMapping: map[string]MockReplyData{},
		receivedMessages: []structs.Message{},
		server:           http.Server{},
	}
	var handler http.HandlerFunc = func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/services" {
			conn, err := upgrader.Upgrade(w, r, nil)
			if err != nil {
				fmt.Println("Error upgrading request to websocket:", err)
				return
			}
			go result.websocketHandler(conn)
		} else {
			http.Error(w, http.StatusText(http.StatusBadRequest), http.StatusBadRequest)
		}
	}
	result.server.Handler = handler
	return result
}

// AddMockReply adds a mock reply for the given message
func (e *ExoComMock) AddMockReply(requestName string, data MockReplyData) {
	e.mockReplyMapping[requestName] = data
}

// Close takes this ExoComMock instance offline
func (e *ExoComMock) Close() error {
	err := e.CloseConnection()
	if err != nil {
		return err
	}
	return e.server.Close()
}

// Listen brings this ExoComMock instance online
func (e *ExoComMock) Listen(port int) error {
	e.server.Addr = fmt.Sprintf(":%d", port)
	return e.server.ListenAndServe()
}

// GetReceivedMessages returns the received messages
func (e *ExoComMock) GetReceivedMessages() []structs.Message {
	e.receivedMessagesMutex.RLock()
	defer e.receivedMessagesMutex.RUnlock()
	return e.receivedMessages
}

// HasConnection returns whether or not a socket is connected
func (e *ExoComMock) HasConnection() bool {
	e.socketMutex.RLock()
	defer e.socketMutex.RUnlock()
	return e.socket != nil
}

// CloseConnection closes any open connection
func (e *ExoComMock) CloseConnection() error {
	if e.HasConnection() {
		e.socketMutex.Lock()
		err := e.socket.Close()
		if err != nil {
			return err
		}
		e.socket = nil
		e.socketMutex.Unlock()
	}
	return nil
}

// Reset closes and nils the socket and clears all received messages
func (e *ExoComMock) Reset() error {
	err := e.CloseConnection()
	if err != nil {
		return err
	}
	e.receivedMessagesMutex.Lock()
	e.receivedMessages = []structs.Message{}
	e.receivedMessagesMutex.Unlock()
	return nil
}

// Send sends the given message to the connected socket
func (e *ExoComMock) Send(message structs.Message) error {
	if !e.HasConnection() {
		return fmt.Errorf("Nothing connected to exocom")
	}
	serializedBytes, err := json.Marshal(message)
	if err != nil {
		return err
	}
	return e.socket.WriteMessage(websocket.TextMessage, serializedBytes)
}

// WaitForConnection waits for a socket to connect
func (e *ExoComMock) WaitForConnection() (structs.Message, error) {
	err := utils.WaitFor(func() bool {
		return e.HasConnection()
	}, "Expected a socket to connect to exocom")
	if err != nil {
		return structs.Message{}, err
	}
	return e.WaitForMessageWithName("exocom.register-service")
}

// WaitForMessageWithName waits to receive a message with the given name
func (e *ExoComMock) WaitForMessageWithName(name string) (structs.Message, error) {
	return utils.WaitForMessageWithName(e, name)
}

// Helpers

func (e *ExoComMock) websocketHandler(socket *websocket.Conn) {
	e.socketMutex.Lock()
	e.socket = socket
	e.socketMutex.Unlock()
	utils.ListenForMessages(e.socket, func(message structs.Message) error {
		e.receivedMessagesMutex.Lock()
		e.receivedMessages = append(e.receivedMessages, message)
		e.receivedMessagesMutex.Unlock()
		if replyData, hasReplyData := e.mockReplyMapping[message.Name]; hasReplyData {
			return e.Send(structs.Message{
				Name:       replyData.Name,
				ActivityID: message.ActivityID,
				Payload:    replyData.Payload,
			})
		}
		return nil
	}, func(err error) {
		fmt.Println(errors.Wrap(err, "Exocom listening for messages"))
	})
}
