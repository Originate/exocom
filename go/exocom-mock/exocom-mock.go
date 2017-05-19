package exocomMock

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"

	"github.com/Originate/exocom/go/structs"
	"golang.org/x/net/websocket"
)

// ExoComMock is a mock implementation of ExoRelay,
// to be used for testing
type ExoComMock struct {
	ReceivedMessages []structs.Message
	server           http.Server
	socket           *websocket.Conn
}

// New creates a new ExoComMock instance
func New() *ExoComMock {
	result := new(ExoComMock)
	result.server = http.Server{
		Handler: websocket.Handler(result.websocketHandler),
	}
	return result
}

// Close takes this ExoComMock instance offline
func (exoCom *ExoComMock) Close() error {
	return exoCom.server.Close()
}

// Listen brings this ExoComMock instance online
func (exoCom *ExoComMock) Listen(port int) error {
	exoCom.server.Addr = fmt.Sprintf(":%d", port)
	return exoCom.server.ListenAndServe()
}

// HasConnection returns whether or not a socket is connected
func (exoCom *ExoComMock) HasConnection() bool {
	return exoCom.socket != nil
}

// Reset nils the socket and clears all received messages
func (exoCom *ExoComMock) Reset() {
	exoCom.socket = nil
	exoCom.ReceivedMessages = []structs.Message{}
}

// Send sends the given message to the connected socket
func (exoCom *ExoComMock) Send(message structs.Message) error {
	if !exoCom.HasConnection() {
		return fmt.Errorf("Nothing connected to exocom")
	}
	serializedBytes, err := json.Marshal(message)
	if err != nil {
		return err
	}
	return websocket.Message.Send(exoCom.socket, serializedBytes)
}

// Helpers

func (exoCom *ExoComMock) readMessage(socket *websocket.Conn) (structs.Message, error) {
	var bytes []byte
	if err := websocket.Message.Receive(socket, &bytes); err != nil {
		return structs.Message{}, err
	}

	var unmarshaled structs.Message
	err := json.Unmarshal(bytes, &unmarshaled)
	if err != nil {
		return structs.Message{}, err
	}

	return unmarshaled, nil
}

// websocketHandler is called when the given socket connects
func (exoCom *ExoComMock) websocketHandler(socket *websocket.Conn) {
	exoCom.socket = socket
	for {
		message, err := exoCom.readMessage(socket)
		if err == io.EOF {
			break
		} else if err != nil {
			fmt.Println("Error reading message from websocket:", err)
			break
		} else {
			exoCom.ReceivedMessages = append(exoCom.ReceivedMessages, message)
		}
	}
}
