package exocomMock

import (
	"encoding/json"
	"fmt"
	"net/http"
	"time"

	"github.com/Originate/exocom/go/structs"
	"golang.org/x/net/websocket"
)

// ExoComMock is a mock implementation of ExoRelay,
// to be used for testing
type ExoComMock struct {
	ReceivedMessages []structs.Message
	server           http.Server
}

// New creates a new ExoComMock instance
func New() *ExoComMock {
	result := new(ExoComMock)
	result.server = http.Server{
		Handler: websocket.Handler(result.messageHandler),
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

// messageHandler is called when the given socket receives a message.
// It logs the received message.
func (exoCom *ExoComMock) messageHandler(socket *websocket.Conn) {
	message, err := exoCom.readMessage(socket)
	if err != nil {
		panic(err)
	}
	exoCom.ReceivedMessages = append(exoCom.ReceivedMessages, message)
}

// readMessage reads the next message from the given socket
// and parses it into a Message struct
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

// WaitForReceivedMessage blocks until this instance receives the given message
func (exoCom *ExoComMock) WaitForReceivedMessage() (structs.Message, error) {
	c1 := make(chan structs.Message, 1)
	numMessages := len(exoCom.ReceivedMessages)

	go func() {
		for {
			time.Sleep(time.Millisecond)
			if len(exoCom.ReceivedMessages) > numMessages {
				c1 <- exoCom.ReceivedMessages[len(exoCom.ReceivedMessages)-1]
				break
			}
		}
	}()

	select {
	case res := <-c1:
		return res, nil
	case <-time.After(time.Second * 10):
		return structs.Message{}, fmt.Errorf("Wait for messages timed out")
	}

}
