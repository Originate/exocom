package exocomMock

import (
	"encoding/json"
	"fmt"
	"io"
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

// websocketHandler is called when the given socket connects
func (exoCom *ExoComMock) websocketHandler(socket *websocket.Conn) {
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

// Reset clears all received messages
func (exoCom *ExoComMock) Reset() {
	exoCom.ReceivedMessages = []structs.Message{}
}

// WaitForReceivedMessageCount blocks until this instance has received the given number of messages
func (exoCom *ExoComMock) WaitForReceivedMessageCount(count int) error {
	success := make(chan bool, 1)

	go func() {
		for {
			time.Sleep(time.Millisecond)
			if len(exoCom.ReceivedMessages) >= count {
				success <- true
				break
			}
		}
	}()

	select {
	case <-success:
		return nil
	case <-time.After(time.Second * 10):
		return fmt.Errorf("Wait for messages timed out")
	}
}
