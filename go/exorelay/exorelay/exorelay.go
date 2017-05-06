package exorelay

import (
	"errors"
	"fmt"
	// HandlerManager
	// MessageSender
	// ZmqListener
)

type Relay struct {
	ServiceName string
	ExocomHost  string
	ExocomPort  int
}

func NewRelay(ServiceName, ExocomHost string, ExocomPort int) *Relay {
	return &Relay{ServiceName, ExocomHost, ExocomPort}
}

func (relay *Relay) Send(message string) error {
	fmt.Println("Message: " + message)
	return errors.New("Send is unimplemented\n")
}

func (relay *Relay) OnIncomingMessage(request string) (string, error) {
	fmt.Println("OnIncomingMessage is unimplemented")
	return "error", errors.New("OnIncomingMessage is unimplemented\n")
}

func (relay *Relay) SendRoutingConfig() error {
	fmt.Println("SendRoutingConfig is unimplemented")
	return errors.New("SendRoutingConfig is unimplemented\n")
}
