package exosecurity

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"strconv"
	"sync"
	"time"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/structs"
)

// ExoSecurity is the exosecurity
type ExoSecurity struct {
	messageAuthorizer  MessageAuthorizer
	closed             bool
	exoRelay           *exorelay.ExoRelay
	replyChannels      ReplyChannelMapping
	replyChannelsMutex sync.RWMutex
}

// ReplyChannelMapping is a map from message activityID to a channel that should be sent messages
type ReplyChannelMapping map[string]chan *structs.Message

// Bootstrap brings an exosecurity online using environment variables and the
// given messageauthorizer
func Bootstrap(messageAuthorizer MessageAuthorizer) {
	port, err := strconv.Atoi(os.Getenv("EXOCOM_PORT"))
	if err != nil {
		panic(fmt.Sprintf("Invalid port: %s", os.Getenv("EXOCOM_PORT")))
	}
	config := exorelay.Config{
		Host: os.Getenv("EXOCOM_HOST"),
		Port: port,
		Role: os.Getenv("ROLE"),
	}
	security := ExoSecurity{}
	err = security.Connect(config)
	if err != nil {
		panic(fmt.Sprintf("Error connecting: %v", err))
	}
	fmt.Println("online at port", port)
	err = security.ListenForMessages(messageAuthorizer)
	if err != nil {
		panic(fmt.Sprintf("Error listening for messages: %v", err))
	}
}

// Close closes the security socket
func (e *ExoSecurity) Close() error {
	if e.closed {
		return nil
	}
	err := e.exoRelay.Close()
	e.closed = true
	return err
}

// Connect connects the exosecurtiy instance
func (e *ExoSecurity) Connect(config exorelay.Config) error {
	e.exoRelay = &exorelay.ExoRelay{Config: config}
	err := e.exoRelay.Connect()
	e.closed = false
	return err
}

//ListenForMessages listens for messages
func (e *ExoSecurity) ListenForMessages(messageAuthorizer MessageAuthorizer) error {
	if e.exoRelay == nil {
		return errors.New("Please call Connect() first")
	}
	e.messageAuthorizer = messageAuthorizer
	e.replyChannels = map[string]chan *structs.Message{}
	messageChannel := e.exoRelay.GetMessageChannel()
	for {
		message, ok := <-messageChannel
		if !ok {
			return nil
		}
		bytes, err := json.Marshal(message.Payload)
		if err != nil {
			return err
		}
		payload := structs.Message{}
		err = json.Unmarshal(bytes, &payload)
		if err != nil {
			return err
		}

		if message.Name == "authorize message" {
			go e.receiveMessage(payload, message.ActivityID)
		} else if message.Name == "security response" {
			e.replyChannelsMutex.RLock()
			replyChannel, ok := e.replyChannels[message.ActivityID]
			e.replyChannelsMutex.RUnlock()
			if ok {
				replyChannel <- &message
			}
		} else {
			fmt.Printf("Unexpected message: %v", message.Name)
		}
	}
}

func (e *ExoSecurity) buildRequester(activityID string) func(*structs.Message) *structs.Message {
	return func(message *structs.Message) *structs.Message {
		_, err := e.exoRelay.Send(exorelay.MessageOptions{
			Name:       "security request",
			Payload:    message,
			ActivityID: activityID,
		})
		if err != nil {
			fmt.Println(err)
		}
		messageChannel := make(chan *structs.Message)
		e.replyChannelsMutex.Lock()
		e.replyChannels[activityID] = messageChannel
		e.replyChannelsMutex.Unlock()
		defer func() {
			e.replyChannelsMutex.Lock()
			delete(e.replyChannels, activityID)
			e.replyChannelsMutex.Unlock()
		}()
		select {
		case message := <-messageChannel:
			bytes, err := json.Marshal(message.Payload)
			if err != nil {
				fmt.Println(err)
				return &structs.Message{}
			}
			payload := structs.Message{}
			err = json.Unmarshal(bytes, &payload)
			if err != nil {
				fmt.Println(err)
				return &structs.Message{}
			}
			return &payload
		case <-time.After(time.Second * 5):
			fmt.Printf("Did not receive a reply for '%s' after 5 seconds", activityID)
			return &structs.Message{}
		}
	}
}

func (e *ExoSecurity) receiveMessage(payload structs.Message, activityID string) {
	if e.messageAuthorizer == nil {
		return
	}
	var name string
	messageToSend := exorelay.MessageOptions{
		ActivityID: activityID,
	}
	if e.messageAuthorizer(&payload, e.buildRequester(activityID)) {
		name = "message authorized"
	} else {
		name = "message unauthorized"
	}
	messageToSend.Name = name
	_, err := e.exoRelay.Send(messageToSend)
	if err != nil {
		fmt.Println(err)
	}
}
