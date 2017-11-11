package main

import (
	"fmt"
	"time"

	"github.com/Originate/exocom/go/exosecurity"
	"github.com/Originate/exocom/go/structs"
	"github.com/pkg/errors"
)

func main() {
	exosecurity.Bootstrap(func(message *structs.Message, requestData exosecurity.DataRequester) bool {
		if message.Name == "create user" {
			receivedMessage, err := requestData(&structs.Message{
				Name:       "retrieve user session",
				Payload:    message.Auth,
				ActivityID: "456",
				ID:         "123",
			}, time.Second*5)
			if err != nil {
				fmt.Println(errors.Wrap(err, "Error retrieving user session"))
				return false
			}
			payload, ok := receivedMessage.Payload.(map[string]interface{})
			if !ok {
				return false
			}
			return payload["isAdmin"] == "true"
		}
		return false
	})
}
