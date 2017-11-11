package main

import (
	"github.com/Originate/exocom/go/exosecurity"
	"github.com/Originate/exocom/go/structs"
)

func main() {
	exosecurity.Bootstrap(func(message *structs.Message, requestData exosecurity.DataRequester) bool {
		if message.Name == "create user" {
			return message.Auth == "abc"
		}
		return false
	})
}
