package exosecurityTestFixtures

import (
	"github.com/Originate/exocom/go/exosecurity"
	"github.com/Originate/exocom/go/structs"
)

// ComplexTestFixture is a complex test fixture
type ComplexTestFixture struct{}

// GetMessageValidator returns a message validator
func (s *ComplexTestFixture) GetMessageValidator() exosecurity.MessageAuthorizer {
	return func(message *structs.Message, requestData exosecurity.DataRequester) bool {
		if message.Name == "create user" {
			receivedMessage := requestData(&structs.Message{
				Name:       "retrieve user session",
				Payload:    message.Auth,
				ActivityID: "456",
				ID:         "123",
			})
			payload, ok := receivedMessage.Payload.(map[string]interface{})
			if !ok {
				return false
			}
			return payload["isAdmin"] == "true"
		}
		return false
	}
}
