package exosecurityTestFixtures

import (
	"github.com/Originate/exocom/go/exosecurity"
	"github.com/Originate/exocom/go/structs"
)

// SimpleTestFixture is a simple test fixture
type SimpleTestFixture struct{}

// GetMessageValidator returns a message validator
func (s *SimpleTestFixture) GetMessageValidator() exosecurity.MessageAuthorizer {
	return func(message *structs.Message, requestData exosecurity.DataRequester) bool {
		if message.Name == "create user" {
			return message.Auth == "abc"
		}
		return false
	}
}
