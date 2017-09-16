package exosecurityTestFixtures

import (
	"fmt"
	"log"

	"github.com/Originate/exocom/go/exosecurity"
)

// TestFixture is an interface used in feature tests
type TestFixture interface {
	GetMessageValidator() exosecurity.MessageAuthorizer
}

// Get returns the TestFixture for the given name
func Get(name string) TestFixture {
	switch name {
	case "simple":
		return &SimpleTestFixture{}
	case "complex":
		return &ComplexTestFixture{}
	}
	log.Fatal(fmt.Sprintf("Cannot find text fixture: %s", name))
	return nil
}
