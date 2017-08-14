package exoserviceTestFixtures

import (
	"fmt"
	"log"

	"github.com/Originate/exocom/go/exoservice"
)

// TestFixture is an interface used in feature tests
type TestFixture interface {
	GetMessageHandler() exoservice.MessageHandlerMapping
}

// Get returns the TestFixture for the given name
func Get(name string) TestFixture {
	switch name {
	case "ping":
		return &PingTestFixture{}
	case "ping with dependency":
		return &PingWithDependencyTestFixture{}
	case "send":
		return &SendTestFixture{}
	}
	log.Fatal(fmt.Sprintf("Cannot find text fixture: %s", name))
	return nil
}
