package fixtures

import (
	"fmt"
	"log"

	"github.com/Originate/exocom/go/exosocket"
	"github.com/Originate/exocom/go/structs"
)

// TestFixture is an interface used in feature tests
type TestFixture interface {
	GetReceivedMessages() []structs.Message
	Setup(exoSocket *exosocket.ExoSocket)
	WaitForMessageWithName(string) (structs.Message, error)
}

// Get returns the TestFixture for the given name
func Get(name string) TestFixture {
	switch name {
	case "receiving-messages":
		return &ReceivingMessagesTestFixture{}
	}
	log.Fatal(fmt.Sprintf("Cannot find example: %s", name))
	return nil
}
