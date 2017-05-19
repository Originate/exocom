package exorelay_test_fixtures

import (
	"fmt"
	"log"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/structs"
)

type TestFixture interface {
	GetReceivedMessages() []structs.Message
	Setup(exoRelay *exorelay.ExoRelay)
}

func Get(name string) TestFixture {
	switch name {
	case "receiving-messages":
		return &ReceivingMessagesTestFixture{}
	}
	log.Fatal(fmt.Sprintf("Cannot find example: %s", name))
	return nil
}
