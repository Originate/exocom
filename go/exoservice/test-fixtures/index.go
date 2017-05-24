package exoserviceTestFixtures

import (
	"fmt"
	"log"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exoservice"
)

// TestFixture is an interface used in feature tests
type TestFixture interface {
	Setup(config exorelay.Config) *exoservice.ExoService
}

// Get returns the TestFixture for the given name
func Get(name string) TestFixture {
	switch name {
	case "ping":
		return &PingTextFixture{}
	}
	log.Fatal(fmt.Sprintf("Cannot find text fixture: %s", name))
	return nil
}
