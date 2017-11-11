package helpers

import (
	"fmt"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exosocket"
)

// ConnectExoRelay creates an exorelay instance based on environment variables
// and connects it
func ConnectExoRelay() *exorelay.ExoRelay {
	config := exosocket.NewConfigFromEnv()
	exoRelay := exorelay.NewExoRelay(config, 0)
	err := exoRelay.Connect()
	if err != nil {
		panic(fmt.Sprintf("Error connecting: %v", err))
	}
	return exoRelay
}
