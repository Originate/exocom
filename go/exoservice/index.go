package exoservice

import (
	"fmt"
	"runtime"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exosocket"
)

// Bootstrap is the high level API to Exocom
//   create an ExoRelay instance using environment variables
//   connects to ExoCom
//   registers the given handlers
func Bootstrap(messageHandlers exorelay.MessageHandlerMapping) {
	config := exosocket.NewConfigFromEnv()
	exoRelay := exorelay.NewExoRelay(config, 1000)
	err := exoRelay.Connect()
	if err != nil {
		panic(fmt.Sprintf("Error connecting: %v", err))
	}
	for messageName, handler := range messageHandlers {
		exoRelay.RegisterHandler(messageName, handler)
	}
	fmt.Println("online at port", config.Port)
	runtime.Goexit()
}
