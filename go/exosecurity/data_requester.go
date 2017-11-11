package exosecurity

import (
	"time"

	"github.com/Originate/exocom/go/structs"
)

// DataRequester is a function which sends a message and returns the first response
type DataRequester func(*structs.Message, time.Duration) (*structs.Message, error)
