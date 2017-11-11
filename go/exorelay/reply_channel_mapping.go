package exorelay

import (
	"github.com/Originate/exocom/go/structs"
)

// ReplyChannelMapping is a map from message activityId to a channel that should be sent messages
type ReplyChannelMapping map[string]chan *structs.Message
