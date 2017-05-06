package exocomMock

import "github.com/Originate/exocom/go/structs"

type ExoComMock struct {
	ReceivedMessages []structs.Message
}

func New() *ExoComMock {
	return &ExoComMock{
		ReceivedMessages: []structs.Message{structs.Message{
			Name:    "exocom.register-service",
			Payload: nil,
		}}}
}
