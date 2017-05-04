package exocomMock

type ExoComMock struct {
}

type Message struct {
	Name    string
	Payload map[string]interface{}
}

func New() *ExoComMock {
	return &ExoComMock{}
}

func (*ExoComMock) ReceivedMessages() []*Message {
	return []*Message{&Message{
		Name:    "exocom.register-service",
		Payload: nil,
	}}
}
