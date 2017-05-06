package exocomMock

type ExoComMock struct {
	ReceivedMessages []Message
}

type Message struct {
	Name    string
	Payload map[string]interface{}
}

func New() *ExoComMock {
	return &ExoComMock{
		ReceivedMessages: []Message{Message{
			Name:    "exocom.register-service",
			Payload: nil,
		}}}
}
