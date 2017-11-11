package exosecurity

import (
	"fmt"
	"runtime"
	"time"

	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exosocket"
	"github.com/Originate/exocom/go/structs"
	"github.com/pkg/errors"
)

// Bootstrap brings an exosecurity online using environment variables and the
// given messageauthorizer
func Bootstrap(messageAuthorizer MessageAuthorizer) {
	config := exosocket.NewConfigFromEnv()
	exoRelay := exorelay.NewExoRelay(config, 1000)
	err := exoRelay.Connect()
	if err != nil {
		panic(fmt.Sprintf("Error connecting: %v", err))
	}
	exoRelay.RegisterHandler("authorize message", func(request exorelay.Request) {
		message, err := request.Message.GetPayloadAsMessage()
		if err != nil {
			fmt.Println(errors.Wrap(err, "Error casting message payload to message"))
		}
		requester := buildRequestDataFunc(request.Message.ActivityID, exoRelay)
		var replyMessageName string
		if messageAuthorizer(message, requester) {
			replyMessageName = "message authorized"
		} else {
			replyMessageName = "message unauthorized"
		}
		err = request.Reply(replyMessageName, nil)
		if err != nil {
			fmt.Println(errors.Wrap(err, "Error sending reply"))
		}
	})
	runtime.Goexit()
}

func buildRequestDataFunc(activityID string, exoRelay *exorelay.ExoRelay) DataRequester {
	return func(message *structs.Message, timeout time.Duration) (*structs.Message, error) {
		_, err := exoRelay.Send(exosocket.MessageOptions{
			Name:       "security request",
			Payload:    message,
			ActivityID: activityID,
		})
		if err != nil {
			return nil, err
		}
		result, err := exoRelay.WaitForActivity(activityID, timeout)
		if err != nil {
			return nil, err
		}
		return result.GetPayloadAsMessage()
	}
}
