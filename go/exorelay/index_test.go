package exorelay_test

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"reflect"
	"testing"
	"time"

	"github.com/DATA-DOG/godog"
	"github.com/DATA-DOG/godog/gherkin"
	"github.com/Originate/exocom/go/exocom-mock"
	"github.com/Originate/exocom/go/structs"
	execplus "github.com/Originate/go-execplus"
	"github.com/phayes/freeport"
	"github.com/pkg/errors"
	uuid "github.com/satori/go.uuid"
)

func newExocom(port int) *exocomMock.ExoComMock {
	exocom := exocomMock.New()
	go func() {
		err := exocom.Listen(port)
		if err != nil && err != http.ErrServerClosed {
			log.Fatal(err)
		}
	}()
	return exocom
}

// Cucumber step definitions
// nolint gocyclo
func FeatureContext(s *godog.Suite) {
	var exocomPort int
	var exocomMockInstance *exocomMock.ExoComMock
	var cmdPlus *execplus.CmdPlus

	s.BeforeScenario(func(interface{}) {
		exocomPort = freeport.GetPort()
	})

	s.AfterScenario(func(interface{}, error) {
		err := cmdPlus.Kill()
		if err != nil {
			panic(err)
		}
		err = exocomMockInstance.Close()
		if err != nil {
			panic(err)
		}
	})

	s.Step(`^the "([^"]*)" test fixture runs with the role "([^"]*)"$`, func(name, role string) error {
		exocomMockInstance = newExocom(exocomPort)
		cmdPlus = execplus.NewCmdPlus("go", "run", fmt.Sprintf("./test-fixtures/%s/main.go", name))
		cmdPlus.AppendEnv([]string{
			fmt.Sprintf("EXOCOM_PORT=%d", exocomPort),
			fmt.Sprintf("ROLE=%d", role),
		})
		err := cmdPlus.Start()
		if err != nil {
			return err
		}
		_, err = exocomMockInstance.WaitForConnection()
		if err != nil {
			return errors.Wrapf(err, "Child Output: %s", cmdPlus.GetOutput())
		}
		return nil
	})

	s.Step(`^no message is received$`, func() error {
		time.Sleep(time.Millisecond * 1500)
		return nil
	})

	s.Step(`^receiving a "([^"]*)" message(?: with (?:activityId "([^"]*)")?(?:(?: and )?auth "([^"]*)")?)?$`, func(name, activityId, auth string) error {
		message := structs.Message{
			ActivityID: activityId,
			ID:         uuid.NewV4().String(),
			Name:       name,
			Auth:       auth,
		}
		return exocomMockInstance.Send(message)
	})

	s.Step(`^receiving a "([^"]*)" message with activityId "([^"]*)" and isSecurity true$`, func(name, activityId string) error {
		message := structs.Message{
			ActivityID: activityId,
			ID:         uuid.NewV4().String(),
			Name:       name,
			IsSecurity: true,
		}
		return exocomMockInstance.Send(message)
	})

	s.Step(`^receiving a "([^"]*)" message with the same activityId as the "([^"]*)" message and the payload:$`, func(messageName, sentMessageName string, payloadDocString *gherkin.DocString) error {
		activityId := ""
		receivedMessages := exocomMockInstance.GetReceivedMessages()
		for _, message := range receivedMessages {
			if message.Name == sentMessageName {
				activityId = message.ActivityID
			}
		}
		if activityId == "" {
			return fmt.Errorf("Expected exocom to have received a '%s' message. It recieved: %v", sentMessageName, receivedMessages)
		}
		var payload structs.MessagePayload
		err := json.Unmarshal([]byte(payloadDocString.Content), &payload)
		if err != nil {
			return err
		}
		return exocomMockInstance.Send(structs.Message{
			ActivityID: activityId,
			ID:         uuid.NewV4().String(),
			Name:       messageName,
			Payload:    payload,
		})
	})

	s.Step(`^it sends a "([^"]*)" message with activityId "([^"]*)" and payload:$`, func(messageName, activityId string, payloadDocString *gherkin.DocString) error {
		var expectedPayload structs.MessagePayload
		err := json.Unmarshal([]byte(payloadDocString.Content), &expectedPayload)
		if err != nil {
			return err
		}
		actualMessage, err := exocomMockInstance.WaitForMessageWithName(messageName)
		if err != nil {
			return err
		}
		if actualMessage.ActivityID != activityId {
			return fmt.Errorf("Expected message to be a part of activity %s but got %s", activityId, actualMessage.ActivityID)
		}
		if !reflect.DeepEqual(actualMessage.Payload, expectedPayload) {
			return fmt.Errorf("Expected payload to %s but got %s", expectedPayload, actualMessage.Payload)
		}
		return nil
	})

	s.Step(`^it sends a "([^"]*)" message as a reply to the message with activityId "([^"]*)" and isSecurity true$`, func(name, activityId string) error {
		actualMessage, err := exocomMockInstance.WaitForMessageWithName(name)
		if err != nil {
			return err
		}
		if actualMessage.ActivityID != activityId && activityId != "" {
			return fmt.Errorf("Expected message to be a part of activity %s but got %s", activityId, actualMessage.ActivityID)
		}
		if !actualMessage.IsSecurity {
			return fmt.Errorf("Expected message to be a response to have isSecurity true but got %s", actualMessage.IsSecurity)
		}
		return nil
	})

	s.Step(`^it sends a "([^"]*)" message(?: as a reply to the message with (?:activityId "([^"]*)")?(?:(?: and )?auth "([^"]*)")?)?$`, func(name, activityId, auth string) error {
		actualMessage, err := exocomMockInstance.WaitForMessageWithName(name)
		if err != nil {
			return err
		}
		if activityId != "" && actualMessage.ActivityID != activityId {
			return fmt.Errorf("Expected message to be a part of activity %s but got %s", activityId, actualMessage.ActivityID)
		}
		if auth != "" && !reflect.DeepEqual(actualMessage.Auth, auth) {
			return fmt.Errorf("Expected message to be a response to have auth %s but got %s", auth, actualMessage.Auth)
		}
		return nil
	})
}

func TestMain(m *testing.M) {
	var paths []string
	var format string
	if len(os.Args) == 3 && os.Args[1] == "--" {
		format = "pretty"
		paths = append(paths, os.Args[2])
	} else {
		format = "progress"
		paths = append(paths, "features")
	}
	status := godog.RunWithOptions("godogs", func(s *godog.Suite) {
		FeatureContext(s)
	}, godog.Options{
		Format:        format,
		NoColors:      false,
		StopOnFailure: true,
		Paths:         paths,
	})

	os.Exit(status)
}
