package exoservice_test

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"reflect"
	"testing"

	"github.com/DATA-DOG/godog"
	"github.com/DATA-DOG/godog/gherkin"
	"github.com/Originate/exocom/go/exocom-mock"
	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exoservice"
	"github.com/Originate/exocom/go/exoservice/test-fixtures"
	"github.com/Originate/exocom/go/structs"
	"github.com/phayes/freeport"
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

// nolint gocyclo
func FeatureContext(s *godog.Suite) {
	var exocomPort int
	var exocom *exocomMock.ExoComMock
	var exoService *exoservice.ExoService
	var testFixture exoserviceTestFixtures.TestFixture

	s.BeforeScenario(func(interface{}) {
		exocomPort = freeport.GetPort()
	})

	s.AfterScenario(func(interface{}, error) {
		err := exoService.Close()
		if err != nil {
			panic(err)
		}
		err = exocom.Close()
		if err != nil {
			panic(err)
		}
	})

	s.Step(`^I connect the "([^"]*)" test fixture$`, func(name string) error {
		exocom = newExocom(exocomPort)
		testFixture = exoserviceTestFixtures.Get(name)
		config := exorelay.Config{
			Host: "localhost",
			Port: exocomPort,
			Role: "test-service",
		}
		exoService = &exoservice.ExoService{}
		err := exoService.Connect(config)
		if err != nil {
			return err
		}
		go func() {
			err := exoService.ListenForMessages(testFixture.GetMessageHandler())
			if err != nil {
				panic(err)
			}
		}()
		return nil
	})

	s.Step(`^receiving a "([^"]*)" message(?: with (?:activityId "([^"]*)")?(?:(?: and )?auth "([^"]*)")?)?$`, func(name, activityId, auth string) error {
		message := structs.Message{
			ActivityID: activityId,
			ID:         uuid.NewV4().String(),
			Name:       name,
			Auth:       auth,
		}
		_, err := exocom.WaitForConnection()
		if err != nil {
			return err
		}
		return exocom.Send(message)
	})

	s.Step(`^it sends a "([^"]*)" message(?: as a reply to the message with (?:activityId "([^"]*)")?(?:(?: and )?auth "([^"]*)")?)?$`, func(name, activityId, auth string) error {
		actualMessage, err := exocom.WaitForMessageWithName(name)
		if err != nil {
			return err
		}
		if actualMessage.ActivityID != activityId && activityId != "" {
			return fmt.Errorf("Expected message to be a part of activity %s but got %s", activityId, actualMessage.ActivityID)
		}
		if !reflect.DeepEqual(actualMessage.Auth, auth) {
			return fmt.Errorf("Expected message to be a response to have auth %s but got %s", auth, actualMessage.Auth)
		}
		return nil
	})

	s.Step(`^receiving a "([^"]*)" message with the same activityId as the "([^"]*)" message and the payload:$`, func(messageName, sentMessageName string, payloadDocString *gherkin.DocString) error {
		activityId := ""
		receivedMessages := exocom.GetReceivedMessages()
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
		return exocom.Send(structs.Message{
			ActivityID: activityId,
			ID:         uuid.NewV4().String(),
			Name:       messageName,
			Payload:    payload,
		})
	})

	s.Step(`^receiving a "([^"]*)" message with activityId "([^"]*)" and isSecurity true$`, func(name, activityId string) error {
		message := structs.Message{
			ActivityID: activityId,
			ID:         uuid.NewV4().String(),
			Name:       name,
			IsSecurity: true,
		}
		_, err := exocom.WaitForConnection()
		if err != nil {
			return err
		}
		return exocom.Send(message)
	})

	s.Step(`^it sends a "([^"]*)" message as a reply to the message with activityId "([^"]*)" and isSecurity true$`, func(name, activityId string) error {
		actualMessage, err := exocom.WaitForMessageWithName(name)
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

	s.Step(`^it sends a "([^"]*)" message with activityId "([^"]*)" and payload:$`, func(messageName, activityId string, payloadDocString *gherkin.DocString) error {
		var expectedPayload structs.MessagePayload
		err := json.Unmarshal([]byte(payloadDocString.Content), &expectedPayload)
		if err != nil {
			return err
		}
		actualMessage, err := exocom.WaitForMessageWithName(messageName)
		if err != nil {
			return err
		}
		if actualMessage.ActivityID != activityId && activityId != "" {
			return fmt.Errorf("Expected message to be a part of activity %s but got %s", activityId, actualMessage.ActivityID)
		}
		if !reflect.DeepEqual(actualMessage.Payload, expectedPayload) {
			return fmt.Errorf("Expected payload to %s but got %s", expectedPayload, actualMessage.Payload)
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
