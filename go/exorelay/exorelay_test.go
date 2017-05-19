package exorelay_test

import (
	"encoding/json"
	"errors"
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
	"github.com/Originate/exocom/go/exorelay"
	"github.com/Originate/exocom/go/exorelay/test-fixtures"
	"github.com/Originate/exocom/go/structs"
)

func newExocom() *exocomMock.ExoComMock {
	exocom := exocomMock.New()
	go func() {
		err := exocom.Listen(4100)
		if err != nil && err != http.ErrServerClosed {
			log.Fatal(err)
		}
	}()
	return exocom
}

func WaitFor(condition func() bool, errorMessage string) error {
	success := make(chan bool, 1)

	go func() {
		for {
			time.Sleep(time.Millisecond)
			if condition() {
				success <- true
				break
			}
		}
	}()

	select {
	case <-success:
		return nil
	case <-time.After(time.Second * 10):
		return errors.New(errorMessage)
	}
}

func WaitForReceivedMessagesCount(exocom *exocomMock.ExoComMock, count int) error {
	return WaitFor(func() bool {
		return len(exocom.ReceivedMessages) >= count
	}, fmt.Sprintf("Expected exocom to recieve %d messages but only has %d:\n%v", count, len(exocom.ReceivedMessages), exocom.ReceivedMessages))
}

// Cucumber step definitions
// nolint gocyclo
func FeatureContext(s *godog.Suite) {
	var exocom *exocomMock.ExoComMock
	var exoInstance *exorelay.ExoRelay
	var savedError error
	var testFixture exorelay_test_fixtures.TestFixture

	s.BeforeSuite(func() {
		exocom = newExocom()
	})

	s.BeforeScenario(func(interface{}) {
		savedError = nil
		testFixture = nil
	})

	s.AfterScenario(func(interface{}, error) {
		exocom.Reset()
	})

	s.AfterSuite(func() {
		err := exocom.Close()
		if err != nil {
			panic(err)
		}
	})

	s.Step(`^an ExoRelay with the role "([^"]*)"$`, func(role string) error {
		exoInstance = exorelay.New(exorelay.Config{
			Host: "localhost",
			Port: "4100",
			Role: role,
		})
		return nil
	})

	s.Step(`^ExoRelay connects to Exocom$`, func() error {
		err := exoInstance.Connect()
		return err
	})

	s.Step(`^it registers by sending the message "([^"]*)" with payload:$`, func(expectedName string, payloadStr *gherkin.DocString) error {
		err := WaitForReceivedMessagesCount(exocom, 1)
		if err != nil {
			return err
		}
		message := exocom.ReceivedMessages[0]
		if message.Name != expectedName {
			return fmt.Errorf("Expected message name to match %s but got %s", expectedName, message.Name)
		}
		var expectedPayload map[string]interface{}
		err = json.Unmarshal([]byte(payloadStr.Content), &expectedPayload)
		if err != nil {
			return err
		}
		if !reflect.DeepEqual(message.Payload, expectedPayload) {
			return fmt.Errorf("Expected message payload to equal %s but got %s", expectedPayload, message.Payload)
		}
		return nil
	})

	s.Step(`^sending the message "([^"]*)"$`, func(message string) error {
		return exoInstance.Send(message, nil)
	})

	s.Step(`^sending the message "([^"]*)" with the payload:$`, func(message string, payloadStr *gherkin.DocString) error {
		var payload map[string]interface{}
		err := json.Unmarshal([]byte(payloadStr.Content), &payload)
		if err != nil {
			return err
		}
		return exoInstance.Send(message, payload)
	})

	s.Step(`^trying to send an empty message$`, func() error {
		savedError = exoInstance.Send("", nil)
		if savedError == nil {
			return fmt.Errorf("Expected ExoRelay to error but it did not")
		} else {
			return nil
		}
	})

	s.Step(`^ExoRelay makes the WebSocket request:$`, func(messageStr *gherkin.DocString) error {
		err := WaitForReceivedMessagesCount(exocom, 2)
		if err != nil {
			return err
		}
		actualMessage := exocom.ReceivedMessages[1]
		var expectedMessage structs.Message
		err = json.Unmarshal([]byte(messageStr.Content), &expectedMessage)
		if err != nil {
			return err
		}
		expectedMessage.ID = actualMessage.ID
		if !reflect.DeepEqual(actualMessage, expectedMessage) {
			return fmt.Errorf("Expected request to equal %s but got %s", expectedMessage, actualMessage)
		}
		return nil
	})

	s.Step(`^ExoRelay errors with "([^"]*)"$`, func(expectedErrorMessage string) error {
		actualErrorMessage := savedError.Error()
		if actualErrorMessage != expectedErrorMessage {
			return fmt.Errorf("Expected error to equal %s but got %s", expectedErrorMessage, actualErrorMessage)
		}
		return nil
	})

	s.Step(`^I setup the "([^"]*)" test fixture$`, func(name string) error {
		testFixture = exorelay_test_fixtures.Get(name)
		testFixture.Setup(exoInstance)
		return nil
	})

	s.Step(`^receiving this message:$`, func(messageStr *gherkin.DocString) error {
		var message structs.Message
		err := json.Unmarshal([]byte(messageStr.Content), &message)
		if err != nil {
			return err
		}
		err = WaitFor(func() bool { return exocom.HasConnection() }, "nothing connected to exocom")
		if err != nil {
			return err
		}
		return exocom.Send(message)
	})

	s.Step(`^the fixture receives a message with the name "([^"]*)" and the payload nil$`, func(messageName string) error {
		err := WaitFor(func() bool { return len(testFixture.GetReceivedMessages()) == 1 }, "channel sent no messages")
		if err != nil {
			return err
		}
		actualPayload := testFixture.GetReceivedMessages()[0].Payload
		if actualPayload != nil {
			return fmt.Errorf("Expected payload to nil but got %s", actualPayload)
		}
		return nil
	})

	s.Step(`^the fixture receives a message with the name "([^"]*)" and the payload:$`, func(messageName string, payloadStr *gherkin.DocString) error {
		var expectedPayload map[string]interface{}
		err := json.Unmarshal([]byte(payloadStr.Content), &expectedPayload)
		if err != nil {
			return err
		}
		err = WaitFor(func() bool { return len(testFixture.GetReceivedMessages()) == 1 }, "channel sent no messages")
		if err != nil {
			return err
		}
		actualPayload := testFixture.GetReceivedMessages()[0].Payload
		if !reflect.DeepEqual(actualPayload, expectedPayload) {
			return fmt.Errorf("Expected payload to %s but got %s", expectedPayload, actualPayload)
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
