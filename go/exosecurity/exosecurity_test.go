package exosecurity_test

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
	"github.com/Originate/exocom/go/exosecurity"
	"github.com/Originate/exocom/go/exosecurity/test-fixtures"
	"github.com/Originate/exocom/go/structs"
	"github.com/phayes/freeport"
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
	var exoSecurity *exosecurity.ExoSecurity
	var testFixture exosecurityTestFixtures.TestFixture

	s.BeforeScenario(func(interface{}) {
		exocomPort = freeport.GetPort()
	})

	s.AfterScenario(func(interface{}, error) {
		err := exoSecurity.Close()
		if err != nil {
			panic(err)
		}
		err = exocom.Close()
		if err != nil {
			panic(err)
		}
	})

	s.Step(`^I connect the "([^"]*)" test fixture$`, func(fixtureName string) error {
		exocom = newExocom(exocomPort)
		testFixture = exosecurityTestFixtures.Get(fixtureName)
		config := exorelay.Config{
			Host: "localhost",
			Port: exocomPort,
			Role: "test-service",
		}
		exoSecurity = &exosecurity.ExoSecurity{}
		err := exoSecurity.Connect(config)
		if err != nil {
			return err
		}
		go func() {
			err := exoSecurity.ListenForMessages(testFixture.GetMessageValidator())
			if err != nil {
				panic(err)
			}
		}()
		return nil
	})

	s.Step(`^receiving this message:$`, func(message *gherkin.DocString) error {
		unmarshaled := structs.Message{}
		err := json.Unmarshal([]byte(message.Content), &unmarshaled)
		if err != nil {
			return err
		}
		_, err = exocom.WaitForConnection()
		if err != nil {
			return err
		}
		return exocom.Send(unmarshaled)
	})

	s.Step(`^it sends the message:$`, func(message *gherkin.DocString) error {
		expectedMessage := structs.Message{}
		err := json.Unmarshal([]byte(message.Content), &expectedMessage)
		if err != nil {
			return err
		}
		actualMessage, err := exocom.WaitForMessageWithName(expectedMessage.Name)
		if err != nil {
			return err
		}
		if actualMessage.ActivityID != expectedMessage.ActivityID {
			return fmt.Errorf("Expected message to be a part of activity %s but got %s", expectedMessage.ActivityID, actualMessage.ActivityID)
		}

		actualPayload := structs.Message{}
		bytes, err := json.Marshal(actualMessage.Payload)
		if err != nil {
			return err
		}
		err = json.Unmarshal(bytes, &actualPayload)
		if err != nil {
			return err
		}
		expectedPayload := structs.Message{}
		bytes, err = json.Marshal(expectedMessage.Payload)
		if err != nil {
			return err
		}
		err = json.Unmarshal(bytes, &expectedPayload)
		if err != nil {
			return err
		}

		if !reflect.DeepEqual(actualPayload, expectedPayload) {
			return fmt.Errorf("Expected payload to be %s but got %s", expectedPayload, actualPayload)
		}
		return nil
	})

	s.Step(`^it receives the message:$`, func(message *gherkin.DocString) error {
		messageToSend := structs.Message{}
		err := json.Unmarshal([]byte(message.Content), &messageToSend)
		if err != nil {
			return err
		}
		return exocom.Send(messageToSend)
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
