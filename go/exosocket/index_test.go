package exosocket_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"html/template"
	"log"
	"net/http"
	"os"
	"reflect"
	"strconv"
	"sync"
	"testing"
	"time"

	"github.com/DATA-DOG/godog"
	"github.com/DATA-DOG/godog/gherkin"
	"github.com/Originate/exocom/go/exocom-mock"
	"github.com/Originate/exocom/go/exosocket"
	"github.com/Originate/exocom/go/exosocket/test-fixtures"
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

// nolint: gocyclo
func FeatureContext(s *godog.Suite) {
	var (
		exocomPort         int
		exoComMockInstance *exocomMock.ExoComMock
		exoSocketInstance  *exosocket.ExoSocket
		outgoingMessage    *structs.Message
		savedError         error
		testFixture        fixtures.TestFixture
	)

	s.BeforeScenario(func(interface{}) {
		exocomPort = freeport.GetPort()
		outgoingMessage = &structs.Message{}
		savedError = nil
		testFixture = nil
	})

	s.AfterScenario(func(interface{}, error) {
		err := exoSocketInstance.Close()
		if err != nil {
			panic(err)
		}
		err = exoComMockInstance.Close()
		if err != nil {
			panic(err)
		}
	})

	s.Step(`^an ExoSocket instance with the role "([^"]*)"$`, func(role string) error {
		exoSocketInstance = exosocket.NewExoSocket(
			exosocket.Config{
				Host: "localhost",
				Port: strconv.Itoa(exocomPort),
				Role: role,
			},
			0,
		)
		return nil
	})

	s.Step(`^(an ExoSocket instance that is connected to ExoCom|ExoSocket connects to ExoCom)$`, func() error {
		exoComMockInstance = newExocom(exocomPort)
		err := exoSocketInstance.Connect()
		if err != nil {
			return err
		}
		_, err = exoComMockInstance.WaitForConnection()
		return err
	})

	s.Step(`^it registers by sending the message "([^"]*)" with the sender "([^"]*)"`, func(expectedName, expectedSender string) error {
		message, err := exoComMockInstance.WaitForMessageWithName(expectedName)
		if err != nil {
			return err
		}
		if message.Sender != expectedSender {
			return fmt.Errorf("Expected sender to equal %s but got %s", expectedSender, message.Sender)
		}
		return nil
	})

	s.Step(`^sending the message "([^"]*)"$`, func(name string) error {
		var err error
		outgoingMessage, err = exoSocketInstance.Send(exosocket.MessageOptions{Name: name})
		return err
	})

	s.Step(`^sending the message "([^"]*)" with the payload:$`, func(name string, payloadStr *gherkin.DocString) error {
		var payload structs.MessagePayload
		err := json.Unmarshal([]byte(payloadStr.Content), &payload)
		if err != nil {
			return err
		}
		outgoingMessage, err = exoSocketInstance.Send(exosocket.MessageOptions{Name: name, Payload: payload})
		return err
	})

	s.Step(`^sending the message "([^"]*)" with auth "([^"]*)"$`, func(name, auth string) error {
		var err error
		outgoingMessage, err = exoSocketInstance.Send(exosocket.MessageOptions{Name: name, Auth: auth})
		return err
	})

	s.Step(`^trying to send an empty message$`, func() error {
		outgoingMessage, savedError = exoSocketInstance.Send(exosocket.MessageOptions{Name: ""})
		if savedError == nil {
			return fmt.Errorf("Expected ExoRelay to error but it did not")
		} else {
			return nil
		}
	})

	s.Step(`^ExoSocket makes the WebSocket request:$`, func(messageStr *gherkin.DocString) error {
		t := template.New("request")
		t, err := t.Parse(messageStr.Content)
		if err != nil {
			return err
		}
		var expectedMessageBuffer bytes.Buffer
		err = t.Execute(&expectedMessageBuffer, map[string]interface{}{
			"outgoingMessageId":  outgoingMessage.ID,
			"outgoingActivityId": outgoingMessage.ActivityID,
		})
		if err != nil {
			return err
		}
		var expectedMessage structs.Message
		err = json.Unmarshal(expectedMessageBuffer.Bytes(), &expectedMessage)
		if err != nil {
			return err
		}
		actualMessage, err := exoComMockInstance.WaitForMessageWithName(expectedMessage.Name)
		if err != nil {
			return err
		}
		if !reflect.DeepEqual(actualMessage, expectedMessage) {
			return fmt.Errorf("Expected request to equal %v but got %v", expectedMessage, actualMessage)
		}
		return nil
	})

	s.Step(`^ExoSocket errors with "([^"]*)"$`, func(expectedErrorMessage string) error {
		actualErrorMessage := savedError.Error()
		if actualErrorMessage != expectedErrorMessage {
			return fmt.Errorf("Expected error to equal %s but got %s", expectedErrorMessage, actualErrorMessage)
		}
		return nil
	})

	s.Step(`^I setup the "([^"]*)" test fixture$`, func(name string) error {
		testFixture = fixtures.Get(name)
		testFixture.Setup(exoSocketInstance)
		return nil
	})

	s.Step(`^receiving this message:$`, func(messageStr *gherkin.DocString) error {
		var message structs.Message
		err := json.Unmarshal([]byte(messageStr.Content), &message)
		if err != nil {
			return err
		}
		return exoComMockInstance.Send(message)
	})

	s.Step(`^the fixture receives a message with the name "([^"]*)" and the payload nil$`, func(messageName string) error {
		message, err := testFixture.WaitForMessageWithName(messageName)
		if err != nil {
			return err
		}
		actualPayload := message.Payload
		if actualPayload != nil {
			return fmt.Errorf("Expected payload to nil but got %s", actualPayload)
		}
		return nil
	})

	s.Step(`^the fixture receives a message with the name "([^"]*)" and the payload:$`, func(messageName string, payloadStr *gherkin.DocString) error {
		var expectedPayload structs.MessagePayload
		err := json.Unmarshal([]byte(payloadStr.Content), &expectedPayload)
		if err != nil {
			return err
		}
		message, err := testFixture.WaitForMessageWithName(messageName)
		if err != nil {
			return err
		}
		actualPayload := message.Payload
		if !reflect.DeepEqual(actualPayload, expectedPayload) {
			return fmt.Errorf("Expected payload to %s but got %s", expectedPayload, actualPayload)
		}
		return nil
	})

	s.Step(`^the fixture receives a message with the name "([^"]*)" and auth "([^"]*)"$`, func(messageName, auth string) error {
		message, err := testFixture.WaitForMessageWithName(messageName)
		if err != nil {
			return err
		}
		if !reflect.DeepEqual(message.Auth, auth) {
			return fmt.Errorf("Expected auth %s but got %s", auth, message.Auth)
		}
		return nil
	})

	s.Step(`^the fixture receives a message with the name "([^"]*)" and isSecurity true$`, func(messageName string) error {
		message, err := testFixture.WaitForMessageWithName(messageName)
		if err != nil {
			return err
		}
		if !message.IsSecurity {
			return fmt.Errorf("Expected isSecurity true but got false")
		}
		return nil
	})

	s.Step(`^ExoCom is offline$`, func() error {
		return nil // noop
	})

	s.Step(`^ExoSocket boots up a second before ExoCom$`, func() error {
		var wg sync.WaitGroup
		wg.Add(1)
		go func() {
			err := exoSocketInstance.Connect()
			if err != nil {
				panic(err)
			}
			wg.Done()
		}()
		time.Sleep(time.Duration(200) * time.Millisecond)
		exoComMockInstance = newExocom(exocomPort)
		wg.Wait()
		return nil
	})

	s.Step(`^ExoSocket should (?:re)?connect to ExoCom$`, func() error {
		_, err := exoComMockInstance.WaitForConnection()
		return err
	})

	s.Step(`^Exocom crashes and reboots`, func() error {
		return exoComMockInstance.CloseConnection()
	})
}

func TestMain(m *testing.M) {
	var (
		paths  []string
		format string
	)
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
