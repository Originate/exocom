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
	"github.com/Originate/exocom/go/structs"
	execplus "github.com/Originate/go-execplus"
	"github.com/phayes/freeport"
	"github.com/pkg/errors"
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

	s.Step(`^the "([^"]*)" test fixture runs$`, func(name string) error {
		exocomMockInstance = newExocom(exocomPort)
		cmdPlus = execplus.NewCmdPlus("go", "run", fmt.Sprintf("./test-fixtures/%s/main.go", name))
		cmdPlus.AppendEnv([]string{fmt.Sprintf("EXOCOM_PORT=%d", exocomPort)})
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

	s.Step(`^receiving this message:$`, func(message *gherkin.DocString) error {
		unmarshaled := structs.Message{}
		err := json.Unmarshal([]byte(message.Content), &unmarshaled)
		if err != nil {
			return err
		}
		return exocomMockInstance.Send(unmarshaled)
	})

	s.Step(`^it sends the message:$`, func(message *gherkin.DocString) error {
		expectedMessage := structs.Message{}
		err := json.Unmarshal([]byte(message.Content), &expectedMessage)
		if err != nil {
			return err
		}
		actualMessage, err := exocomMockInstance.WaitForMessageWithName(expectedMessage.Name)
		if err != nil {
			return err
		}
		if actualMessage.ActivityID != expectedMessage.ActivityID {
			return fmt.Errorf("Expected message to be a part of activity %s but got %s", expectedMessage.ActivityID, actualMessage.ActivityID)
		}
		actualPayload, err := actualMessage.GetPayloadAsMessage()
		if err != nil {
			return err
		}
		expectedPayload, err := expectedMessage.GetPayloadAsMessage()
		if err != nil {
			return err
		}
		if !reflect.DeepEqual(actualPayload, expectedPayload) {
			return fmt.Errorf("Expected payload to be %v but got %v", expectedPayload, actualPayload)
		}
		return nil
	})

	s.Step(`^it receives the message:$`, func(message *gherkin.DocString) error {
		messageToSend := structs.Message{}
		err := json.Unmarshal([]byte(message.Content), &messageToSend)
		if err != nil {
			return err
		}
		return exocomMockInstance.Send(messageToSend)
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
