package exorelay

import (
	"encoding/json"
	"fmt"
	"os"
	"reflect"
	"strings"
	"testing"

	"github.com/DATA-DOG/godog"
	"github.com/DATA-DOG/godog/gherkin"
	"github.com/Originate/exocom/go/exocom-mock"
	"github.com/Originate/exocom/go/structs"
)

// Cucumber step definitions
func FeatureContext(s *godog.Suite) {
	var exoInstance *ExoRelay
	exocom := exocomMock.New()
	go exocom.Listen(4100)

	s.Step(`^an ExoRelay with the role "([^"]*)"$`, func(role string) error {
		exoInstance = New(ExoRelayConfig{
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
		var message structs.Message
		message, err := exocom.WaitForReceivedMessage()
		if err != nil {
			return err
		}
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
}

func TestMain(m *testing.M) {
	var paths []string
	if len(os.Args) == 2 {
		paths = append(paths, strings.Split(os.Args[1], "=")[1])
	} else {
		paths = append(paths, "features")
	}
	status := godog.RunWithOptions("godogs", func(s *godog.Suite) {
		FeatureContext(s)
	}, godog.Options{
		Format:        "pretty",
		NoColors:      false,
		StopOnFailure: true,
		Paths:         paths,
	})

	os.Exit(status)
}
