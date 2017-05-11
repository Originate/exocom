package exorelay

import (
	"encoding/json"
	"fmt"
	"os"
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

	s.Step(`^an ExoRelay with the configuration:$`, func(configStr *gherkin.DocString) error {
		var config map[string]interface{}
		err := json.Unmarshal([]byte(configStr.Content), &config)
		exoInstance = New("ws://localhost:4100", config)
		if err != nil {
			return err
		}
		return nil
	})

	s.Step(`^ExoRelay connects to Exocom$`, func() error {
		err := exoInstance.Connect()
		return err
	})

	s.Step(`^it registers by sending the message "([^"]*)" with payload:$`, func(expectedName string) error {
		var message structs.Message
		message, err := exocom.WaitForReceivedMessage()
		if err != nil {
			return err
		}
		if message.Name != expectedName {
			return fmt.Errorf("Expected message name to match %s but got %s", expectedName, message.Name)
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
