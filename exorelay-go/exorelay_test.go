package exorelay_test

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"
	"testing"

	"github.com/DATA-DOG/godog"
	"github.com/DATA-DOG/godog/gherkin"
	"github.com/Originate/exocom/exocom-mock-go"
	exorelay "github.com/Originate/exocom/exorelay-go"
)

// Cucumber step definitions
func FeatureContext(s *godog.Suite) {
	var exoInstance *exorelay.ExoRelay
	exocom := exocomMock.New()

	s.Step(`^an ExoRelay with the configuration:$`, func(configStr *gherkin.DocString) error {
		var config map[string]interface{}
		err := json.Unmarshal([]byte(configStr.Content), &config)
		exoInstance = exorelay.New("ws://localhost:4100", config)
		if err != nil {
			return err
		}
		return nil
	})

	s.Step(`^ExoRelay connects to Exocom$`, func() error {
		exoInstance.Connect()
		return nil
	})

	s.Step(`^it registers by sending the message "([^"]*)" with payload:$`, func(expectedName string) error {
		messages := exocom.ReceivedMessages
		if len(messages) != 1 {
			return fmt.Errorf("Expected 1 received message but got %d", len(messages))
		}
		if messages[0].Name != expectedName {
			return fmt.Errorf("Expected message name to match %s but got %s", expectedName, messages[0].Name)
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
