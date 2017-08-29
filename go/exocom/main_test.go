package main_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"html/template"
	"io/ioutil"
	"net/http"
	"os"
	"reflect"
	"strings"
	"testing"
	"time"

	"github.com/DATA-DOG/godog"
	"github.com/DATA-DOG/godog/gherkin"
	"github.com/Originate/exocom/go/exocom/test_helpers"
	"github.com/Originate/exocom/go/structs"
	execplus "github.com/Originate/go-execplus"
	"github.com/phayes/freeport"
	"github.com/pkg/errors"
)

// nolint gocyclo
func FeatureContext(s *godog.Suite) {
	var cmdPlus *execplus.CmdPlus
	var server *http.Server
	var exocomPort int
	var services map[string]*testHelpers.MockService
	var outgoingMessageActivityID string

	createService := func(serviceName string) error {
		service := testHelpers.NewMockService(exocomPort, serviceName)
		err := service.Connect()
		if err != nil {
			return err
		}
		err = cmdPlus.WaitForText(fmt.Sprintf("'%s' registered", serviceName), time.Second*10)
		if err != nil {
			return err
		}
		services[serviceName] = service
		return nil
	}

	startExocom := func(env []string) error {
		cmdPlus = execplus.NewCmdPlus("exocom")
		cmdPlus.SetEnv(env)
		return cmdPlus.Start()
	}

	s.BeforeScenario(func(arg1 interface{}) {
		cmdPlus = nil
		server = nil
		exocomPort = freeport.GetPort()
		services = map[string]*testHelpers.MockService{}
		outgoingMessageActivityID = ""
	})

	s.AfterScenario(func(arg1 interface{}, arg2 error) {
		if cmdPlus != nil {
			err := cmdPlus.Kill()
			if err != nil {
				panic(err)
			}
		}
		if server != nil {
			err := server.Close()
			if err != nil {
				panic(err)
			}
		}
		for _, service := range services {
			err := service.Close()
			if err != nil {
				panic(err)
			}
		}
	})

	s.Step(`^starting ExoCom$`, func() error {
		return startExocom([]string{
			fmt.Sprintf("SERVICE_ROUTES=%s", "[]"),
		})
	})

	s.Step(`^another service already uses port (\d+)$`, func(port int) error {
		server = testHelpers.StartMockServer(port)
		return nil
	})

	s.Step(`^I see "([^"]*)"$`, func(text string) error {
		return cmdPlus.WaitForText(text, time.Second*10)
	})

	s.Step(`^it aborts with the message "([^"]*)"$`, func(text string) error {
		output := cmdPlus.GetOutput()
		if !strings.Contains(output, text) {
			return fmt.Errorf("Expected '%s' to contain '%s'", output, text)
		}
		return nil
	})

	s.Step(`^starting ExoCom at port (\d+)$`, func(port int) error {
		exocomPort = port
		return startExocom([]string{
			fmt.Sprintf("PORT=%d", exocomPort),
			fmt.Sprintf("SERVICE_ROUTES=%s", "[]"),
		})
	})

	s.Step(`^trying to start ExoCom at port (\d+)$`, func(port int) error {
		cmdPlus = execplus.NewCmdPlus("exocom")
		cmdPlus.SetEnv([]string{
			fmt.Sprintf("PORT=%d", port),
			fmt.Sprintf("SERVICE_ROUTES=%s", "[]"),
		})
		err := cmdPlus.Run()
		if err == nil {
			return fmt.Errorf("Expected exocom to fail but it didn't")
		}
		return nil
	})

	s.Step(`^an ExoCom instance configured with the routes:$`, func(docString *gherkin.DocString) error {
		return startExocom([]string{
			fmt.Sprintf("PORT=%d", exocomPort),
			fmt.Sprintf("SERVICE_ROUTES=%s", docString.Content),
		})
	})

	s.Step(`^a "([^"]*)" service connects and registers itself$`, func(serviceName string) error {
		return createService(serviceName)
	})

	s.Step(`^a running "([^"]*)" instance$`, func(serviceName string) error {
		return createService(serviceName)
	})

	s.Step(`^the "([^"]*)" service goes offline$`, func(serviceName string) error {
		err := services[serviceName].Close()
		if err != nil {
			return err
		}
		return cmdPlus.WaitForText(fmt.Sprintf("'%s' disconnected", serviceName), time.Second*10)
	})

	s.Step(`^the "([^"]+)" service sends "([^"]*)"$`, func(serviceName, messageName string) error {
		message := structs.Message{
			ID:      "123",
			Payload: "",
			Sender:  serviceName,
			Name:    messageName,
		}
		return services[serviceName].Send(message)
	})

	s.Step(`^the "([^"]*)" service sends:$`, func(serviceName string, messageStr *gherkin.DocString) error {
		t := template.New("request")
		t, err := t.Parse(messageStr.Content)
		if err != nil {
			return err
		}
		var expectedMessageBuffer bytes.Buffer
		err = t.Execute(&expectedMessageBuffer, map[string]interface{}{
			"outgoingActivityID": outgoingMessageActivityID,
		})
		if err != nil {
			return err
		}
		message := structs.Message{}
		err = json.Unmarshal(expectedMessageBuffer.Bytes(), &message)
		if err != nil {
			return err
		}
		message.Sender = serviceName
		return services[serviceName].Send(message)
	})

	s.Step(`^ExoCom broadcasts the following message to the "([^"]*)" service:$`, func(serviceName string, message *gherkin.DocString) error {
		expectedMessage := structs.Message{}
		err := json.Unmarshal([]byte(message.Content), &expectedMessage)
		if err != nil {
			return err
		}
		actualMessage, err := services[serviceName].WaitForMessageWithName(expectedMessage.Name)
		if err != nil {
			return fmt.Errorf("Expected to receive a message but got %v", err)
		}
		outgoingMessageActivityID = actualMessage.ActivityID

		expectedBytes, err := json.Marshal(expectedMessage.Payload)
		if err != nil {
			return err
		}
		actualBytes, err := json.Marshal(actualMessage.Payload)
		if err != nil {
			return err
		}
		actualMessagePayload := structs.Message{}
		expectedMessagePayload := structs.Message{}
		err = json.Unmarshal(actualBytes, &actualMessagePayload)
		if err != nil {
			return err
		}
		err = json.Unmarshal(expectedBytes, &expectedMessagePayload)
		if err != nil {
			return err
		}
		if !reflect.DeepEqual(expectedMessagePayload, actualMessagePayload) {
			return fmt.Errorf("Expected payload to equal %s but got %s", expectedMessagePayload, actualMessagePayload)
		}
		if expectedMessage.Name != actualMessage.Name {
			return fmt.Errorf("Expected name to equal %s but got %s", expectedMessage.Name, actualMessage.Name)
		}
		return nil
	})

	s.Step(`^ExoCom does not send any message to the "([^"]*)" service$`, func(serviceName string) error {
		receivedMessages := services[serviceName].GetReceivedMessages()
		if len(receivedMessages) == 0 {
			return nil
		}
		return fmt.Errorf("Expected the %v service to not have recived any messages but received %v", serviceName, receivedMessages)
	})

	s.Step(`ExoCom signals "([^"]*)"$`, func(messageName string) error {
		return cmdPlus.WaitForText(messageName, time.Second*10)
	})

	s.Step(`^ExoCom should have the config:$`, func(servicesDocString *gherkin.DocString) error {
		var expected map[string]interface{}
		err := json.Unmarshal([]byte(servicesDocString.Content), &expected)
		if err != nil {
			return err
		}
		resp, err := http.Get(fmt.Sprintf("http://localhost:%d/config.json", exocomPort))
		if err != nil {
			return err
		}
		body, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			return err
		}
		var actual map[string]interface{}
		err = json.Unmarshal(body, &actual)
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("Error unmarshalling: %s", string(body)))
		}
		if !reflect.DeepEqual(expected, actual) {
			return fmt.Errorf("Expected to equal %s but got %s", expected, actual)
		}
		return nil
	})

	s.Step(`^ExoCom broadcasts the (?:message|reply) "([^"]*)" to the "([^"]*)" service$`, func(messageName, serviceName string) error {
		_, err := services[serviceName].WaitForMessageWithName(messageName)
		return err
	})

	s.Step(`^the "([^"]*)" service sends "([^"]*)" for activity "([^"]*)"$`, func(serviceName, replyMessage, activityId string) error {
		return services[serviceName].Send(structs.Message{
			Sender:     serviceName,
			Payload:    "",
			ID:         "123",
			Name:       replyMessage,
			ActivityID: activityId,
		})
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
		StopOnFailure: false,
		Paths:         paths,
	})

	os.Exit(status)
}
