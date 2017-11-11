package exoservice_test

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"testing"

	"github.com/DATA-DOG/godog"
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

	s.Step(`^receiving a "([^"]*)" message with activityId "([^"]*)"$`, func(name, activityId string) error {
		message := structs.Message{
			ActivityID: activityId,
			ID:         uuid.NewV4().String(),
			Name:       name,
		}
		return exocomMockInstance.Send(message)
	})

	s.Step(`^it sends a "([^"]*)" message as a reply to the message with activityId "([^"]*)"$`, func(name, activityId string) error {
		actualMessage, err := exocomMockInstance.WaitForMessageWithName(name)
		if err != nil {
			return err
		}
		if actualMessage.ActivityID != activityId {
			return fmt.Errorf("Expected message to be a part of activity %s but got %s", activityId, actualMessage.ActivityID)
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
