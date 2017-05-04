package main

import (
	"os"
	"strings"
	"testing"

	"github.com/DATA-DOG/godog"
)

// Cucumber step definitions
func FeatureContext(s *godog.Suite) {
	s.Step(`^Hello$`, func() error {
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
