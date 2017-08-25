package security_test

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"testing"
)

func TestSecurity(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Security Suite")
}
