package connection_test

import (
	"github.com/Originate/exocom/go/exocom/src/connection"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("NestedServiceMapping", func() {
	var _ = Describe("DeleteNestedKey", func() {
		It("deletes a nested key value entry", func() {
			actualMap := connection.NestedServiceMapping{
				"service1": map[string]*connection.Service{
					"111": &connection.Service{},
					"222": &connection.Service{},
				},
				"service2": map[string]*connection.Service{
					"111": &connection.Service{},
				},
			}
			expectedMap := connection.NestedServiceMapping{
				"service1": map[string]*connection.Service{
					"222": &connection.Service{},
				},
				"service2": map[string]*connection.Service{},
			}
			actualMap.DeleteNestedKey("111")
			Expect(actualMap).To(Equal(expectedMap))
		})
	})
})
