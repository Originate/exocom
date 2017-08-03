package exocom_test

import (
	"github.com/Originate/exocom/go/exocom/src/exocom"
	"github.com/Originate/exocom/go/exocom/src/types"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("ParseServiceRoutes", func() {
	Describe("with role only", func() {
		It("returns the routes", func() {
			routes, err := exocom.ParseServiceRoutes(
				`[
						{
								"role": "role 1"
						}
				 ]`)
			Expect(err).To(BeNil())
			Expect(routes).To(Equal(types.Routes{
				"role 1": {
					InternalNamespace: "",
					Receives:          []string{},
					Sends:             []string{},
				},
			}))
		})
	})

	Describe("with sends / receives", func() {
		It("returns the routes", func() {
			routes, err := exocom.ParseServiceRoutes(
				`[
						{
								"receives": ["message 1 name"],
								"role": "role 1",
								"sends": ["message 2 name"]
						}
				 ]`)
			Expect(err).To(BeNil())
			Expect(routes).To(Equal(types.Routes{
				"role 1": {
					InternalNamespace: "",
					Receives:          []string{"message 1 name"},
					Sends:             []string{"message 2 name"},
				},
			}))
		})
	})

	Describe("with internal namespace", func() {
		It("returns the routes", func() {
			routes, err := exocom.ParseServiceRoutes(
				`[
						{
								"role": "role 1",
								"namespace": "tweets"
						}
				 ]`)
			Expect(err).To(BeNil())
			Expect(routes).To(Equal(types.Routes{
				"role 1": {
					InternalNamespace: "tweets",
					Receives:          []string{},
					Sends:             []string{},
				},
			}))
		})
	})

	Describe("with multiple roles", func() {
		It("returns the routes", func() {
			routes, err := exocom.ParseServiceRoutes(
				`[
						{
								"role": "role 1"
						},
						{
								"role": "role 2"
						}
				 ]`)
			Expect(err).To(BeNil())
			Expect(routes).To(Equal(types.Routes{
				"role 1": {
					InternalNamespace: "",
					Receives:          []string{},
					Sends:             []string{},
				},
				"role 2": {
					InternalNamespace: "",
					Receives:          []string{},
					Sends:             []string{},
				},
			}))
		})
	})
})
