package exocom_test

import (
	"github.com/Originate/exocom/go/exocom/src/exocom"
	"github.com/Originate/exocom/go/exocom/src/translation"
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
					MessageTranslations: []translation.MessageTranslation{},
					Receives:            []string{},
					Sends:               []string{},
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
					MessageTranslations: []translation.MessageTranslation{},
					Receives:            []string{"message 1 name"},
					Sends:               []string{"message 2 name"},
				},
			}))
		})
	})

	Describe("with sends / receives and translation", func() {
		It("returns the routes", func() {
			routes, err := exocom.ParseServiceRoutes(
				`[
						{
								"role": "tweets",
								"sends": ["text-snippets created"],
								"receives": ["text-snippets create"],
								"messageTranslations": [
									{
										"public": "tweets create",
										"internal": "text-snippets create"
									},
									{
										"public": "tweets created",
										"internal": "text-snippets created"
									}
								]
						}
				 ]`)
			Expect(err).To(BeNil())
			Expect(routes).To(Equal(types.Routes{
				"tweets": {
					MessageTranslations: []translation.MessageTranslation{
						translation.MessageTranslation{
							Public:   "tweets create",
							Internal: "text-snippets create",
						},
						translation.MessageTranslation{
							Public:   "tweets created",
							Internal: "text-snippets created",
						},
					},
					Receives: []string{"text-snippets create"},
					Sends:    []string{"text-snippets created"},
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
					MessageTranslations: []translation.MessageTranslation{},
					Receives:            []string{},
					Sends:               []string{},
				},
				"role 2": {
					MessageTranslations: []translation.MessageTranslation{},
					Receives:            []string{},
					Sends:               []string{},
				},
			}))
		})
	})
	Describe("with security", func() {
		It("creates a service route for the security service", func() {
			routes, err := exocom.ParseServiceRoutes(
				`
				[
					{
						"role": "security"
					}
				]`)
			Expect(err).To(BeNil())
			Expect(routes).To(Equal(types.Routes{
				"security": {
					MessageTranslations: []translation.MessageTranslation{},
					Receives:            []string{"authorize message", "security response"},
					Sends:               []string{"message authorized", "message unauthorized", "security request"},
				},
			}))
		})
	})
})
