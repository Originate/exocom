package routing_test

import (
	"github.com/Originate/exocom/go/exocom/src/routing"
	"github.com/Originate/exocom/go/exocom/src/translation"
	"github.com/Originate/exocom/go/exocom/src/types"
	"github.com/Originate/exocom/go/structs"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("Manager", func() {
	var manager *routing.Manager

	Describe("CanSend", func() {
		BeforeEach(func() {
			manager = routing.NewManager(types.Routes{
				"role 1": {
					Sends: []string{"my message name"},
				},
			})
		})

		It("returns true if the routing table includes the message name", func() {
			result := manager.CanSend("role 1", "my message name")
			Expect(result).To(BeTrue())
		})

		It("returns false if the routing table does not include the message name", func() {
			result := manager.CanSend("role 1", "other message name")
			Expect(result).To(BeFalse())
		})
	})

	Describe("GetSubscribersFor", func() {
		Describe("no subscribers", func() {
			BeforeEach(func() {
				manager = routing.NewManager(types.Routes{})
			})

			It("returns an empty array", func() {
				result := manager.GetSubscribersFor(structs.Message{
					Name: "users create",
				})
				Expect(result).To(BeEmpty())
			})
		})

		Describe("with a subscriber without an internal namespace", func() {
			BeforeEach(func() {
				manager = routing.NewManager(types.Routes{
					"user": types.Route{
						Receives: []string{"users create"},
					},
				})
			})

			It("returns the subscriber", func() {
				result := manager.GetSubscribersFor(structs.Message{
					Name: "users create",
				})
				Expect(result).To(Equal(types.ReceiverMapping{
					"user": "users create",
				}))
			})
		})

		Describe("with a subscriber with a translation table", func() {
			It("returns the subscriber", func() {
				manager = routing.NewManager(types.Routes{
					"tweet": types.Route{
						MessageTranslations: []translation.MessageTranslation{
							translation.MessageTranslation{
								Public:   "tweets create",
								Internal: "text-snippets create",
							},
						},
						Receives: []string{"text-snippets create"},
					},
				})
				result := manager.GetSubscribersFor(structs.Message{
					Name: "tweets create",
				})
				Expect(result).To(Equal(types.ReceiverMapping{
					"tweet": "text-snippets create",
				}))
			})
		})
	})
})
