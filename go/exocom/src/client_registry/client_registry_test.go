package clientRegistry_test

import (
	"github.com/Originate/exocom/go/exocom/src/client_registry"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("ClientRegistry", func() {
	var registry *clientRegistry.ClientRegistry

	Describe("can send", func() {
		BeforeEach(func() {
			registry = clientRegistry.NewClientRegistry(clientRegistry.Routes{
				"role 1": {
					Sends: []string{"message 3 name"},
				},
			})
		})
		It("returns true if the routing table includes the message name", func() {
			result := registry.CanSend("role 1", "message 3 name")
			Expect(result).To(BeTrue())
		})
		It("returns false if the routing table does not include the message name", func() {
			result := registry.CanSend("role 1", "message 2 name")
			Expect(result).To(BeFalse())
		})

	})
	Describe("subscribers for", func() {
		BeforeEach(func() {
			registry = clientRegistry.NewClientRegistry(clientRegistry.Routes{
				"role 1": {
					Receives:          []string{"message 3 name"},
					InternalNamespace: "namespace",
				},
			})
		})
		Describe("when there are no registered receivers of the message", func() {
			It("returns an empty array", func() {
				result := registry.GetSubscribersFor("message 3 name")
				Expect(result).To(BeEmpty())
			})
		})
		Describe("when we register a client", func() {
			BeforeEach(func() {
				registry.RegisterClient("role 1")
			})
			It("returns the clients that are subscribed to the given message", func() {
				result := registry.GetSubscribersFor("message 3 name")
				Expect(result).To(Equal([]clientRegistry.Subscriber{{
					Role:              "role 1",
					InternalNamespace: "namespace",
				}}))
			})
			Describe("when we deregister the same client", func() {
				BeforeEach(func() {
					registry.DeregisterClient("role 1")
				})
				It("returns an empty slice", func() {
					result := registry.GetSubscribersFor("message 3 name")
					Expect(result).To(BeEmpty())
				})
			})
		})
	})
	Describe("the clients", func() {
		BeforeEach(func() {
			registry = clientRegistry.NewClientRegistry(clientRegistry.Routes{
				"role 1": {
					Receives:          []string{"message 3 name"},
					InternalNamespace: "namespace",
				},
			})
		})
		Describe("when there are no clients", func() {
			It("is empty", func() {
				Expect(registry.Clients).To(BeEmpty())
			})
		})
		Describe("after multiple clients register", func() {
			It("has clients", func() {
				registry.RegisterClient("role 1")
				Expect(registry.Clients["role 1"]).To(Equal(clientRegistry.Client{
					Role:              "role 1",
					ServiceType:       "role 1",
					InternalNamespace: "namespace",
				}))
			})
		})
		Describe("after a client registers and deregisters", func() {
			It("reverts to an empty map", func() {
				registry.RegisterClient("role 1")
				registry.DeregisterClient("role 1")
				Expect(registry.Clients).To(BeEmpty())
			})
		})
	})
})
