package security_test

import (
	"github.com/Originate/exocom/go/exocom/src/security"
	"github.com/Originate/exocom/go/structs"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("Security", func() {
	Describe("ReceiveMessage", func() {
		var (
			manager               *security.Manager
			initialMessage        structs.Message
			initialSecurityResult *security.Result
		)
		Describe("without a security service", func() {
			manager = security.NewSecurityManager(false)
			initialMessage = structs.Message{
				Name:       "create user",
				Sender:     "sender",
				ActivityID: "2",
			}
			initialSecurityResult = manager.ReceiveMessage(initialMessage)
			It(" lets all messages pass through", func() {
				Expect(*initialSecurityResult.MessageToSend).To(Equal(initialMessage))
				Expect(initialSecurityResult.WarningMessage).To(Equal(""))

			})
		})
		Describe("with a security service", func() {
			Describe("authorizing messages", func() {
				BeforeEach(func() {
					manager = security.NewSecurityManager(true)
					initialMessage = structs.Message{
						Name:       "create user",
						Sender:     "sender",
						ActivityID: "2",
					}
					initialSecurityResult = manager.ReceiveMessage(initialMessage)
				})
				It("finds and returns authorized messages", func() {
					securityResult := manager.ReceiveMessage(structs.Message{
						Name:       "message authorized",
						ActivityID: initialSecurityResult.MessageToSend.ActivityID,
					})
					Expect(*securityResult.MessageToSend).To(Equal(initialMessage))
					Expect(securityResult.WarningMessage).To(Equal(""))
				})
				It("does not allow unauthorized messages", func() {
					securityResult := manager.ReceiveMessage(structs.Message{
						Name:       "message unauthorized",
						ActivityID: initialSecurityResult.MessageToSend.ActivityID,
					})
					Expect(securityResult.MessageToSend).To((BeNil()))
					Expect(securityResult.WarningMessage).To(Equal("Warning: Unauthorized message 'create user' from 'sender' with activityId '2'"))
				})
			})
			Describe("security messages", func() {
				BeforeEach(func() {
					manager = security.NewSecurityManager(true)
					initialMessage = structs.Message{
						Name: "security request",
						Payload: structs.Message{
							Name:       "create user",
							Sender:     "sender",
							ActivityID: "123",
							ID:         "456",
						},
						ActivityID: "789",
					}
					initialSecurityResult = manager.ReceiveMessage(initialMessage)
				})

				It("creates and sends security requests", func() {
					Expect(*initialSecurityResult.MessageToSend).To(Equal(structs.Message{
						Name:       "create user",
						Sender:     "sender",
						ActivityID: "123",
						ID:         "456",
					}))
				})
				It("finds and returns security responses", func() {
					securityResult := manager.ReceiveMessage(structs.Message{
						Name:       "user created",
						Sender:     "sender",
						ActivityID: "123",
						ID:         "012",
					})
					Expect(*securityResult.MessageToSend).To(Equal(structs.Message{
						Name: "security response",
						ID:   securityResult.MessageToSend.ID,
						Payload: structs.Message{
							Name:       "user created",
							Sender:     "sender",
							ActivityID: "123",
							ID:         "012",
						},
						ActivityID: "789",
						IsSecurity: true,
					}))
					Expect(securityResult.WarningMessage).To(Equal(""))
				})
			})
		})
	})
})
