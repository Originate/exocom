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
	})
})
