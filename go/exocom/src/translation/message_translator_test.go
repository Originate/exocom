package translation_test

import (
	"github.com/Originate/exocom/go/exocom/src/translation"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("GetInternalMessageName", func() {
	It("translates the given message to the internal format of the given sender", func() {
		result := translation.GetInternalMessageName(&translation.GetInternalMessageNameOptions{
			MessageTranslations: []translation.MessageTranslation{
				translation.MessageTranslation{
					Public:   "tweets create",
					Internal: "text-snippets create",
				},
			},
			PublicMessageName: "tweets create",
		})
		Expect(result).To(Equal("text-snippets create"))
	})

	It("does not translate the given message if there is no translation mapping of the message", func() {
		result := translation.GetInternalMessageName(&translation.GetInternalMessageNameOptions{
			MessageTranslations: []translation.MessageTranslation{},
			PublicMessageName:   "users create",
		})
		Expect(result).To(Equal("users create"))
	})

})

var _ = Describe("PublicMessageName", func() {
	It("does not convert messages that have the same internal and external namespace", func() {
		result := translation.GetPublicMessageName(&translation.GetPublicMessageNameOptions{
			MessageTranslations: []translation.MessageTranslation{},
			InternalMessageName: "users create",
		})
		Expect(result).To(Equal("users create"))
	})

	It("converts messages into the external namespace of the service", func() {
		result := translation.GetPublicMessageName(&translation.GetPublicMessageNameOptions{
			MessageTranslations: []translation.MessageTranslation{
				translation.MessageTranslation{
					Public:   "tweets create",
					Internal: "text-snippets create",
				},
			},
			InternalMessageName: "text-snippets create",
		})
		Expect(result).To(Equal("tweets create"))
	})
})
