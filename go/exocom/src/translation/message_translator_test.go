package translation_test

import (
	"github.com/Originate/exocom/go/exocom/src/translation"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("GetInternalMessageName", func() {
	It("translates the given message to the internal format of the given sender", func() {
		result := translation.GetInternalMessageName(&translation.GetInternalMessageNameOptions{
			Namespace:         "text-snippets",
			PublicMessageName: "tweets.create",
		})
		Expect(result).To(Equal("text-snippets.create"))
	})

	It("does not translate the given message if the internal namespace matches the internal namespace of the message", func() {
		result := translation.GetInternalMessageName(&translation.GetInternalMessageNameOptions{
			Namespace:         "users",
			PublicMessageName: "users.create",
		})
		Expect(result).To(Equal("users.create"))
	})

	It("does not translate the given message if it doesn't have a namespace", func() {
		result := translation.GetInternalMessageName(&translation.GetInternalMessageNameOptions{
			Namespace:         "users",
			PublicMessageName: "foo bar",
		})
		Expect(result).To(Equal("foo bar"))
	})

	It("does not translate the given message if no internal namespace is provided", func() {
		result := translation.GetInternalMessageName(&translation.GetInternalMessageNameOptions{
			Namespace:         "",
			PublicMessageName: "foo.bar",
		})
		Expect(result).To(Equal("foo.bar"))
	})
})

var _ = Describe("PublicMessageName", func() {
	It("does not convert messages that don't match the format", func() {
		result := translation.GetPublicMessageName(&translation.GetPublicMessageNameOptions{
			Namespace:           "text-snippets",
			Role:                "tweets",
			InternalMessageName: "foo bar",
		})
		Expect(result).To(Equal("foo bar"))
	})

	It("does not convert messages that have the same internal and external namespace", func() {
		result := translation.GetPublicMessageName(&translation.GetPublicMessageNameOptions{
			Namespace:           "users",
			Role:                "users",
			InternalMessageName: "users.create",
		})
		Expect(result).To(Equal("users.create"))
	})

	It("does not convert messages if the service has no internal namespace", func() {
		result := translation.GetPublicMessageName(&translation.GetPublicMessageNameOptions{
			Namespace:           "",
			Role:                "users",
			InternalMessageName: "users.create",
		})
		Expect(result).To(Equal("users.create"))
	})

	It("converts messages into the external namespace of the service", func() {
		result := translation.GetPublicMessageName(&translation.GetPublicMessageNameOptions{
			Namespace:           "text-snippets",
			Role:                "tweets",
			InternalMessageName: "text-snippets.create",
		})
		Expect(result).To(Equal("tweets.create"))
	})
})
