package messageCache_test

import (
	"time"

	"github.com/Originate/exocom/go/exocom/src/message_cache"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("MessageCache", func() {
	var (
		cache     *messageCache.MessageCache
		timestamp time.Time
	)
	BeforeEach(func() {
		cache = messageCache.NewMessageCache(time.Millisecond)
		timestamp = time.Now()
	})
	Describe("Get / Set", func() {
		It("returns the timestamp of the message with the given messageId", func() {
			cache.Set("message1", timestamp)
			result, ok := cache.Get("message1")
			Expect(ok).To(BeTrue())
			Expect(result).To(Equal(timestamp))
		})
		It("returns not ok if the messageid does not exist", func() {
			_, ok := cache.Get("foo-bar")
			Expect(ok).To(BeFalse())
		})
	})
	Describe("clearCache", func() {
		It("clears the cache from messages older than one minute", func() {
			cache.Set("message1", time.Now().Add(time.Minute*-1))
			time.Sleep(time.Millisecond * 5)
			_, ok := cache.Get("message1")
			Expect(ok).To(BeFalse())
		})
		It("keeps messages in the cache that are younger than a minute", func() {
			cache.Set("message2", timestamp)
			time.Sleep(time.Millisecond * 5)
			result, ok := cache.Get("message2")
			Expect(ok).To(BeTrue())
			Expect(result).To(Equal(timestamp))
		})
	})

})
