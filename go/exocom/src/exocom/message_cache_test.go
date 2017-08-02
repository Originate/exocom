package exocom_test

import (
	"time"

	"github.com/Originate/exocom/go/exocom/src/exocom"
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = Describe("MessageCache", func() {
	var (
		messageCache *exocom.MessageCache
		timestamp    time.Time
	)
	BeforeEach(func() {
		messageCache = exocom.NewMessageCache(time.Millisecond)
		timestamp = time.Now()
	})
	Describe("Get / Set", func() {
		It("returns the timestamp of the message with the given activityId", func() {
			messageCache.Set("message1", timestamp)
			result, ok := messageCache.Get("message1")
			Expect(ok).To(BeTrue())
			Expect(result).To(Equal(timestamp))
		})
		It("returns not ok if the activityId does not exist", func() {
			_, ok := messageCache.Get("foo-bar")
			Expect(ok).To(BeFalse())
		})
	})
	Describe("clearCache", func() {
		It("clears the messageCache from messages older than one minute", func() {
			messageCache.Set("message1", time.Now().Add(time.Minute*-1))
			time.Sleep(time.Millisecond * 5)
			_, ok := messageCache.Get("message1")
			Expect(ok).To(BeFalse())
		})
		It("keeps messages in the messageCache that are younger than a minute", func() {
			messageCache.Set("message2", timestamp)
			time.Sleep(time.Millisecond * 10)
			result, ok := messageCache.Get("message2")
			Expect(ok).To(BeTrue())
			Expect(result).To(Equal(timestamp))
		})
	})

})
