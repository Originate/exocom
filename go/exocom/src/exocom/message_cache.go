package exocom

import (
	"sync"
	"time"
)

// MessageCache records the timestamp of each message.
// The cache automatically deletes any message older then 1 minute
type MessageCache struct {
	cache map[string]time.Time
	// mutex is needed to prevent writes (Set) from occurring during
	// iteration (clearCache) which is a runtime error
	mutex sync.RWMutex
}

// NewMessageCache returns a new MessageCache
// removing old messages with a frequency equal to the given duration
func NewMessageCache(cleanupInterval time.Duration) *MessageCache {
	result := &MessageCache{cache: map[string]time.Time{}}
	go func() {
		for {
			time.Sleep(cleanupInterval)
			result.mutex.Lock()
			result.clearCache()
			result.mutex.Unlock()
		}
	}()
	return result
}

func (c *MessageCache) clearCache() {
	for id, timestamp := range c.cache {
		if time.Since(timestamp) > time.Minute {
			delete(c.cache, id)
		}
	}
}

// Get returns the timestamp for the given activityId and whether or not data exists for that messageId
func (c *MessageCache) Get(activityID string) (time.Time, bool) {
	c.mutex.RLock()
	timestamp, ok := c.cache[activityID]
	c.mutex.RUnlock()
	return timestamp, ok
}

// Set adds the given activityId and timestamp to the cache
func (c *MessageCache) Set(activityID string, timestamp time.Time) {
	c.mutex.Lock()
	c.cache[activityID] = timestamp
	c.mutex.Unlock()
}
