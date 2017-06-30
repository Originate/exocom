package utils

import (
	"math"
	"time"
)

// Retry will execute the given function until it returns true or until it has
// been called the given number of times. The delay between calls uses an
// exponential backoff: 100 milliseconds, 200, 400, 800, 1600
func Retry(maxRetries int, attemptFn func() bool) {
	for retries := 1; retries <= maxRetries; retries++ {
		if attemptFn() {
			return
		}
		delay := int(100 * math.Pow(2, float64(retries-1)))
		time.Sleep(time.Duration(delay) * time.Millisecond)
	}
}
