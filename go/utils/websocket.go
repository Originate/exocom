package utils

import (
	"fmt"
	"time"

	"github.com/gorilla/websocket"
)

// ConnectWithRetry will attempt to connect a websocket to the given url
// retrying if needed with the given delay (milliseconds) between calls
func ConnectWithRetry(url string, delay int) (socket *websocket.Conn, err error) {
	startTime := time.Now()
	logErrorDelay := time.Duration(delay*10) * time.Millisecond
	Retry(delay, func() bool {
		socket, _, err = websocket.DefaultDialer.Dial(url, nil)
		if err != nil {
			if time.Now().After(startTime.Add(logErrorDelay)) {
				fmt.Printf("Unable to connect after %s (%s). Retrying...\n", logErrorDelay, err)
				logErrorDelay = logErrorDelay * 2
			}
			return false
		}
		return true
	})
	return socket, err
}
