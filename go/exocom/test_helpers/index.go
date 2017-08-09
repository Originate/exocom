package testHelpers

import (
	"fmt"
	"net/http"
)

// StartMockServer starts a mock server on the given port
func StartMockServer(port int) *http.Server {
	server := http.Server{Addr: fmt.Sprintf(":%d", port)}
	go func() {
		err := server.ListenAndServe()
		if err != nil && err.Error() != "Server Closed" {
			fmt.Println("Mock server error", err)
		}
	}()
	return &server
}
