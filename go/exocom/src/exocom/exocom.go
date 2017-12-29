package exocom

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"time"

	"github.com/Originate/exocom/go/exocom/src/connection"
	"github.com/Originate/exocom/go/exocom/src/routing"
	"github.com/Originate/exocom/go/exocom/src/security"
	"github.com/Originate/exocom/go/exocom/src/types"
	"github.com/Originate/exocom/go/structs"
	"github.com/gorilla/websocket"
)

// ExoCom is the top level message broadcaster
type ExoCom struct {
	securityManager   *security.Manager
	connectionManager *connection.Manager
	deregisterChannel chan string
	errorChannel      chan error
	logger            *Logger
	messageCache      *MessageCache
	messageChannel    chan structs.Message
	registerChannel   chan string
	routingManager    *routing.Manager
	server            http.Server
}

var upgrader = websocket.Upgrader{}

// New creates a new ExoCom instance
func New(serviceRoutes types.Routes) (*ExoCom, error) {
	result := &ExoCom{
		deregisterChannel: make(chan string),
		errorChannel:      make(chan error),
		securityManager:   security.NewSecurityManager(serviceRoutes.HasSecurity()),
		logger:            NewLogger(os.Stdout),
		messageCache:      NewMessageCache(time.Minute),
		messageChannel:    make(chan structs.Message),
		registerChannel:   make(chan string),
		routingManager:    routing.NewManager(serviceRoutes),
	}
	result.connectionManager = connection.NewManager(connection.ManagerOptions{
		DeregisterChannel:         result.deregisterChannel,
		ErrorChannel:              result.errorChannel,
		MessageChannel:            result.messageChannel,
		RegisterChannel:           result.registerChannel,
		ActivityIDCleanupInterval: 60 * time.Second,
	})
	go result.listenForErrors()
	go result.listenForMessages()
	go result.listenForRegistrations()
	go result.listenForDeregistrations()
	var handler http.HandlerFunc = func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/services" {
			conn, err := upgrader.Upgrade(w, r, nil)
			if err != nil {
				fmt.Println("Error upgrading request to websocket:", err)
				return
			}
			result.connectionManager.AddWebsocket(conn)
		} else if r.URL.Path == "/config.json" {
			err := json.NewEncoder(w).Encode(map[string]interface{}{
				"clients": result.connectionManager.GetClients(),
				"routes":  result.routingManager.GetRoutes(),
			})
			if err != nil {
				http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
			}
		} else {
			http.Error(w, http.StatusText(http.StatusBadRequest), http.StatusBadRequest)
		}
	}
	result.server = http.Server{Handler: handler}
	return result, nil
}

// Close closes the server
func (e *ExoCom) Close() error {
	return e.server.Close()
}

// Listen opens a server on the given port
func (e *ExoCom) Listen(port int) error {
	e.server.Addr = fmt.Sprintf(":%d", port)
	fmt.Printf("ExoCom online at port %d\n", port)
	return e.server.ListenAndServe()
}

// Helpers

func (e *ExoCom) listenForErrors() {
	for {
		err := <-e.errorChannel
		printLogError(e.logger.Error(err.Error()))
	}
}

func (e *ExoCom) listenForMessages() {
	for {
		message := <-e.messageChannel
		if !e.routingManager.CanSend(message.Sender, message.Name) {
			printLogError(e.logger.Warning(fmt.Sprintf("Warning: Service '%s' is not allowed to broadcast the message '%s'", message.Sender, message.Name)))
		} else {
			securityResult := e.securityManager.ReceiveMessage(message)
			if securityResult.MessageToSend != nil {
				err := e.send(*securityResult.MessageToSend)
				if err != nil {
					printLogError(e.logger.Error(err.Error()))
				}
			}
			if securityResult.WarningMessage != "" {
				printLogError(e.logger.Warning(securityResult.WarningMessage))
			}
		}
	}
}

func (e *ExoCom) listenForRegistrations() {
	for {
		role := <-e.registerChannel
		printLogError(e.logger.Log(fmt.Sprintf("'%s' registered", role)))
	}
}

func (e *ExoCom) listenForDeregistrations() {
	for {
		role := <-e.deregisterChannel
		printLogError(e.logger.Log(fmt.Sprintf("'%s' disconnected", role)))
	}
}

func printLogError(err error) {
	if err != nil {
		fmt.Println("Error logging to stdout", err)
	}
}

func (e *ExoCom) send(message structs.Message) error {
	receiverMapping := e.routingManager.GetSubscribersFor(message)
	if len(receiverMapping) == 0 {
		printLogError(e.logger.Warning(fmt.Sprintf("Warning: No receivers for message '%s' registered", message.Name)))
		return nil
	}
	message.Timestamp = time.Now()
	originalTimestamp, ok := e.messageCache.Get(message.ActivityID)
	if ok {
		message.ResponseTime = message.Timestamp.Sub(originalTimestamp)
	}
	e.messageCache.Set(message.ActivityID, message.Timestamp)
	err := e.sendToServices(message, receiverMapping)
	if err != nil {
		return err
	}
	printLogError(e.logger.Messages(message, receiverMapping))
	return nil
}

func (e *ExoCom) sendToServices(message structs.Message, receiverMapping types.ReceiverMapping) error {
	for role, internalMessageName := range receiverMapping {
		err := e.sendToService(message, internalMessageName, role)
		if err != nil {
			return err
		}
	}
	return nil
}

func (e *ExoCom) sendToService(message structs.Message, internalMessageName, role string) error {
	serviceMessage := structs.Message{
		Name:       internalMessageName,
		ID:         message.ID,
		Payload:    message.Payload,
		Timestamp:  message.Timestamp,
		ActivityID: message.ActivityID,
	}
	if message.ResponseTime > 0 {
		serviceMessage.ResponseTime = message.ResponseTime
	}
	return e.connectionManager.SendMessage(role, serviceMessage)
}
