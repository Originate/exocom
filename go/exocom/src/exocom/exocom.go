package exocom

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"time"

	"github.com/Originate/exocom/go/exocom/src/client_registry"
	"github.com/Originate/exocom/go/exocom/src/logger"
	"github.com/Originate/exocom/go/exocom/src/message_cache"
	"github.com/Originate/exocom/go/exocom/src/message_translator"
	"github.com/Originate/exocom/go/structs"
	"github.com/Originate/exocom/go/utils"
	"github.com/gorilla/websocket"
	"github.com/pkg/errors"
)

// ExoCom is the top level message broadcaster
type ExoCom struct {
	server         http.Server
	clientRegistry *clientRegistry.ClientRegistry
	logger         *logger.Logger
	messageCache   *messageCache.MessageCache
	sockets        map[string]*websocket.Conn
}

var upgrader = websocket.Upgrader{}

// New creates a new ExoCom instance
func New(serviceRoutes clientRegistry.Routes) (*ExoCom, error) {
	result := new(ExoCom)
	var err error
	result.messageCache = messageCache.NewMessageCache(time.Minute)
	result.clientRegistry = clientRegistry.NewClientRegistry(serviceRoutes)
	result.logger = logger.NewLogger(os.Stdout)
	result.sockets = map[string]*websocket.Conn{}
	if err != nil {
		return result, err
	}
	var handler http.HandlerFunc = func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/services" {
			conn, err := upgrader.Upgrade(w, r, nil)
			if err != nil {
				fmt.Println("Error upgrading request to websocket:", err)
				return
			}
			go result.websocketHandler(conn)
		} else if r.URL.Path == "/config.json" {
			err := json.NewEncoder(w).Encode(map[string]interface{}{
				"clients": result.clientRegistry.Clients,
				"routes":  result.clientRegistry.Routing,
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

func (e *ExoCom) hasInvalidSender(message structs.Message) bool {
	return !(e.clientRegistry.CanSend(message.Sender, message.Name))
}

func (e *ExoCom) websocketHandler(socket *websocket.Conn) {
	var clientName string
	utils.ListenForMessages(socket, func(message structs.Message) error {
		if message.Name == "exocom.register-service" {
			var err error
			clientName, err = parseRegisterMessagePayload(message)
			if err == nil {
				e.clientRegistry.RegisterClient(clientName)
				e.sockets[clientName] = socket
				printLogError(e.logger.Log(fmt.Sprintf("'%s' registered", clientName)))
			} else {
				printLogError(e.logger.Error(err.Error()))
			}
		} else if e.hasInvalidSender(message) {
			printLogError(e.logger.Warning(fmt.Sprintf("Warning: Service '%s' is not allowed to broadcast the message '%s'", message.Sender, message.Name)))
		} else {
			err := e.send(message)
			if err != nil {
				printLogError(e.logger.Error(err.Error()))
			}
		}
		return nil
	}, func(err error) {
		fmt.Println(errors.Wrap(err, "Exocom listening for messages"))
	})
	if clientName != "" {
		e.clientRegistry.DeregisterClient(clientName)
	}
}

func printLogError(err error) {
	if err != nil {
		fmt.Println("Error logging to stdout", err)
	}
}

func parseRegisterMessagePayload(message structs.Message) (string, error) {
	if objectPayload, ok := message.Payload.(map[string]interface{}); ok {
		if clientName, ok := objectPayload["clientName"].(string); ok {
			return clientName, nil
		}
	}
	return "", fmt.Errorf("Invalid register message payload: %v", message.Payload)
}

func (e *ExoCom) send(message structs.Message) error {
	publicMessageName := messageTranslator.GetPublicMessageName(&messageTranslator.GetPublicMessageNameOptions{
		Namespace:           e.clientRegistry.Clients[message.Sender].InternalNamespace,
		ClientName:          message.Sender,
		InternalMessageName: message.Name,
	})
	originalName := message.Name
	message.Timestamp = time.Now()
	subscribers := e.clientRegistry.GetSubscribersFor(publicMessageName)
	if len(subscribers) == 0 {
		printLogError(e.logger.Warning(fmt.Sprintf("Warning: No receivers for message '%s' registered", publicMessageName)))
		return nil
	}
	if message.ResponseTo != "" {
		originalTimestamp, ok := e.messageCache.Get(message.ID)
		if ok {
			message.ResponseTime = message.Timestamp.Sub(originalTimestamp)
		}
	} else {
		e.messageCache.Set(message.ID, message.Timestamp)
	}
	internalMessageNameMapping, err := e.sendToServices(message, publicMessageName, subscribers)
	if err != nil {
		return err
	}
	printLogError(e.logger.Messages(message, originalName, internalMessageNameMapping))
	return nil
}

func (e *ExoCom) sendToServices(message structs.Message, publicMessageName string, subscribers []clientRegistry.Subscriber) (map[string]string, error) {
	internalMessageNames := map[string]string{}
	for _, subscriber := range subscribers {
		internalMessageName, err := e.sendToService(message, publicMessageName, subscriber)
		if err != nil {
			return internalMessageNames, err
		}
		internalMessageNames[subscriber.ClientName] = internalMessageName
	}
	return internalMessageNames, nil
}

func (e *ExoCom) sendToService(message structs.Message, publicMessageName string, subscriber clientRegistry.Subscriber) (string, error) {
	internalMessageName := messageTranslator.GetInternalMessageName(&messageTranslator.GetInternalMessageNameOptions{
		Namespace:         subscriber.InternalNamespace,
		PublicMessageName: publicMessageName,
	})
	serviceMessage := structs.Message{
		Name:      internalMessageName,
		ID:        message.ID,
		Payload:   message.Payload,
		Timestamp: message.Timestamp,
	}
	if message.ResponseTo != "" {
		serviceMessage.ResponseTime = message.ResponseTime
		serviceMessage.ResponseTo = message.ResponseTo
	}
	serializedBytes, err := json.Marshal(serviceMessage)
	if err != nil {
		return "", err
	}
	err = e.sockets[subscriber.ClientName].WriteMessage(websocket.TextMessage, serializedBytes)
	if err != nil {
		return "", err
	}
	return internalMessageName, nil
}
