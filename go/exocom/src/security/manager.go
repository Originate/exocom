package security

import (
	"fmt"

	"github.com/Originate/exocom/go/structs"
	"github.com/pkg/errors"
	uuid "github.com/satori/go.uuid"
)

// Manager is the struct that handles incoming security messages
type Manager struct {
	authTable    map[string]*structs.Message
	requestTable map[string]string
	hasSecurity  bool
}

// NewSecurityManager returns a new security manager
func NewSecurityManager(hasSecurity bool) *Manager {
	result := &Manager{
		authTable:    map[string]*structs.Message{},
		requestTable: map[string]string{},
		hasSecurity:  hasSecurity,
	}
	return result
}

// ReceiveMessage handles incoming security messages
func (m *Manager) ReceiveMessage(message structs.Message) *Result {
	if !m.hasSecurity {
		return &Result{
			MessageToSend:  &message,
			WarningMessage: "",
		}
	}
	var messageToSend *structs.Message
	warningMessage := ""
	switch message.Name {
	case "security request":
		var err error
		messageToSend, err = message.GetPayloadAsMessage()
		if err != nil {
			warningMessage = errors.Wrap(err, "Converting 'security request' payload to a message").Error()
			break
		}
		m.requestTable[messageToSend.ActivityID] = message.ActivityID
	case "message authorized":
		messageToSend = m.authTable[message.ActivityID]
		delete(m.authTable, message.ActivityID)
	case "message unauthorized":
		unauthorizedMessage := *m.authTable[message.ActivityID]
		delete(m.authTable, message.ActivityID)
		messageToSend = &structs.Message{
			Name:       "message unauthorized",
			Payload:    unauthorizedMessage,
			ID:         uuid.NewV4().String(),
			ActivityID: unauthorizedMessage.ActivityID,
			Sender:     message.Sender,
		}
		warningMessage = fmt.Sprintf("Warning: Unauthorized message '%s' from '%s' with activityId '%s'", unauthorizedMessage.Name, unauthorizedMessage.Sender, unauthorizedMessage.ActivityID)
	default:
		activityID, ok := m.requestTable[message.ActivityID]
		if ok {
			delete(m.requestTable, message.ActivityID)
			messageToSend = &structs.Message{
				Name:       "security response",
				Payload:    message,
				ID:         uuid.NewV4().String(),
				ActivityID: activityID,
				IsSecurity: true,
			}
		} else {
			activityID = uuid.NewV4().String()
			m.authTable[activityID] = &message
			messageToSend = &structs.Message{
				Name:       "authorize message",
				Payload:    message,
				ID:         uuid.NewV4().String(),
				ActivityID: activityID,
			}
		}
	}
	return &Result{
		MessageToSend:  messageToSend,
		WarningMessage: warningMessage,
	}
}
