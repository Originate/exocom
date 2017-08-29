package security

import (
	"encoding/json"
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
		bytes, err := json.Marshal(message.Payload)
		if err != nil {
			warningMessage = errors.Wrap(err, "Marshaling 'security request' payload").Error()
			break
		}
		messageToSend = &structs.Message{}
		err = json.Unmarshal(bytes, messageToSend)
		if err != nil {
			warningMessage = errors.Wrap(err, "Unmarshaling 'security request' payload").Error()
			break
		}
		m.requestTable[messageToSend.ActivityID] = message.ActivityID
	case "message authorized":
		messageToSend = m.authTable[message.ActivityID]
		delete(m.authTable, message.ActivityID)
	case "message unauthorized":
		messageNotToSend := m.authTable[message.ActivityID]
		delete(m.authTable, message.ActivityID)
		warningMessage = fmt.Sprintf("Warning: Unauthorized message '%s' from '%s' with activityId '%s'", messageNotToSend.Name, messageNotToSend.Sender, messageNotToSend.ActivityID)
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
