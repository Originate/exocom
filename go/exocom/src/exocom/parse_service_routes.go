package exocom

import (
	"encoding/json"

	"github.com/Originate/exocom/go/exocom/src/translation"
	"github.com/Originate/exocom/go/exocom/src/types"
)

type rawRoute struct {
	Role                string
	Receives            []string
	Sends               []string
	MessageTranslations []translation.MessageTranslation
}

// ParseServiceRoutes parses the incoming routes
func ParseServiceRoutes(data string) (types.Routes, error) {
	var unmarshaled []rawRoute
	err := json.Unmarshal([]byte(data), &unmarshaled)
	if err != nil {
		return types.Routes{}, err
	}
	parsed := types.Routes{}
	for _, data := range unmarshaled {
		if data.Sends == nil {
			data.Sends = []string{}
		}
		if data.Receives == nil {
			data.Receives = []string{}
		}
		if data.MessageTranslations == nil {
			data.MessageTranslations = []translation.MessageTranslation{}
		}
		if data.Role == "security" {
			data.Sends = []string{"message authorized", "message unauthorized"}
			data.Receives = []string{"authorize message"}
		}
		parsed[data.Role] = types.Route{
			Receives:            data.Receives,
			Sends:               data.Sends,
			MessageTranslations: data.MessageTranslations,
		}
	}
	return parsed, nil
}
