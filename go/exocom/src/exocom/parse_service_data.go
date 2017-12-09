package exocom

import (
	"encoding/json"
	"errors"

	"github.com/Originate/exocom/go/exocom/src/translation"
	"github.com/Originate/exocom/go/exocom/src/types"
)

// ParseServiceData parses the incoming routes
func ParseServiceData(data string) (types.Routes, error) {
	if data == "" {
		return types.Routes{}, errors.New("No service data provided")
	}
	var unmarshaled types.Routes
	err := json.Unmarshal([]byte(data), &unmarshaled)
	if err != nil {
		return types.Routes{}, err
	}
	parsed := types.Routes{}
	for serviceRole, route := range unmarshaled {
		if route.Sends == nil {
			route.Sends = []string{}
		}
		if route.Receives == nil {
			route.Receives = []string{}
		}
		if route.MessageTranslations == nil {
			route.MessageTranslations = []translation.MessageTranslation{}
		}
		parsed[serviceRole] = route
	}
	if _, ok := parsed["security"]; ok {
		parsed["security"] = types.Route{
			Sends:               []string{"message authorized", "message unauthorized", "security request"},
			Receives:            []string{"authorize message", "security response"},
			MessageTranslations: []translation.MessageTranslation{},
		}
	}
	return parsed, nil
}
