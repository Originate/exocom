package clientRegistry

import "encoding/json"

type rawRoute struct {
	Role      string
	Receives  []string
	Sends     []string
	Namespace string
}

// ParseServiceRoutes parses the incoming routes and
func ParseServiceRoutes(data string) (Routes, error) {
	var unmarshaled []rawRoute
	err := json.Unmarshal([]byte(data), &unmarshaled)
	if err != nil {
		return Routes{}, err
	}
	parsed := Routes{}
	for _, data := range unmarshaled {
		if data.Sends == nil {
			data.Sends = []string{}
		}
		if data.Receives == nil {
			data.Receives = []string{}
		}
		parsed[data.Role] = Route{
			Receives:          data.Receives,
			Sends:             data.Sends,
			InternalNamespace: data.Namespace,
		}
	}
	return parsed, nil
}
