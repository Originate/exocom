package types

import "github.com/Originate/exocom/go/exocom/src/translation"

// Route is an entry in the routes
type Route struct {
	Receives            []string                         `json:"receives"`
	Sends               []string                         `json:"sends"`
	MessageTranslations []translation.MessageTranslation `json:"messageTranslations"`
}
