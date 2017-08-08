package translation

// MessageTranslation is the mapping from public to internal message names
type MessageTranslation struct {
	Public   string `json:"public"`
	Internal string `json:"internal"`
}
