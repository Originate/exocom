package translation

// GetInternalMessageName returns the internal message name for the given options
func GetInternalMessageName(opts *GetInternalMessageNameOptions) string {
	if len(opts.MessageTranslations) == 0 {
		return opts.PublicMessageName
	}
	for _, messageTranslation := range opts.MessageTranslations {
		if opts.PublicMessageName == messageTranslation.Public {
			return messageTranslation.Internal
		}
	}
	return opts.PublicMessageName
}

// GetPublicMessageName returns the public message name for the given options
func GetPublicMessageName(opts *GetPublicMessageNameOptions) string {
	if len(opts.MessageTranslations) == 0 {
		return opts.InternalMessageName
	}
	for _, messageTranslation := range opts.MessageTranslations {
		if opts.InternalMessageName == messageTranslation.Internal {
			return messageTranslation.Public
		}
	}
	return opts.InternalMessageName
}
