package translation

import "strings"

// GetInternalMessageName returns the internal message name for the given options
func GetInternalMessageName(opts *GetInternalMessageNameOptions) string {
	if !strings.Contains(opts.PublicMessageName, ".") || opts.Namespace == "" {
		return opts.PublicMessageName
	}
	return opts.Namespace + "." + strings.Split(opts.PublicMessageName, ".")[1]
}

// GetPublicMessageName returns the public message name for the given options
func GetPublicMessageName(opts *GetPublicMessageNameOptions) string {
	if !strings.Contains(opts.InternalMessageName, ".") || opts.Namespace == "" {
		return opts.InternalMessageName
	}
	return opts.Role + "." + strings.Split(opts.InternalMessageName, ".")[1]
}
