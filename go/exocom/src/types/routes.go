package types

// Routes is a map from role to Route
type Routes map[string]Route

// HasSecurity returns a boolean indicating whether or not the
// routing table has a security service
func (r Routes) HasSecurity() bool {
	_, ok := r["security"]
	return ok
}
