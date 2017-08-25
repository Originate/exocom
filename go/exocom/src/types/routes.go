package types

// Routes is a map from role to Route
type Routes map[string]Route

// GetHasSecurity returns a boolean indicating whether or not the
// routing table has a security service
func (r Routes) GetHasSecurity() bool {
	_, ok := r["security"]
	return ok
}
