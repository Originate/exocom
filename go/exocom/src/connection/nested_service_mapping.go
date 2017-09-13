package connection

import "math/rand"

// NestedServiceMapping is a 2 level map of strings to Services
type NestedServiceMapping map[string]map[string]*Service

// Delete retrives the value for the given keys
func (n NestedServiceMapping) Delete(key1, key2 string) {
	if n[key1] == nil {
		return
	}
	delete(n[key1], key2)
}

// GetRandom retrives a random Service under the given key
func (n NestedServiceMapping) GetRandom(key1 string) *Service {
	if len(n[key1]) == 0 {
		return nil
	}
	i := rand.Intn(len(n[key1]))
	var key2 string
	for key2 = range n[key1] {
		if i == 0 {
			break
		}
		i--
	}
	return n[key1][key2]
}

// Get retrives the value for the given keys
func (n NestedServiceMapping) Get(key1, key2 string) *Service {
	if n[key1] == nil {
		return nil
	}
	return n[key1][key2]
}

// Set saves the value for the given keys
func (n NestedServiceMapping) Set(key1, key2 string, value *Service) {
	if n[key1] == nil {
		n[key1] = map[string]*Service{}
	}
	n[key1][key2] = value
}
