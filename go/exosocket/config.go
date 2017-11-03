package exosocket

import (
	"os"
)

// Config contains the configuration values for ExoSocket instances
type Config struct {
	Host string
	Port string
	Role string
}

// NewConfigFromEnv returns a Config object based on environment variables
func NewConfigFromEnv() Config {
	return Config{
		Host: os.Getenv("EXOCOM_HOST"),
		Port: getPort(),
		Role: os.Getenv("ROLE"),
	}
}

func getPort() string {
	userPort := os.Getenv("EXOCOM_PORT")
	if userPort == "" {
		return "80"
	}
	return userPort
}
