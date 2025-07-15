package sdktests

import (
	"testing"
)

func TestGetEndpoints(t *testing.T) {
	TestSSEGetEndpoints(t)
}

func TestPostEndpoints(t *testing.T) {
	TestSSEPostEndpoints(t)
}