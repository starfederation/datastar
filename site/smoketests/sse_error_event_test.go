package smoketests

import (
	"testing"
)

func TestUnitSseErrorEvent(t *testing.T) {
	setupPageTestOnLoad(t, "tests/sse_error_event")
}
