package smoketests

import (
	"testing"
)

func TestUnitSseEvents(t *testing.T) {
	setupPageTestOnLoad(t, "tests/sse_events")
}
