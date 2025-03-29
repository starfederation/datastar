package smoketests

import (
	"testing"
)

func TestUnitOnSignalChangeUpdated(t *testing.T) {
	setupPageTestOnLoad(t, "tests/on_signal_change_updated")
}
