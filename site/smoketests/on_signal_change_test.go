package smoketests

import (
	"testing"
)

func TestUnitOnSignalChange(t *testing.T) {
	setupPageTestOnLoad(t, "tests/on_signal_change")
}
