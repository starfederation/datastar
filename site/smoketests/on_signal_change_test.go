package smoketests

import (
	"testing"
)

func TestUnitOnSignalChange(t *testing.T) {
	setupPageTestOnClick(t, "tests/on_signal_change")
}
