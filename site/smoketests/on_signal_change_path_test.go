package smoketests

import (
	"testing"
)

func TestUnitOnSignalChangePath(t *testing.T) {
	setupPageTestOnLoad(t, "tests/on_signal_change_path")
}
