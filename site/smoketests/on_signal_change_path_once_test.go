package smoketests

import (
	"testing"
)

func TestUnitOnSignalChangePathOnce(t *testing.T) {
	setupPageTestOnLoad(t, "tests/on_signal_change_path_once")
}
