package smoketests

import (
	"testing"
)

func TestUnitOnSignalChangePathOnce(t *testing.T) {
	setupPageTestOnClick(t, "tests/on_signal_change_path_once")
}
