package smoketests

import (
	"testing"
)

func TestUnitOnSignalChangePathWildcard(t *testing.T) {
	setupPageTestOnLoad(t, "tests/on_signal_change_path_wildcard")
}
