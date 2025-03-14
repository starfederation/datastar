package smoketests

import (
	"testing"
)

func TestUnitSignalsChangePathOnce(t *testing.T) {
	setupPageTestOnClick(t, "tests/signals_change_path_once")
}
