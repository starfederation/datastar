package smoketests

import (
	"testing"
)

func TestUnitSignalsChange(t *testing.T) {
	setupPageTestOnClick(t, "tests/signals_change")
}
