package smoketests

import (
	"testing"
)

func TestUnitSignalsJson(t *testing.T) {
	setupPageTestOnLoad(t, "tests/signals_json")
}
