package smoketests

import (
	"testing"
)

func TestUnitOnLoadDelay(t *testing.T) {
	setupPageTestOnDelay(t, "tests/on_load_delay")
}
