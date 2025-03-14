package smoketests

import (
	"testing"
)

func TestUnitOnResize(t *testing.T) {
	setupPageTestOnDelay(t, "tests/on_resize")
}
