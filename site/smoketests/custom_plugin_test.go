package smoketests

import (
	"testing"
)

func TestUnitCustomPlugin(t *testing.T) {
	setupPageTestOnLoad(t, "tests/custom_plugin")
}
