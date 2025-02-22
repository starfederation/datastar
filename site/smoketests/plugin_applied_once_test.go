package smoketests

import (
	"testing"
)

func TestUnitPluginAppliedOnce(t *testing.T) {
	setupPageTestOnLoad(t, "tests/plugin_applied_once")
}
