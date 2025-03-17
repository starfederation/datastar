package smoketests

import (
	"testing"
)

func TestUnitPluginNamePrefix(t *testing.T) {
	setupPageTestOnLoad(t, "tests/plugin_name_prefix")
}
