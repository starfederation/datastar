package smoketests

import (
	"testing"
)

func TestUnitAliased(t *testing.T) {
	setupPageTestOnLoad(t, "tests/aliased")
}
