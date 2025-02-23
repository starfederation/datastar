package smoketests

import (
	"testing"
)

func TestUnitAttrFalse(t *testing.T) {
	setupPageTestOnLoad(t, "tests/attr_false")
}
