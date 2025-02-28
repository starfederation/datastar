package smoketests

import (
	"testing"
)

func TestUnitTextInputArray(t *testing.T) {
	setupPageTestOnPopulate(t, "tests/text_input_array", "bar")
}
