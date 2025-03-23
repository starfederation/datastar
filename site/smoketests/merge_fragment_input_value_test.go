package smoketests

import (
	"testing"
)

func TestUnitMergeFragmentInputValue(t *testing.T) {
	setupPageTestOnPopulateAndClick(t, "tests/merge_fragment_input_value", "foo")
}
