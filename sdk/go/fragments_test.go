package datastar

import "testing"

func TestAllValidFragmentMergeTypes(t *testing.T) {
	validFragmentMergeTypes := [...]FragmentMergeMode{
		FragmentMergeModeMorph,
		FragmentMergeModeInner,
		FragmentMergeModeOuter,
		FragmentMergeModePrepend,
		FragmentMergeModeAppend,
		FragmentMergeModeBefore,
		FragmentMergeModeAfter,
		FragmentMergeModeUpsertAttributes,
	}

	var err error
	for _, validType := range validFragmentMergeTypes {
		if _, err = FragmentMergeTypeFromString(string(validType)); err != nil {
			t.Errorf("Expected %v to be a valid fragment merge type, but it was rejected: %v", validType, err)
		}
	}

	if _, err = FragmentMergeTypeFromString(""); err == nil {
		t.Errorf("Expected an empty string to be an invalid fragment merge type, but it was accepted")
	}

	if _, err = FragmentMergeTypeFromString("fakeType"); err == nil {
		t.Errorf("Expected a fake type to be an invalid fragment merge type, but it was accepted")
	}
}
