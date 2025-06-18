package datastar

import "testing"

func TestAllValidElementMergeTypes(t *testing.T) {
	var err error
	for _, validType := range ValidElementPatchModes {
		if _, err = ElementPatchModeFromString(string(validType)); err != nil {
			t.Errorf("Expected %v to be a valid element merge type, but it was rejected: %v", validType, err)
		}
	}

	if _, err = ElementPatchModeFromString(""); err == nil {
		t.Errorf("Expected an empty string to be an invalid element merge type, but it was accepted")
	}

	if _, err = ElementPatchModeFromString("fakeType"); err == nil {
		t.Errorf("Expected a fake type to be an invalid element merge type, but it was accepted")
	}
}
