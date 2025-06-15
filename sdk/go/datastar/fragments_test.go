package datastar

import "testing"

func TestAllValidElementMergeTypes(t *testing.T) {
	var err error
	for _, validType := range ValidElementMergeTypes {
		if _, err = ElementMergeTypeFromString(string(validType)); err != nil {
			t.Errorf("Expected %v to be a valid element merge type, but it was rejected: %v", validType, err)
		}
	}

	if _, err = ElementMergeTypeFromString(""); err == nil {
		t.Errorf("Expected an empty string to be an invalid element merge type, but it was accepted")
	}

	if _, err = ElementMergeTypeFromString("fakeType"); err == nil {
		t.Errorf("Expected a fake type to be an invalid element merge type, but it was accepted")
	}
}
