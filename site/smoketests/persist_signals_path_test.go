package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestPersistSignalsPathWildcard(t *testing.T) {
	setupPageTest(t, "tests/persist_signals_path", func(runner runnerFn) {
		runner("tests/persist_signals_path", func(t *testing.T, page *rod.Page) {
			page.MustWaitIdle()
			assert.Equal(t, "1", getLocalStoragePath(t, page, "foo"))
			assert.Equal(t, "1", getLocalStoragePath(t, page, "bar"))
			assert.Equal(t, "", getLocalStoragePath(t, page, "baz"))
		})
	})
}
