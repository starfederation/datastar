package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestPersistSignalsPath(t *testing.T) {
	setupPageTest(t, "tests/persist_signals_path_wildcard", func(runner runnerFn) {
		runner("tests/persist_signals_path_wildcard", func(t *testing.T, page *rod.Page) {
			page.MustWaitIdle()
			assert.Equal(t, "{bar:{baz:1}", getLocalStoragePath(t, page, "foo"))
		})
	})
}
