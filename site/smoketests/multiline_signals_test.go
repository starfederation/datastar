package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleMultilineSignals(t *testing.T) {
	setupPageTest(t, "examples/multiline_signals", func(runner runnerFn) {
		runner("foo", func(t *testing.T, page *rod.Page) {
			input := page.MustElement("#foo").MustText()
			assert.Equal(t, "1234", input)
		})
		runner("baz", func(t *testing.T, page *rod.Page) {
			input := page.MustElement("#baz").MustText()
			assert.Equal(t, "2468", input)
		})

		runner("bar", func(t *testing.T, page *rod.Page) {
			input := page.MustElement("#bar").MustText()
			assert.Equal(t, "bar", input)
		})
	})
}
