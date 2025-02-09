package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleSignalsChanged(t *testing.T) {
	setupPageTest(t, "examples/signals_changed", func(runner runnerFn) {
		runner("increment", func(t *testing.T, page *rod.Page) {
			initial := page.MustElement("#local_clicks").MustText()
			page.MustElement("#increment").MustClick()

			result := page.MustElement("#local_clicks").MustText()

			assert.NotEqual(t, initial, result)
		})
	})
}
