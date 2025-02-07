package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleSignalsChange(t *testing.T) {
	setupPageTest(t, "examples/signals_change", func(runner runnerFn) {
		runner("increment", func(t *testing.T, page *rod.Page) {
			initial := page.MustElement("#local_clicks").MustText()

			wait := page.MustWaitRequestIdle()
			page.MustElement("#increment").MustClick()
			wait()

			result := page.MustElement("#local_clicks").MustText()

			assert.NotEqual(t, initial, result)
		})
	})
}
