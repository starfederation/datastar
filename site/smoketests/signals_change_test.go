package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleSignalsChange(t *testing.T) {
	setupPageTest(t, "examples/signals_change", func(runner runnerFn) {
		runner("increment", func(t *testing.T, page *rod.Page) {
			initialClicks := page.MustElement("#local_clicks").MustText()
			initialServer := page.MustElement("#from_server").MustText()

			wait := page.MustWaitRequestIdle()
			page.MustElement("#increment").MustClick()
			wait()

			result := page.MustElement("#local_clicks").MustText()
			assert.NotEqual(t, initialClicks, result)

			result = page.MustElement("#from_server").MustText()
			assert.NotEqual(t, initialServer, result)
		})
	})
}
