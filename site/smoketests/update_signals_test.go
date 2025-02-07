package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleUpdateSignals(t *testing.T) {
	t.Run("examples/update_signals - apply random signal patch", func(t *testing.T) {
		setupPageTest(t, "examples/update_signals", func(runner runnerFn) {
			runner("apply random signal patch", func(t *testing.T, page *rod.Page) {
				initial := page.MustElement("pre").MustHTML()

				wait := page.MustWaitRequestIdle()
				page.MustElementR("button", "Apply random signals").MustClick()

				wait()
				result := page.MustElement("pre").MustHTML()

				assert.NotEqual(t, initial, result)
			})
		})
	})

	t.Run("examples/update_signals - remove 2 random", func(t *testing.T) {
		setupPageTest(t, "examples/update_signals", func(runner runnerFn) {
			runner("remove 2 random", func(t *testing.T, page *rod.Page) {
				initial := page.MustElement("pre").MustHTML()

				wait := page.MustWaitRequestIdle()
				page.MustElementR("button", "Remove 2 random").MustClick()

				wait()
				result := page.MustElement("pre").MustHTML()

				assert.NotEqual(t, initial, result)
			})
		})
	})
}
