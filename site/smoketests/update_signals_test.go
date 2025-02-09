package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleUpdateSignals(t *testing.T) {
	setupPageTest(t, "examples/update_signals", func(runner runnerFn) {
		runner("apply random signal patch", func(t *testing.T, page *rod.Page) {
			initial := page.MustElement("pre").MustHTML()

			page.MustElementR("button", "Apply random signals").MustClick()

			result := page.MustElement("pre").MustHTML()

			assert.NotEqual(t, initial, result)
		})

		runner("remove 2 random", func(t *testing.T, page *rod.Page) {
			initial := page.MustElement("pre").MustHTML()

			page.MustElementR("button", "Remove 2 random").MustClick()

			page.MustWaitIdle()

			result := page.MustElement("pre").MustHTML()

			assert.NotEqual(t, initial, result)
		})
	})
}
