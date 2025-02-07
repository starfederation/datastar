package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleTemplCounter(t *testing.T) {
	setupPageTest(t, "examples/templ_counter", func(runner runnerFn) {
		runner("increment global", func(t *testing.T, page *rod.Page) {
			initial := page.MustElement("#container > div:nth-of-type(2) > div > div:nth-of-type(2)").MustText()

			wait := page.MustWaitRequestIdle()
			page.MustElementR("button", "Increment Global").MustClick()
			wait()

			result := page.MustElement("#container > div:nth-of-type(2) > div > div:nth-of-type(2)").MustText()

			assert.NotEqual(t, initial, result)
		})

		runner("increment user", func(t *testing.T, page *rod.Page) {
			initial := page.MustElement("#container > div:nth-of-type(2) > div:nth-of-type(2) > div:nth-of-type(2)").MustText()

			wait := page.MustWaitRequestIdle()
			page.MustElementR("button", "Increment User").MustClick()
			wait()

			result := page.MustElement("#container > div:nth-of-type(2) > div:nth-of-type(2) > div:nth-of-type(2)").MustText()

			assert.NotEqual(t, initial, result)
		})
	})
}
