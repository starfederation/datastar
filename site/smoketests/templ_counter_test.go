package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleTemplCounter(t *testing.T) {
	t.Skip("flaky test")
	setupPageTest(t, "examples/templ_counter", func(runner runnerFn) {
		runner("increment global", func(t *testing.T, page *rod.Page) {
			initial := page.MustElement("#container > div:nth-of-type(2) > div > div:nth-of-type(2)").MustText()

			page.MustElementR("button", "Increment Global").MustClick()

			result := page.MustElement("#container > div:nth-of-type(2) > div > div:nth-of-type(2)").MustText()

			assert.NotEqual(t, initial, result)
		})

		runner("increment user", func(t *testing.T, page *rod.Page) {
			initial := page.MustElement("#container > div:nth-of-type(2) > div:nth-of-type(2) > div:nth-of-type(2)").MustText()

			page.MustElementR("button", "Increment User").MustClick()

			result := page.MustElement("#container > div:nth-of-type(2) > div:nth-of-type(2) > div:nth-of-type(2)").MustText()

			assert.NotEqual(t, initial, result)
		})
	})
}
