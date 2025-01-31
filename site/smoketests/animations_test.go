package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleAnimations(t *testing.T) {
	t.Skip("Skip test because it's flaky")
	setupPageTest(t, "examples/animations", func(runner runnerFn) {
		runner("click and fade out", func(t *testing.T, page *rod.Page) {
			selector := "#fade_out_swap"
			btn := page.MustElement(selector)

			btn.MustClick()
			btn.WaitInvisible()

			exists, el, err := page.Has(selector)

			assert.Nil(t, el)
			assert.NoError(t, err)
			assert.False(t, exists)
		})

		runner("click and fade in", func(t *testing.T, page *rod.Page) {
			selector := "#fade_me_in"
			btn := page.MustElement(selector)

			btn.MustClick()
			btn.WaitVisible()

			exists, el, err := page.Has(selector)

			assert.NotNil(t, el)
			assert.NoError(t, err)
			assert.True(t, exists)
		})

		runner("in flight indicator", func(t *testing.T, page *rod.Page) {
			page.MustElement("input.flex-1").Input("test")

			page.MustElement("#submit_request_in_flight").MustClick()

			indicator := page.MustElement("#request_in_flight > div > div > div")
			indicator.WaitInvisible()

			result := page.MustElement("#request_in_flight").MustText()

			assert.Equal(t, "Submitted!", result)
		})
	})
}
