package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExamplePrefetch(t *testing.T) {
	setupPageTest(t, "examples/prefetch", func(runner runnerFn) {
		runner("carousel next", func(t *testing.T, page *rod.Page) {
			assert.NotNil(t, page)

			initial := page.MustElement("#carousel > img").MustProperty("src").Str()

			wait := page.MustWaitRequestIdle()
			page.MustElement("#carousel > button:nth-of-type(2)").MustClick()
			wait()

			result := page.MustElement("#carousel").MustProperty("src").Str()

			assert.NotEqual(t, initial, result)
		})

		runner("carousel previous", func(t *testing.T, page *rod.Page) {
			assert.NotNil(t, page)

			initial := page.MustElement("#carousel > img").MustProperty("src").Str()

			wait := page.MustWaitRequestIdle()
			page.MustElement("#carousel > button:nth-of-type(1)").MustClick()
			wait()

			result := page.MustElement("#carousel").MustProperty("src").Str()

			assert.NotEqual(t, initial, result)

		})
	})
}
