package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleBadApple(t *testing.T) {
	t.Skip("flaky test")

	setupPageTest(t, "examples/bad_apple", func(runner runnerFn) {
		runner("bad apple 30fps", func(t *testing.T, page *rod.Page) {
			selector := "#contents > div > pre"
			initial := page.MustElement(selector).MustText()
			// start := time.Now()

			page.MustWait(`() => document.querySelector("` + selector + `").innerText.includes("@")`)
			// t.Logf("TestExampleBadApple - bad apple 30fps - MustWait duration: %s", time.Since(start))

			result := page.MustElement(selector).MustText()

			assert.NotEqual(t, initial, result)
		})
	})

}
