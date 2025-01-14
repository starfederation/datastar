package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func DisabledTestExampleLazyLoad(t *testing.T) {
	setupPageTest(t, "examples/lazy_load", func(runner runnerFn) {
		runner("observe lazy load", func(t *testing.T, page *rod.Page) {
			selector := "#lazy_load"

			initial := page.MustElement(selector).MustText()
			assert.Equal(t, "Loading...", initial)

			page.MustWait(`() => document.querySelector("` + selector + `").innerText === ""`)

			src := page.MustElement(selector).MustAttribute("src")

			assert.NotNil(t, src)
		})
	})
}
