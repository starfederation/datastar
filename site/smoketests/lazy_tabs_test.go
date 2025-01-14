package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func DisabledTestExampleLazyTabs(t *testing.T) {
	setupPageTest(t, "examples/lazy_tabs", func(runner runnerFn) {
		runner("click through lazy tabs", func(t *testing.T, page *rod.Page) {
			tabContents := page.MustElement("#tab_content")
			initial := tabContents.MustText()

			tabs := page.MustElement("#tabButtons")
			tabButtons := tabs.MustElements("button")

			currentText := initial
			for _, btn := range tabButtons {
				btn.MustClick()
				waitForSelectorToNotHaveInnerTextEqual(page, "#tabButtons", initial)

				result := page.MustElement("#tab_content").MustText()
				assert.NotEqual(t, currentText, result)

				currentText = result
			}
		})
	})
}
