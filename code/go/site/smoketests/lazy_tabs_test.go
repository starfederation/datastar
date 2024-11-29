package smoketests

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestExampleLazyTabs(t *testing.T) {

	g := setup(t)

	page := g.page("examples/lazy_tabs")
	assert.NotNil(t, page)

	t.Run("click through lazy tabs", func(t *testing.T) {
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
}
