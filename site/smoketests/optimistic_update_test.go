package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleOptimisticUpdate(t *testing.T) {
	setupPageTest(t, "examples/optimistic_update", func(runner runnerFn) {
		runner("name", func(t *testing.T, page *rod.Page) {
			btn := page.MustElement("button")
			name := page.MustElement("#name")
			name.MustSelectAllText().MustInput("Test")

			listContainer := page.MustElement("#list")
			optimisticContainer := page.MustElement("#optimistic")
			greetingText := listContainer.MustText()
			assert.Equal(t, "", greetingText)

			btn.MustClick()
			page.MustWaitIdle()
			assert.Equal(t, true, optimisticContainer.MustVisible())
			assert.Equal(t, "Test", optimisticContainer.MustText())
			waitForSelectorToNotHaveInnerTextEqual(page, "#optimistic", "Test")

			assert.Equal(t, "", name.MustText())
			assert.Equal(t, "Test", listContainer.MustText())
			assert.Equal(t, false, optimisticContainer.MustVisible())
		})
	})
}
