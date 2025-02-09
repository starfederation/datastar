package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleInfiniteScroll(t *testing.T) {
	setupPageTest(t, "examples/infinite_scroll", func(runner runnerFn) {
		runner("trigger infinite scroll", func(t *testing.T, page *rod.Page) {
			table := page.MustElement("#infinite_scroll > table")
			rows := table.MustElements("tr")
			assert.Len(t, rows, 11)

			page.MustElement("#infinite_scroll").MustClick()
			page.Mouse.MustScroll(0, 2000)

			updatedRows := table.MustElements("tr")
			assert.Len(t, updatedRows, 21)
		})
	})
}
