package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestMergeRefs(t *testing.T) {
	setupPageTest(t, "tests/merge_refs", func(runner runnerFn) {
		runner("merge refs", func(t *testing.T, page *rod.Page) {

			result := page.MustElement("#result")
			before, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, before, "foo")

			btn := page.MustElement("button")
			btn.MustClick()
			page.MustWaitIdle()
			after, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, after, "foo")
		})
	})
}
