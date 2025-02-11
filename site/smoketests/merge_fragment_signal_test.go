package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestMergeFragment(t *testing.T) {
	setupPageTest(t, "tests/merge_fragment_signal", func(runner runnerFn) {
		runner("merge fragment signal", func(t *testing.T, page *rod.Page) {

			result := page.MustElement("#result")
			before, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, before, "1")

			btn := page.MustElement("button")
			btn.MustClick()
			page.MustWaitIdle()
			after, err := result.Text()
			assert.NoError(t, err)
			assert.Contains(t, after, "2")
		})
	})
}
