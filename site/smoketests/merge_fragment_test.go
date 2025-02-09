package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestMergeFragment(t *testing.T) {
	setupPageTest(t, "examples/merge_fragment", func(runner runnerFn) {
		runner("merge fragment", func(t *testing.T, page *rod.Page) {

			example := page.MustElement("#example")

			buttonCount := func() int {
				exampleButtons := example.MustElements("button")
				return len(exampleButtons)
			}

			// There should be 1 buttons in the example
			assert.Equal(t, 1, buttonCount())

			// DOM helper to see coords of items
			// document.addEventListener("mousemove", (e) => console.log({x: e.clientX, y: e.clientY}))
			fragment := page.MustElement("#fragment")
			before, err := fragment.Text()
			assert.NoError(t, err)
			assert.Contains(t, before, "bar")

			btn := fragment.MustElement("button")
			btn.MustClick()
			// mouse will be at the first item (x: 760.5, y: 665)

			page.MustWaitIdle()

			after, err := fragment.Text()
			assert.NoError(t, err)
			assert.Contains(t, after, "baz")

			// There should still be 1 buttons in the example
			assert.Equal(t, 1, buttonCount())
		})
	})
}
