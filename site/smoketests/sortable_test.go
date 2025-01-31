package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/go-rod/rod/lib/proto"
	"github.com/stretchr/testify/assert"
)

func TestExampleSortable(t *testing.T) {
	t.Skip("Skip sortable test because it's flaky")
	setupPageTest(t, "examples/sortable", func(runner runnerFn) {
		runner("click and drag", func(t *testing.T, page *rod.Page) {
			initial := page.MustElement("#sortContainer").MustHTML()

			// DOM helper to see coords of items
			// document.addEventListener("mousemove", (e) => console.log({x: e.clientX, y: e.clientY}))
			page.MustElement("#sortContainer > div:nth-of-type(1)").MustClick()
			// mouse will be at the first item (x: 760.5, y: 665)

			page.Mouse.MustDown("left")
			page.Mouse.MoveLinear(proto.NewPoint(760.5, 735.0), 1)
			page.Mouse.MustUp("left")

			page.MustWaitIdle()

			result := page.MustElement("#sortContainer").MustHTML()

			assert.NotEqual(t, initial, result)
		})
	})
}
