package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/go-rod/rod/lib/input"
)

func TestExampleBindKeys(t *testing.T) {
	t.Skip("flaky test")
	setupPageTest(t, "examples/bind_keys", func(runner runnerFn) {
		runner("ctrl+k", func(t *testing.T, page *rod.Page) {
			page.MustElement("#demo").MustClick()
			wait, handle := page.MustHandleDialog()

			go page.KeyActions().Press(input.ControlLeft).Type(input.KeyK).MustDo()

			wait()
			handle(true, "")
		})

		runner("enter", func(t *testing.T, page *rod.Page) {
			page.MustElement("#demo").MustClick()
			wait, handle := page.MustHandleDialog()

			// flaky test caused by this go-routine
			go page.Keyboard.MustType(input.Enter)

			wait()
			handle(true, "")
		})
	})

}
