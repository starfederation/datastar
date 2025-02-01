package smoketests

import (
	"testing"
	"time"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleDialogsBrowser(t *testing.T) {
	t.Skip("Skip test because it's flaky")
	setupPageTest(t, "examples/dialogs_browser", func(runner runnerFn) {
		runner("launch dialog", func(t *testing.T, page *rod.Page) {
			btn := page.MustElement("#dialogs")
			page.MustWaitIdle()

			wait, handle := page.MustHandleDialog()
			go btn.MustClick()

			//i don't know why this is needed but wait isn't enough
			time.Sleep(1 * time.Second)

			wait()
			handle(true, "test")
			handle(true, "")
			page.MustWaitIdle()

			confirmation := page.MustElement("#confirmation")
			confirmationText := confirmation.MustText()
			assert.Equal(t, "test", confirmationText)
		})
	})
}
