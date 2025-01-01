package smoketests

import (
	"testing"
	"time"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleDisableButton(t *testing.T) {
	setupPageTest(t, "examples/disable_button", func(runner runnerFn) {
		runner("disabled button click", func(t *testing.T, page *rod.Page) {
			page.WaitStable(100 * time.Millisecond)
			btn := page.MustElement("#target")
			initial := btn.MustAttribute("data-attr-disabled")
			assert.NotNil(t, initial)
			assert.Equal(t, "$shouldDisable", *initial)

			btn.MustClick()
			result := btn.MustAttribute("disabled")
			assert.NotNil(t, result)
			assert.Equal(t, "true", *result)
		})
	})
}
