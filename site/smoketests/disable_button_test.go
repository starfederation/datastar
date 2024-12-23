package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleDisableButton(t *testing.T) {
	setupPageTest(t, "examples/disable_button", func(runner runnerFn) {
		runner("disabled button click", func(t *testing.T, page *rod.Page) {
			btn := page.MustElement("#target")
			initial := btn.MustAttribute("data-attr-disabled")
			assert.Equal(t, "shouldDisable.value", *initial)

			btn.MustClick()
			result := btn.MustAttribute("disabled")
			assert.Equal(t, "true", *result)
		})
	})
}
