package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleClickToEdit(t *testing.T) {
	setupPageTest(t, "examples/click_to_edit", func(runner runnerFn) {
		runner("click to edit", func(t *testing.T, page *rod.Page) {
			editBtn := page.MustElement("#contact_1 > div > button:nth-of-type(1)")
			editBtn.MustClick()

			firstNameInput := page.MustElement("#contact_1 > label > input")
			firstNameInput.MustSelectAllText().MustInput("")
			firstNameInput.MustInput("Test")

			lastNameInput := page.MustElement("#contact_1 > label:nth-of-type(2) > input")
			lastNameInput.MustSelectAllText().MustInput("")
			lastNameInput.MustInput("Test")

			emailInput := page.MustElement("#contact_1 > label:nth-of-type(3) > input")
			emailInput.MustSelectAllText().MustInput("")
			emailInput.MustInput("Test")

			// todo: this is not clicking save correctly, which causes the rest of the test to fail
			wait := page.MustWaitRequestIdle()
			saveBtn := page.MustElement("#contact_1 > div > button:nth-of-type(1)")
			saveBtn.MustClick()

			wait()

			firstNameLabel := page.MustElement("#contact_1 > label:nth-child(1)")
			assert.Equal(t, "First Name: Test", firstNameLabel.MustText())

			lastNameLabel := page.MustElement("#contact_1 > label:nth-child(2)")
			assert.Equal(t, "Last Name: Test", lastNameLabel.MustText())

			emailLabel := page.MustElement("#contact_1 > label:nth-child(3)")
			assert.Equal(t, "Email: Test", emailLabel.MustText())
		})
	})
}
