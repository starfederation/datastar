package smoketests

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestExampleClickToEdit(t *testing.T) {
	g := setup(t)

	page := g.page("examples/click_to_edit")
	assert.NotNil(t, page)

	t.Run("click to edit", func(t *testing.T) {
		editBtn := page.MustElement("#contact_1 > div > button")
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

		saveBtn := page.MustElement("#contact_1 > div > button")
		saveBtn.MustClick()

		firstNameLabel := page.MustElement("#contact_1 > label:nth-child(1)")
		assert.Equal(t, "First Name: Test", firstNameLabel.MustText())

		lastNameLabel := page.MustElement("#contact_1 > label:nth-child(2)")
		assert.Equal(t, "Last Name: Test", lastNameLabel.MustText())

		emailLabel := page.MustElement("#contact_1 > label:nth-child(3)")
		assert.Equal(t, "Email: Test", emailLabel.MustText())
	})
}
