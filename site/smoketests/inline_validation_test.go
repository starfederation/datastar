package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/go-rod/rod/lib/input"
	"github.com/stretchr/testify/assert"
)

func TestExampleInlineValidation(t *testing.T) {
	setupPageTest(t, "examples/inline_validation", func(runner runnerFn) {
		runner("sign up", func(t *testing.T, page *rod.Page) {
			// email
			emailValidatorSelector := "div.form-control:nth-child(1) > label:nth-child(3)"
			emailValidatorMsg := page.MustElement(emailValidatorSelector).MustText()
			assert.Equal(t, "Email '' is already taken or is invalid. Please enter another email.", emailValidatorMsg)

			emailInput := page.MustElement("#inline_validation > div > div:nth-of-type(1) > input")
			emailInput.MustInput("test@test.com")

			// first name
			firstNameValidatorSelector := "div.form-control:nth-child(2) > label:nth-child(3)"
			firstNameValidatorSelectorMsg := page.MustElement(firstNameValidatorSelector).MustText()
			assert.Equal(t, "First name must be at least 2 characters.", firstNameValidatorSelectorMsg)

			firstNameInput := page.MustElement("#inline_validation > div > div:nth-of-type(2) > input")
			firstNameInput.MustInput("test")

			// last name
			lastNameValidatorSelector := "div.form-control:nth-child(3) > label:nth-child(3)"
			lastNameValidatorSelectorMsg := page.MustElement(lastNameValidatorSelector).MustText()
			assert.Equal(t, "Last name must be at least 2 characters.", lastNameValidatorSelectorMsg)

			lastNameInput := page.MustElement("#inline_validation > div > div:nth-of-type(3) > input")
			lastNameInput.MustInput("test")

			emailInput.Type(input.Enter) // will actually trigger the debounce

			page.MustWaitStable()

			emailNotValid, _, _ := page.Has(emailValidatorSelector)
			assert.False(t, emailNotValid)

			firstNameNotValid, _, _ := page.Has(firstNameValidatorSelector)
			assert.False(t, firstNameNotValid)

			lastNameNotValid, _, _ := page.Has(lastNameValidatorSelector)
			assert.False(t, lastNameNotValid)
		})
	})
}
