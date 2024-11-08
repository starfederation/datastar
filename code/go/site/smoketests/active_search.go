package smoketests

import (
	"context"
	"errors"
	"fmt"
	"regexp"

	"github.com/playwright-community/playwright-go"
)

func activeSearch(ctx context.Context, page playwright.Page) error {
	searchPlaceholder := page.GetByTestId("search")
	searchPlaceholderExpect := expect.Locator(searchPlaceholder)
	if err := errors.Join(
		searchPlaceholderExpect.ToBeAttached(),
	// 	searchPlaceholderExpect.ToBeEmpty(),
	); err != nil {
		return fmt.Errorf("could not find search placeholder: %w", err)
	}

	tr := page.Locator("#active_search_rows tr")
	if err := expect.Locator(tr).ToHaveCount(10); err != nil {
		return fmt.Errorf("could not find 10 rows: %w", err)
	}

	nameReg := regexp.MustCompile("^[A-Za-z]+$")

	firstName, err := page.Locator("#active_search_rows tr:nth-child(5) > td:first-child").TextContent()
	if err != nil {
		return fmt.Errorf("could not find first name: %w", err)
	}
	if firstName == "" {
		return errors.New("first name is empty")
	}
	if !nameReg.MatchString(firstName) {
		return fmt.Errorf("first name is not valid: %w", err)
	}

	lastName, err := page.Locator("#active_search_rows tr:nth-child(5) > td:nth-child(2)").TextContent()
	if err != nil {
		return fmt.Errorf("could not find last name: %w", err)
	}
	if lastName == "" {
		return errors.New("last name is empty")
	}
	if !nameReg.MatchString(lastName) {
		return fmt.Errorf("last name is not valid: %w", err)
	}

	email, err := page.Locator("#active_search_rows tr:nth-child(5) > td:nth-child(3)").TextContent()
	if err != nil {
		return fmt.Errorf("could not find email: %w", err)
	}
	if email == "" {
		return errors.New("email is empty")
	}
	emailReg := regexp.MustCompile("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+.[a-zA-Z]{2,}$")
	if !emailReg.MatchString(email) {
		return fmt.Errorf("email is not valid: %w", err)
	}

	// scoreReg := regexp.MustCompile(`^\d+\.\d{2}$`)

	return nil
}
