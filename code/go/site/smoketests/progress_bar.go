package smoketests

import (
	"context"
	"fmt"

	"github.com/playwright-community/playwright-go"
)

func progressbar(ctx context.Context, page playwright.Page) error {

	progressElement := page.Locator("#progress-bar")
	if err := expect.Locator(progressElement).ToContainText("Progress Bar"); err != nil {
		return fmt.Errorf("could not find progress bar: %w", err)
	}

	svgElement := page.Locator("#progress_bar > svg")
	if err := expect.Locator(svgElement).ToBeAttached(); err != nil {
		return fmt.Errorf("could not find svg element: %w", err)
	}

	settling := page.Locator("div#progress_bar.datastar-swapping.datastar-settling")

	if err := settling.WaitFor(playwright.LocatorWaitForOptions{
		State: playwright.WaitForSelectorStateAttached,
	}); err != nil {
		return fmt.Errorf("could not find settling class: %w", err)
	}

	if err := settling.WaitFor(playwright.LocatorWaitForOptions{
		State: playwright.WaitForSelectorStateDetached,
	}); err != nil {
		return fmt.Errorf("could not find settling class: %w", err)
	}

	completedLink := page.Locator("#completed_link")
	if err := completedLink.WaitFor(playwright.LocatorWaitForOptions{
		State: playwright.WaitForSelectorStateVisible,
	}); err != nil {
		return fmt.Errorf("could not find completed link: %w", err)
	}

	return nil
}
