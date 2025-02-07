package smoketests

import (
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleClasses(t *testing.T) {
	t.Skip("flaky test")

	setupPageTest(t, "examples/classes", func(runner runnerFn) {
		runner("observe class change", func(t *testing.T, page *rod.Page) {
			element := page.MustElement("article > div > div:nth-of-type(2)")

			initialClass, err := element.Attribute("class")
			if err != nil {
				t.Fatal("failed to get initial class: %w", err)
			}

			assert.Equal(t, "", *initialClass)

			page.MustWait(`() => document.querySelector("article > div > div:nth-of-type(2)").className !== "` + *initialClass + `"`)

			updatedClass, err := element.Attribute("class")
			if err != nil {
				t.Fatal("failed to get initial class: %w", err)
			}

			assert.Equal(t, "text-primary font-bold", *updatedClass)
		})
	})
}
