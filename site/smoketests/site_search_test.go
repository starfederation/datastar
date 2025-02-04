package smoketests

import (
	"strings"
	"testing"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestSiteSearch(t *testing.T) {
	setupPageTest(t, "guide/getting_started", func(runner runnerFn) {
		runner("search for content", func(t *testing.T, page *rod.Page) {
			searchTerm := "htmx"
			page.MustElement("#header_site_search input[type='search']").MustInput(searchTerm)
			page.MustWait(`() => document.querySelector("#header_site_search .search-results").innerText !== "No results found."`)
			result := page.MustElement("#header_site_search .search-results > li").MustText()
			assert.Contains(t, strings.ToLower(result), searchTerm)
		})
	})
}
