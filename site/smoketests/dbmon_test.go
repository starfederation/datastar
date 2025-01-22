package smoketests

import (
	"testing"
	"time"

	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestExampleDbmon(t *testing.T) {
	setupPageTest(t, "examples/dbmon", func(runner runnerFn) {
		runner("database monitoring", func(t *testing.T, page *rod.Page) {

			selector := "#dbmon > table"
			initial := page.MustElement(selector).MustHTML()
			// start := time.Now()

			time.Sleep(100 * time.Millisecond)
			page.MustWait("() => document.querySelector(`" + selector + "`).innerHTML !== `" + initial + "`")
			// t.Logf("TestExampleDbmon - database monitoring - MustWait duration: %s", time.Since(start))

			result := page.MustElement(selector).MustHTML()

			assert.NotEqual(t, initial, result)
		})
	})
}
