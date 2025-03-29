package smoketests

import (
	"testing"

	"github.com/Jeffail/gabs/v2"
	"github.com/go-rod/rod"
	"github.com/stretchr/testify/assert"
)

func TestPersistSignals(t *testing.T) {
	setupPageTest(t, "tests/persist_signals", func(runner runnerFn) {
		runner("tests/persist_signals", func(t *testing.T, page *rod.Page) {
			checkLocalStorage := func(path string) string {
				fromLocalStorage := page.MustEval(`k => localStorage[k]`, "datastar")
				marshalled := fromLocalStorage.String()
				c, err := gabs.ParseJSON([]byte(marshalled))
				assert.NoError(t, err)
				actual, ok := c.Path(path).Data().(string)
				assert.True(t, ok)
				return actual
			}

			page.MustWaitIdle()
			assert.Equal(t, "", checkLocalStorage("foo"))
			assert.Equal(t, "", checkLocalStorage("bar"))
			assert.Equal(t, "", checkLocalStorage("baz"))
			page.MustWaitIdle()
			foo := page.MustElement("#foo")
			bar := page.MustElement("#bar")
			foo.MustInput("1")
			bar.MustInput("1")
			page.MustWaitIdle()
			assert.Equal(t, "1", checkLocalStorage("foo"))
			assert.Equal(t, "1", checkLocalStorage("bar"))
			assert.Equal(t, "", checkLocalStorage("baz"))
		})
	})
}
