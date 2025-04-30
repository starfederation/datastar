package smoketests

import (
	"testing"

	"github.com/Jeffail/gabs/v2"
	"github.com/go-rod/rod"
	"github.com/starfederation/datastar/sdk/go/datastar"
	"github.com/stretchr/testify/assert"
)

func TestExampleSessionStorage(t *testing.T) {
	setupPageTest(t, "examples/session_storage", func(runner runnerFn) {
		runner("session storage", func(t *testing.T, page *rod.Page) {

			page.MustWaitIdle()

			checkSessionStorage := func() float64 {
				ss := page.MustEval(`k => sessionStorage[k]`, datastar.DatastarKey)
				marshalled := ss.String()
				c, err := gabs.ParseJSON([]byte(marshalled))
				assert.NoError(t, err)
				actual := c.Path("sessionId").Data().(float64)
				return actual
			}
			assert.Equal(t, float64(1234), checkSessionStorage())
		})
	})
}
