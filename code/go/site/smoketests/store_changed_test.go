package smoketests

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestExampleStoreChanged(t *testing.T) {
	g := setup(t)

	page := g.page("examples/store_changed")
	assert.NotNil(t, page)

	t.Run("increment", func(t *testing.T) {
		initial := page.MustElement("#local_clicks").MustText()
		page.MustElement("#increment").MustClick()

		result := page.MustElement("#local_clicks").MustText()

		assert.NotEqual(t, initial, result)
	})
}
