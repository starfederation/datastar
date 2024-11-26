package smoketests

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestExampleRafUpdate(t *testing.T) {
	g := setup(t)

	page := g.page("examples/raf_update")
	assert.NotNil(t, page)
}
