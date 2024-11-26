package smoketests

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestExampleWebComponent(t *testing.T) {
	g := setup(t)

	page := g.page("examples/web_component")
	assert.NotNil(t, page)
}
