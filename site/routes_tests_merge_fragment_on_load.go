package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupTestsMergeFragmentOnLoad(testsRouter chi.Router) error {

	testsRouter.Get("/merge_fragment_on_load/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.MergeFragments(`<div id="content" data-on-load="$result = 1"></div>`)
	})

	return nil
}
