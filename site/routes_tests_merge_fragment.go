package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupTestsMergeFragment(testsRouter chi.Router) error {
	
	testsRouter.Get("/merge_fragment/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.MergeFragments(`<code id="result">1</code>`)
	})

	return nil
}
