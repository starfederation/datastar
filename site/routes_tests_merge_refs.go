package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupTestsMergeRefs(testsRouter chi.Router) error {
	
	testsRouter.Get("/merge_refs/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		c := mergeRefsTest()
		sse.MergeFragmentTempl(c)
	})

	return nil
}
