package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupTestsMergeFragmentOuterMuplipleTargets(testsRouter chi.Router) error {
	
	testsRouter.Get("/merge_fragment_outer_multiple_targets/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.MergeFragments(
			`<div data-init="$result++"></div>`, 
			datastar.WithSelector(".target"),
			datastar.WithMergeOuter(),
		)
	})

	return nil
}
