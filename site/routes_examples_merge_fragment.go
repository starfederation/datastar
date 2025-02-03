package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupExamplesMergeFragment(examplesRouter chi.Router) error {
	examplesRouter.Get("/merge_fragment/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.MergeFragmentTempl(mergeFragmentView())
	})

	return nil
}
