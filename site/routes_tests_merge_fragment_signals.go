package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupTestsMergeFragmentSignals(testsRouter chi.Router) error {
	
	testsRouter.Get("/merge_fragment_signals/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.MergeFragments(`<div id="content" data-signals-result="1"></div>
}`)
	})

	return nil
}
