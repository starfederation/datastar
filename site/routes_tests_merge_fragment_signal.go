package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupTestsMergeFragmentSignal(testsRouter chi.Router) error {
	
	testsRouter.Get("/merge_fragment_signal/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		c := mergeFragmentSignalTest()
		sse.MergeFragmentTempl(c)
	})

	return nil
}
