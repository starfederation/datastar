package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupTestsRemoveInitiatingFragment(testsRouter chi.Router) error {

	testsRouter.Delete("/remove_initiating_fragment/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.RemoveFragments("#clickable")
		sse.MergeFragments(`<code id="result">1</code>`)
	})

	return nil
}
