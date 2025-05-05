package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupTestsRemoveFragment(testsRouter chi.Router) error {

	testsRouter.Delete("/remove_fragment/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.RemoveFragments("#remove")
	})

	return nil
}
