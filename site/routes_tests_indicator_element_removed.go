package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupTestsIndicatorElementRemoved(testsRouter chi.Router) error {

	testsRouter.Get("/indicator_element_removed/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.MergeFragments(`<div id="content" data-signals-result="1"><button class="btn">Removed</button></div>`)
	})

	return nil
}
