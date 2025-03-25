package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupTestsSseEvents(testsRouter chi.Router) error {
	
	testsRouter.Get("/sse_events/data", func(w http.ResponseWriter, r *http.Request) {
		datastar.NewSSE(w, r)
	})

	return nil
}
