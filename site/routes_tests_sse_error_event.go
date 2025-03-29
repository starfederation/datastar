package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
)

func setupTestsSseErrorEvent(testsRouter chi.Router) error {
	
	testsRouter.Get("/sse_error_event/data", func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "Service Unavailable", http.StatusServiceUnavailable)
	})

	return nil
}
