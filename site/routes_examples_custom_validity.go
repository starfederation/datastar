package site

import (
	"fmt"
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupExamplesCustomValidity(examplesRouter chi.Router) error {
	examplesRouter.Get("/custom_validity/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.ExecuteScript(fmt.Sprintf(`alert('Form submitted')`))
	})

	return nil
}
