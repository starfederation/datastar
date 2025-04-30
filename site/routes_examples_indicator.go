package site

import (
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesIndicator(examplesRouter chi.Router) error {

	examplesRouter.Get("/fetch_indicator/greet", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.MergeFragmentTempl(indicatorEmpty())
		time.Sleep(2 * time.Second)
		sse.MergeFragmentTempl(indicatorGreeting())
	})

	return nil
}
