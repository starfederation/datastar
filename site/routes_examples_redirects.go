package site

import (
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesRedirects(examplesRouter chi.Router) error {

	examplesRouter.Route("/redirects/data", func(dataRouter chi.Router) {

		dataRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			signals := &RedirectsSignals{
				RedirectTo: "/essays/grugs_around_fire",
			}
			sse := datastar.NewSSE(w, r)
			sse.MergeFragmentTempl(redirectsView(signals))
		})

		dataRouter.Post("/", func(w http.ResponseWriter, r *http.Request) {
			signals := &RedirectsSignals{}
			if err := datastar.ReadSignals(r, signals); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}

			sse := datastar.NewSSE(w, r)
			for i := 5; i > 0; i-- {
				sse.MergeFragmentf(`<div id="update">Redirecting in %d...</div>`, i)
				time.Sleep(500 * time.Millisecond)
			}
			sse.Redirect(signals.RedirectTo)
		})
	})

	return nil
}
