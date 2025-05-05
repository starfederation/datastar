package site

import (
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupHowTosRedirect(howTosRedirect chi.Router) error {

	howTosRedirect.Route("/redirect/data", func(dataRouter chi.Router) {

		dataRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			sse := datastar.NewSSE(w, r)
			sse.MergeFragments(`
				<span id="indicator">Redirecting in 3 seconds...</span>
			`)
			time.Sleep(3 * time.Second)
			sse.Redirect("/guide")
		})
	})

	return nil
}
