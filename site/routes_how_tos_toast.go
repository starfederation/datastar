package site

import (
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupHowTosToast(howTosToast chi.Router) error {

	howTosToast.Route("/toast/data", func(dataRouter chi.Router) {

		dataRouter.Post("/", func(w http.ResponseWriter, r *http.Request) {
			sse := datastar.NewSSE(w, r)
			sse.MergeFragments(`
				<li class="toast">Toasted</li>
			`).WithSelector(`
   				#toaster
   			`).WithMergeMode(
				Append
			)
		})
	})

	return nil
}
