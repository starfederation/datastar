package site

import (
	"fmt"
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupHowTosPolling(howTosRouter chi.Router) error {

	howTosRouter.Route("/polling/data", func(dataRouter chi.Router) {

		dataRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			sse := datastar.NewSSE(w, r)
            currentTime := time.Now().Format("2006-01-02 15:04:05")
			currentSeconds := time.Now().Format("05")
			duration := 1
            if currentSeconds < "50" {
                duration = 5
            }
			sse.MergeFragments(fmt.Sprintf(`
				<div id="time" data-on-interval__duration.%ds="@get('/how_tos/polling/data')" class="text-primary font-bold">
					%s
				</div>
			`, duration, currentTime))
		})
	})

	return nil
}
