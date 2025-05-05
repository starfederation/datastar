package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesPolling(examplesRouter chi.Router) error {

	examplesRouter.Get("/polling/interval", func(w http.ResponseWriter, r *http.Request) {
		type Signals struct {
			Count int `json:"count"`
		}
		signals := &Signals{
			Count: 0,
		}
		if err := datastar.ReadSignals(r, signals); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		count := signals.Count + 1

		sse := datastar.NewSSE(w, r)
		sse.MarshalAndMergeSignals(map[string]any{
			"count": count,
		})
	})

	return nil
}
