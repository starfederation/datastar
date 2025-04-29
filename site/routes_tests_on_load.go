package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"

	"github.com/starfederation/datastar/sdk/go/datastar"
)

type OnLoadSignals struct {
	Fetching bool `json:"fetching"`
}

func setupTestsOnLoad(testsRouter chi.Router) error {

	testsRouter.Get("/on_load/data", func(w http.ResponseWriter, r *http.Request) {
		signals := &OnLoadSignals{}
		if err := datastar.ReadSignals(r, signals); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
		}

		sse := datastar.NewSSE(w, r)

		if signals.Fetching {
			sse.MergeFragments(`<code id="result">1</code>`)
		}
	})

	return nil
}
