package site

import (
	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
	"net/http"
	"time"
)

type OptimisticUpdateSignals struct {
	Fetching bool   `json:"fetching"`
	Name     string `json:"name"`
}

func setupExamplesOptimisticUpdate(examplesRouter chi.Router) error {

	examplesRouter.Post("/optimistic_update/update_data", func(w http.ResponseWriter, r *http.Request) {
		signals := &OptimisticUpdateSignals{}
		datastar.ReadSignals(r, signals)
		sse := datastar.NewSSE(w, r)
		time.Sleep(2 * time.Second)
		sse.MergeSignals([]byte("{name:''}"))
		sse.RemoveFragments("#optimistic")
		sse.MergeFragmentTempl(updateSucess(signals.Name), datastar.WithSelector("#list"), datastar.WithMergePrepend())
	})

	return nil
}
