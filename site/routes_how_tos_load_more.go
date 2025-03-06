package site

import (
	"fmt"
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

type OffsetSignals struct {
	Offset int `json:"offset"`
}

func setupHowTosLoadMore(howTosRedirect chi.Router) error {

	howTosRedirect.Route("/load_more/data", func(dataRouter chi.Router) {

		dataRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			signals := &OffsetSignals{}
			if err := datastar.ReadSignals(r, signals); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
			}

			max := 5
			limit := 1
			offset := signals.Offset

			sse := datastar.NewSSE(w, r)

			if offset < max {
				newOffset := offset + limit
				if newOffset < max {
					sse.MergeSignals([]byte(fmt.Sprintf(`{offset: %d}`, newOffset)))
				} else {
					// TODO: set settle duration back to `0` or remove entirely
					sse.RemoveFragments(`#load-more`, datastar.WithRemoveSettleDuration(1))
				}
			
				sse.MergeFragments(fmt.Sprintf(`<div class="text-primary font-bold">Item %d</div>`, newOffset),
					datastar.WithSelectorID("list"),
					datastar.WithMergeMode(datastar.FragmentMergeModeAppend),
				)
			}
		})
	})

	return nil
}
