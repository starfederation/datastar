package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesClickToLoad(examplesRouter chi.Router) error {

	examplesRouter.Get("/click_to_load/data", func(w http.ResponseWriter, r *http.Request) {
		signals := &ClickToLoadSignals{}
		if err := datastar.ReadSignals(r, signals); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
		}
		if signals.Limit < 1 {
			signals.Limit = 10
		} else if signals.Limit > 100 {
			signals.Limit = 100
		}
		if signals.Offset < 0 {
			signals.Offset = 0
		}

		sse := datastar.NewSSE(w, r)

		if signals.Offset == 0 {
			sse.MergeFragmentTempl(ClickToEditAgentsTable(signals))
		} else {
			sse.MergeFragmentTempl(ClickToLoadMoreButton(signals))
			for i := 0; i < signals.Limit; i++ {
				// log.Printf("ClickToLoadAgentRow: %d", signals.Offset+i)
				sse.MergeFragmentTempl(
					ClickToLoadAgentRow(signals.Offset+i),
					datastar.WithSelectorID("click_to_load_rows"),
					datastar.WithMergeAppend(),
				)
			}
		}
	})

	return nil
}
