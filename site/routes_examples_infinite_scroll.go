package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesInfiniteScroll(examplesRouter chi.Router) error {

	examplesRouter.Get("/infinite_scroll/data", func(w http.ResponseWriter, r *http.Request) {
		signals := &infiniteScrollSignals{}
		if err := datastar.ReadSignals(r, signals); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		if signals.Limit < 1 || signals.Limit > 100 {
			signals.Limit = 10
		}

		sse := datastar.NewSSE(w, r)

		if signals.Offset == 0 {
			sse.MergeFragmentTempl(infiniteScrollAgents(signals))
		} else {
			if signals.Offset < 100 {
				sse.MergeFragmentTempl(infiniteScrollMore(signals))
				for i := 0; i < signals.Limit; i++ {

					sse.MergeFragmentTempl(
						infiniteScrollAgent(signals.Offset+i),
						datastar.WithSelectorID("click_to_load_rows"),
						datastar.WithMergeAppend(),
					)
				}
			} else {
				sse.MergeFragmentTempl(infiniteScrollRickroll())
			}
		}
	})

	return nil
}
