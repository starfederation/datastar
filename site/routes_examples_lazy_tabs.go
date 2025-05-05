package site

import (
	"net/http"

	"github.com/a-h/templ"
	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesLazyTabs(examplesRouter chi.Router) error {
	tabs := make([]templ.Component, 8)
	for i := range tabs {
		tabs[i] = setupExamplesLazyTabsContent()
	}

	examplesRouter.Get("/lazy_tabs/data", func(w http.ResponseWriter, r *http.Request) {
		signals := &LazyTabsSignals{}
		if err := datastar.ReadSignals(r, signals); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		sse := datastar.NewSSE(w, r)
		component := setupExamplesLazyTabsComponent(len(tabs), tabs[signals.TabID], signals)
		sse.MergeFragmentTempl(component)
	})

	return nil
}
