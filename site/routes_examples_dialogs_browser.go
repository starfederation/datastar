package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesDialogsBrowser(examplesRouter chi.Router) error {

	examplesRouter.Get("/dialogs_browser/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		signals := &DialogBrowserSignals{Prompt: "foo"}
		sse.MergeFragmentTempl(DialogBrowserView(signals))
	})

	examplesRouter.Get("/dialogs_browser/sure", func(w http.ResponseWriter, r *http.Request) {
		signals := &DialogBrowserSignals{}
		if err := datastar.ReadSignals(r, signals); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		sse := datastar.NewSSE(w, r)
		sse.MergeFragmentTempl(DialogBrowserSure(signals))
	})

	return nil
}
