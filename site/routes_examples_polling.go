package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/gorilla/sessions"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupExamplesPolling(examplesRouter chi.Router, signals sessions.Store) error {
	sessionKey := "datastar-polling-example"

	examplesRouter.Get("/polling/interval", func(w http.ResponseWriter, r *http.Request) {
		session, err := signals.Get(r, sessionKey)
		if err != nil {
			return
		}

		count, ok := session.Values["count"].(int)
		if !ok {
			count = 0
		}
		newCount := count + 1

		sse := datastar.NewSSE(w, r)
		sse.MarshalAndMergeSignals(map[string]any{
			"count": newCount,
		})
	})

	return nil
}
