package site

import (
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/delaneyj/toolbelt"
	"github.com/go-chi/chi/v5"
	"github.com/gorilla/sessions"
	"github.com/hashicorp/golang-lru/v2/expirable"
	"github.com/starfederation/datastar/sdk/go/datastar"
	"github.com/valyala/bytebufferpool"
)

func setupExamplesOfflineSync(examplesRouter chi.Router, signals sessions.Store) error {

	lruCache := expirable.NewLRU[string, string](100, nil, 5*time.Minute)

	sessionKey := "datastar-offline-sync-example"
	examplesRouter.Put("/offline_sync/sync", func(w http.ResponseWriter, r *http.Request) {
		buf := bytebufferpool.Get()
		defer bytebufferpool.Put(buf)
		if _, err := buf.ReadFrom(r.Body); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		signalsJSON := buf.String()

		var sessionID string
		session, err := signals.Get(r, sessionKey)
		if err != nil || len(session.Values) == 0 {
			sessionID = toolbelt.NextEncodedID()
			session.Values["id"] = sessionID
			if err := session.Save(r, w); err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
			}
		} else {
			sessionID = session.Values["id"].(string)
		}

		lruCache.Add(sessionID, signalsJSON)

		datastar.NewSSE(w, r).MergeFragments(fmt.Sprintf(`
<div id="results">
	<p>Synchronized offline data!</p>
	<p>at %s</p>
	<p>Session ID: %s</p>
	<p>Signals JSON: %s</p>
</div>
`,
			time.Now().Format(time.RFC3339),
			sessionID, signalsJSON,
		))

		log.Print("Syncing offline data!")
	})

	return nil
}
