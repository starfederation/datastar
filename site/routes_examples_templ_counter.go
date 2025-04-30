package site

import (
	"net/http"
	"sync/atomic"

	"github.com/Jeffail/gabs/v2"
	"github.com/go-chi/chi/v5"
	"github.com/gorilla/sessions"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesTemplCounter(examplesRouter chi.Router, sessionSignals sessions.Store) error {

	var globalCounter atomic.Uint32
	const (
		sessionKey = "templ_counter"
		countKey   = "count"
	)

	userVal := func(r *http.Request) (uint32, *sessions.Session, error) {
		sess, err := sessionSignals.Get(r, sessionKey)
		if err != nil {
			return 0, nil, err
		}

		val, ok := sess.Values[countKey].(uint32)
		if !ok {
			val = 0
		}
		return val, sess, nil
	}

	examplesRouter.Get("/templ_counter/data", func(w http.ResponseWriter, r *http.Request) {
		userVal, _, err := userVal(r)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
		}

		signals := TemplCounterSignals{
			Global: globalCounter.Load(),
			User:   userVal,
		}

		c := templCounterExampleInitialContents(signals)
		datastar.NewSSE(w, r).MergeFragmentTempl(c)
	})

	updateGlobal := func(signals *gabs.Container) {
		signals.Set(globalCounter.Add(1), "global")
	}

	examplesRouter.Route("/templ_counter/increment", func(incrementRouter chi.Router) {
		incrementRouter.Post("/global", func(w http.ResponseWriter, r *http.Request) {
			update := gabs.New()
			updateGlobal(update)

			datastar.NewSSE(w, r).MarshalAndMergeSignals(update)
		})

		incrementRouter.Post("/user", func(w http.ResponseWriter, r *http.Request) {
			val, sess, err := userVal(r)
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
			}

			val++
			sess.Values[countKey] = val
			if err := sess.Save(r, w); err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
			}

			update := gabs.New()
			updateGlobal(update)
			update.Set(val, "user")

			datastar.NewSSE(w, r).MarshalAndMergeSignals(update)
		})
	})

	return nil
}
