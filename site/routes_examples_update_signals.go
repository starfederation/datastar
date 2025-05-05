package site

import (
	"fmt"
	"math/rand"
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesUpdateSignals(examplesRouter chi.Router) error {

	examplesRouter.Route("/update_signals/data", func(dataRouter chi.Router) {
		dataRouter.Post("/", func(w http.ResponseWriter, r *http.Request) {
			signals := map[string]any{}
			if err := datastar.ReadSignals(r, &signals); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}

			randKey := fmt.Sprintf("%d", rand.Intn(2<<16))
			signals[randKey] = time.Now().Format(time.RFC3339Nano)

			datastar.NewSSE(w, r).MarshalAndMergeSignals(signals)
		})

		dataRouter.Delete("/", func(w http.ResponseWriter, r *http.Request) {
			signals := map[string]any{}
			if err := datastar.ReadSignals(r, &signals); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}

			sse := datastar.NewSSE(w, r)

			if len(signals) == 0 {
				return
			}

			const maxDeletes = 2

			keysToDelete := make([]string, 0, len(signals))
			for k := range signals {
				keysToDelete = append(keysToDelete, k)
			}
			rand.Shuffle(len(keysToDelete), func(i, j int) {
				keysToDelete[i], keysToDelete[j] = keysToDelete[j], keysToDelete[i]
			})

			if len(keysToDelete) > maxDeletes {
				keysToDelete = keysToDelete[:maxDeletes]
			}

			sse.RemoveSignals(keysToDelete...)
		})
	})

	return nil
}
