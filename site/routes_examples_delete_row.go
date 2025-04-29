package site

import (
	"fmt"
	"net/http"
	"strconv"
	"sync"

	"github.com/go-chi/chi/v5"
	"github.com/samber/lo"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesDeleteRow(examplesRouter chi.Router) error {

	contacts := starterActiveContacts()
	mu := &sync.RWMutex{}

	examplesRouter.Route("/delete_row/data", func(dataRouter chi.Router) {
		dataRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			sse := datastar.NewSSE(w, r)
			mu.RLock()
			defer mu.RUnlock()
			sse.MergeFragmentTempl(deleteRowContacts(contacts))
		})

		dataRouter.Get("/reset", func(w http.ResponseWriter, r *http.Request) {
			sse := datastar.NewSSE(w, r)
			mu.Lock()
			defer mu.Unlock()
			contacts = starterActiveContacts()
			sse.MergeFragmentTempl(deleteRowContacts(contacts))
		})

		dataRouter.Delete("/{id}", func(w http.ResponseWriter, r *http.Request) {
			idStr := chi.URLParam(r, "id")
			id, err := strconv.Atoi(idStr)
			if err != nil {
				http.Error(w, fmt.Sprintf("error parsing index: %s", err), http.StatusBadRequest)
				return
			}

			mu.Lock()
			defer mu.Unlock()
			contacts = lo.Filter(contacts, func(cs *ContactActive, i int) bool {
				return cs.ID != id
			})
			datastar.NewSSE(w, r).RemoveFragments("#contact_" + idStr)
		})
	})

	return nil
}
