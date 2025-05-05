package site

import (
	"fmt"
	"net/http"
	"strconv"
	"strings"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

type ContactActive struct {
	ID       int    `json:"id"`
	IsActive bool   `json:"isActive"`
	Name     string `json:"name"`
	Email    string `json:"email"`
}

func starterActiveContacts() []*ContactActive {
	return []*ContactActive{
		{
			ID:       0,
			Name:     "Joe Smith",
			Email:    "joe@smith.org",
			IsActive: true,
		},
		{
			ID:       1,
			Name:     "Angie MacDowell",
			Email:    "angie@macdowell.org",
			IsActive: true,
		},
		{
			ID:       2,
			Name:     "Fuqua Tarkenton",
			Email:    "fuqua@tarkenton.org",
			IsActive: true,
		},
		{
			ID:       3,
			Name:     "Kim Yee",
			Email:    "kim@yee.org",
			IsActive: false,
		},
	}
}

type BulkUpdateSelectionSignals struct {
	Selections map[string]bool `json:"selections"`
}

func setupExamplesBulkUpdate(examplesRouter chi.Router) error {

	contacts := starterActiveContacts()

	defaultSelectionSignals := func() *BulkUpdateSelectionSignals {
		selections := map[string]bool{
			"all": false,
		}
		for i := range contacts {
			key := fmt.Sprintf("contact_%d", i)
			selections[key] = false
		}
		return &BulkUpdateSelectionSignals{
			Selections: selections,
		}
	}

	examplesRouter.Route("/bulk_update/data", func(dataRouter chi.Router) {
		dataRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			sse := datastar.NewSSE(w, r)
			sse.MergeFragmentTempl(bulkUpdateContacts(defaultSelectionSignals(), contacts))
		})

		setActivation := func(w http.ResponseWriter, r *http.Request, isActive bool) {
			signals := &BulkUpdateSelectionSignals{}
			if err := datastar.ReadSignals(r, signals); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}
			// logJSON("incoming", signals)

			sse := datastar.NewSSE(w, r)
			for key, wasSelected := range signals.Selections {
				const prefix = "contact_"
				if strings.HasPrefix(key, prefix) {
					idStr := strings.TrimPrefix(key, prefix)
					i, err := strconv.Atoi(idStr)
					if err != nil {
						http.Error(w, err.Error(), http.StatusBadRequest)
						return
					}

					c := contacts[i]
					wasChanged := c.IsActive != isActive
					if wasSelected {
						if wasChanged {
							c.IsActive = isActive
						}
					}

					sse.MergeFragmentTempl(
						bulkUpdateContact(i, c, wasChanged && wasSelected),
					)
				}
			}

			// Reset all selections
			for k := range signals.Selections {
				signals.Selections[k] = false
			}

			// logJSON("outgoing", signals)
			sse.MarshalAndMergeSignals(signals)
		}

		dataRouter.Put("/activate", func(w http.ResponseWriter, r *http.Request) {
			setActivation(w, r, true)
		})

		dataRouter.Put("/deactivate", func(w http.ResponseWriter, r *http.Request) {
			setActivation(w, r, false)
		})
	})

	return nil
}
