package site

import (
	"fmt"
	"net/http"
	"strconv"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

type ContactEdit struct {
	Name  string `json:"name"`
	Email string `json:"email"`
}

func starterEditContacts() []*ContactEdit {
	return []*ContactEdit{
		{
			Name:  "Joe Smith",
			Email: "joe@smith.org",
		},
		{
			Name:  "Angie MacDowell",
			Email: "angie@macdowell.org",
		},
		{
			Name:  "Fuqua Tarkenton",
			Email: "fuqua@tarkenton.org",
		},
		{
			Name:  "Kim Yee",
			Email: "kim@yee.org",
		},
	}
}

func setupExamplesEditRow(examplesRouter chi.Router) error {
	contacts := starterEditContacts()

	emptySignals := &EditRowSignals{EditRowIndex: -1}

	examplesRouter.Get("/edit_row/reset", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		contacts = starterEditContacts()
		sse.MergeFragmentTempl(EditRowContacts(contacts, emptySignals))
	})

	examplesRouter.Route("/edit_row/data", func(dataRouter chi.Router) {
		dataRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {

			sse := datastar.NewSSE(w, r)
			sse.MergeFragmentTempl(EditRowContacts(contacts, emptySignals))
		})

		dataRouter.Get("/{index}", func(w http.ResponseWriter, r *http.Request) {
			sse := datastar.NewSSE(w, r)
			indexStr := chi.URLParam(r, "index")
			i, err := strconv.Atoi(indexStr)
			if err != nil {
				http.Error(w, fmt.Sprintf("error parsing index: %s", err), http.StatusBadRequest)
				return
			}
			signals := &EditRowSignals{
				EditRowIndex: i,
				Name:         contacts[i].Name,
				Email:        contacts[i].Email,
			}

			sse.MergeFragmentTempl(EditRowContacts(contacts, signals))
		})
	})

	examplesRouter.Route("/edit_row/edit", func(editRouter chi.Router) {
		editRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			signals := &EditRowSignals{}
			if err := datastar.ReadSignals(r, &signals); err != nil {
				http.Error(w, fmt.Sprintf("error unmarshalling contact : %s", err), http.StatusBadRequest)
			}

			if signals.EditRowIndex < 0 || signals.EditRowIndex >= len(contacts) {
				http.Error(w, fmt.Sprintf("invalid index: %d", signals.EditRowIndex), http.StatusBadRequest)
				return
			}

			i := signals.EditRowIndex
			c := contacts[i]
			signals = &EditRowSignals{
				EditRowIndex: i,
				Name:         c.Name,
				Email:        c.Email,
			}

			sse := datastar.NewSSE(w, r)
			sse.MergeFragmentTempl(EditRowContacts(contacts, signals))
		})

		editRouter.Patch("/", func(w http.ResponseWriter, r *http.Request) {
			signals := &EditRowSignals{}
			if err := datastar.ReadSignals(r, &signals); err != nil {
				http.Error(w, fmt.Sprintf("error unmarshalling signals : %s", err), http.StatusBadRequest)
				return
			}

			if signals.EditRowIndex < 0 || signals.EditRowIndex >= len(contacts) {
				http.Error(w, fmt.Sprintf("invalid index: %d", signals.EditRowIndex), http.StatusBadRequest)
				return
			}
			i := signals.EditRowIndex
			c := contacts[i]
			c.Name = signals.Name
			c.Email = signals.Email

			sse := datastar.NewSSE(w, r)
			sse.MergeFragmentTempl(EditRowContacts(contacts, emptySignals))
		})
	})

	return nil
}
