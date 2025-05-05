package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesClickToEdit(examplesRouter chi.Router) error {

	c1 := &ClickToEditContactSignals{}
	resetContact := func() {
		c1.FirstName = "John"
		c1.LastName = "Doe"
		c1.Email = "joe@blow.com"
	}
	resetContact()

	examplesRouter.Route("/click_to_edit/contact/{id}", func(contactRouter chi.Router) {
		contactRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			sse := datastar.NewSSE(w, r)
			sse.MergeFragmentTempl(setupExamplesClickToEditUserComponent(c1))
		})

		contactRouter.Get("/edit", func(w http.ResponseWriter, r *http.Request) {
			c := setupExamplesClickToEditUserEdit(c1)
			datastar.NewSSE(w, r).MergeFragmentTempl(c)
		})

		contactRouter.Patch("/reset", func(w http.ResponseWriter, r *http.Request) {
			resetContact()
			sse := datastar.NewSSE(w, r)
			sse.MergeFragmentTempl(setupExamplesClickToEditUserComponent(c1))
		})

		contactRouter.Put("/", func(w http.ResponseWriter, r *http.Request) {
			c := &ClickToEditContactSignals{}
			if err := datastar.ReadSignals(r, c); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}

			if err := sanitizer.Sanitize(c); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}

			c1 = c // update the contact
			datastar.NewSSE(w, r).MergeFragmentTempl(setupExamplesClickToEditUserComponent(c1))
		})
	})

	return nil
}
