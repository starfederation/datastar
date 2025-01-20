package site

import (
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupExamplesViewTransitionAPI(examplesRouter chi.Router) error {

	examplesRouter.Get("/view_transition_api/watch", func(w http.ResponseWriter, r *http.Request) {
		// You can comment out the below block and still persist the session
		transition := r.URL.Query().Get("transition")
		useSlide := transition == "slide"

		vt := datastar.WithUseViewTransitions(true)
		sse := datastar.NewSSE(w, r)
		sse.MergeFragmentTempl(viewTransitionAPIUpdate(useSlide), vt)

		t := time.NewTicker(time.Second)
		for {
			select {
			case <-r.Context().Done():
				return
			case <-t.C:
				sse.MergeFragmentTempl(viewTransitionAPIUpdate(useSlide), vt)
			}
		}
	})

	return nil
}
