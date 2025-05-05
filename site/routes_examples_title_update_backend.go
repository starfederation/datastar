package site

import (
	"fmt"
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesTitleUpdateBackend(examplesRouter chi.Router) error {
	examplesRouter.Get("/title_update_backend/updates", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		updateTitle := func() {
			frag := fmt.Sprintf(`<title>%s from server</title>`, time.Now().Format(time.TimeOnly))
			sse.MergeFragments(frag, datastar.WithSelector("title"))
		}

		updateTitle()
		t := time.NewTicker(1 * time.Second)
		for {
			select {
			case <-r.Context().Done():
				return
			case <-t.C:
				updateTitle()
			}
		}
	})

	return nil
}
