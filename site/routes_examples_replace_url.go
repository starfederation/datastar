package site

import (
	"math/rand"
	"net/http"
	"net/url"
	"strconv"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesReplaceURL(examplesRouter chi.Router) error {

	examplesRouter.Get("/replace_url_from_backend/updates", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		t := time.NewTicker(1 * time.Second)
		for {
			select {
			case <-r.Context().Done():
				return
			case <-t.C:
				page := rand.Intn(100)
				values := url.Values{
					"page": []string{strconv.Itoa(page)},
				}
				sse.ReplaceURLQuerystring(r, values)
			}
		}
	})

	return nil
}
