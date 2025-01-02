package site

import (
	"fmt"
	"net/http"
	"strings"
	"time"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupExamplesSignalsIfMissing(examplesRouter chi.Router) error {

	examplesRouter.Get("/signals_ifmissing/updates", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)

		t := time.NewTicker(1 * time.Second)
		defer t.Stop()

		i := 1234
		for {
			select {
			case <-r.Context().Done():
				return
			case <-t.C:
				signals := fmt.Sprintf("{id:%d}", i)

				switch i % 2 {
				case 0:
					fragment := strings.TrimSpace(fmt.Sprintf(`
<div
	id="placeholder"
	data-signals__ifmissing=%q
	data-text="$id"
></div>
					`, signals))
					sse.MergeFragments(fragment, datastar.WithMergeUpsertAttributes())
				case 1:
					sse.MarshalAndMergeSignalsIfMissing(signals)
				}
				i++
			}
		}

	})

	return nil
}
