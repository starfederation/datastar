package site

import (
	"fmt"
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesDisableButton(examplesRouter chi.Router) error {
	examplesRouter.Get("/disable_button/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)

		fragment := fmt.Sprintf(`<div>The time is %s</div>`, time.Now().UTC().Format(time.RFC3339))
		sse.MergeFragments(fragment, datastar.WithSelectorID("container"), datastar.WithMergeAppend())

		time.Sleep(1 * time.Second)
		sse.MarshalAndMergeSignals(map[string]any{"shouldDisable": false})
	})

	return nil
}
