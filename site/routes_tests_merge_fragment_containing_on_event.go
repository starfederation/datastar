package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupTestsMergeFragmentContainingOnEvent(testsRouter chi.Router) error {
	
	testsRouter.Get("/merge_fragment_containing_on_event/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.MergeFragments(`<div id="content" data-signals-hidden="false" data-show="!$hidden"><button data-on-click="$hidden = true" class="btn">Hide</button><input data-bind-name class="input input-bordered" /><button data-on-click="@get('/tests/merge_fragment_containing_on_event/data')" class="btn">Merge</button></div>`)
	})

	return nil
}
