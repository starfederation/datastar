package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupTestsMergeFragmentInputValue(testsRouter chi.Router) error {

	testsRouter.Get("/merge_fragment_input_value/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.MergeFragments(`<div id="content">
			<input type="text" data-bind-result class="input input-bordered" />
  		</div>`)
		sse.ExecuteScript(`setTimeout(() => result.innerText = document.querySelector('[data-bind-result]').value === 'foo' ? 1 : 0, 0)`)
	})

	return nil
}
