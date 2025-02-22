package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupTestsMergeFragmentWhitespace(testsRouter chi.Router) error {
	
	testsRouter.Get("/merge_fragment_whitespace/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.MergeFragments(`<pre id="fragment" data-signals-result="ctx.el.innerHTML == 'foo\n    bar' ? 1 : 0">foo
    bar</pre>`)
	})

	return nil
}
