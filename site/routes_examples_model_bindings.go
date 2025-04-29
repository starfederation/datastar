package site

import (
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesModelBinding(examplesRouter chi.Router) error {
	examplesRouter.Get("/model_binding/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)

		signals := &ModelBindingSignals{
			BindText:      "foo",
			BindNumber:    42,
			BindSelection: 1,
			BindBool:      true,
		}

		sse.MergeFragmentTempl(ModelBindingView(7, signals))
	})

	return nil
}
