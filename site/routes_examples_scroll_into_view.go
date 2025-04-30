package site

import (
	"fmt"
	"net/http"

	lorem "github.com/drhodes/golorem"
	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesScrollIntoView(examplesRouter chi.Router) error {
	paragraphCount := 20
	paragraphs := make([]string, paragraphCount)
	for i := 0; i < paragraphCount; i++ {
		paragraphs[i] = lorem.Paragraph(40, 60)
	}

	opts := []ScrollIntoViewOption{
		{"behavior", []string{"smooth", "instant", "auto"}},
		{"block", []string{"vstart", "vcenter", "vend", "vnearest"}},
		{"inline", []string{"hstart", "hcenter", "hend", "hnearest"}},
	}
	// inlineOpts := []string{"hstart", "hcenter", "hend", "hnearest"}

	examplesRouter.Route("/scroll_into_view/data", func(dataRouter chi.Router) {
		dataRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			sse := datastar.NewSSE(w, r)

			signals := &ScrollIntoViewSignals{
				Behavior: "smooth",
				Block:    "vcenter",
				Inline:   "hcenter",
			}

			sse.MergeFragmentTempl(scrollIntoViewView(paragraphs, opts, signals))
		})

		dataRouter.Put("/", func(w http.ResponseWriter, r *http.Request) {
			signals := &ScrollIntoViewSignals{}
			if err := datastar.ReadSignals(r, signals); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}

			sse := datastar.NewSSE(w, r)

			attr := "scroll-into-view"
			if signals.Behavior != "" {
				attr += "__" + signals.Behavior
			}
			if signals.Block != "" {
				attr += "__" + signals.Block
			}
			if signals.Inline != "" {
				attr += "__" + signals.Inline
			}

			updated := fmt.Sprintf(`<p id="p%d" data-%s></p>`, paragraphCount/2, attr)
			sse.MergeFragments(updated, datastar.WithMergeUpsertAttributes())
		})
	})

	return nil
}
