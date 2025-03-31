package site

import (
	"fmt"
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/samber/lo"
	datastar "github.com/starfederation/datastar/sdk/go"
	"github.com/zeebo/xxh3"
)

func setupExamplesMergeOptions(examplesRouter chi.Router) error {
	examplesRouter.Get("/merge_options/reset", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		sse.MergeFragmentTempl(mergeOptionsView())
	})

	brewerColorsBG := []string{
		"#a6cee3",
		"#1f78b4",
		"#b2df8a",
		"#33a02c",
		"#fb9a99",
		"#e31a1c",
		"#fdbf6f",
		"#ff7f00",
		"#cab2d6",
		"#6a3d9a",
		"#ffff99",
		"#b15928",
	}

	brewrColorsFG := []string{
		"black",
		"white",
		"black",
		"black",
		"black",
		"white",
		"black",
		"black",
		"black",
		"white",
		"black",
		"white",
	}

	examplesRouter.Get("/merge_options/{mergeMode}", func(w http.ResponseWriter, r *http.Request) {
		mergeModeRaw := chi.URLParam(r, "mergeMode")

		sse := datastar.NewSSE(w, r)

		switch mergeModeRaw {
		case "":
			http.Error(w, "missing merge mode", http.StatusBadRequest)
			return
		case "delete":
			sse.RemoveFragments("#target")
			return
		default:
			mergeMode, err := datastar.FragmentMergeTypeFromString(mergeModeRaw)
			if err != nil {
				sse.ConsoleError(err)
				return
			}

			if !lo.Contains(datastar.ValidFragmentMergeTypes, mergeMode) {
				http.Error(w, "invalid merge mode", http.StatusBadRequest)
				return
			}

			idx := lo.IndexOf(datastar.ValidFragmentMergeTypes, mergeMode)
			now := time.Now().UTC().Format(time.RFC3339)
			h := fmt.Sprint(xxh3.HashString(now))
			frag := mergeOptionsViewUpdate(brewerColorsBG[idx], brewrColorsFG[idx], h)
			sse.MergeFragmentTempl(frag, datastar.WithMergeMode(mergeMode), datastar.WithSelector("#imTheTarget"))
		}
	})

	return nil
}
