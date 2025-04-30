package site

import (
	"fmt"
	"math/rand"
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesProgressBar(examplesRouter chi.Router) error {

	examplesRouter.Get("/progress_bar/data", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)

		progress := 0

		for progress < 100 {
			progress = min(100, progress+rand.Intn(10)+1)
			sse.MergeFragmentTempl(progressBarView(progress))
			sse.MergeFragments(
				fmt.Sprintf("<title>%d%%</title>", progress),
				datastar.WithSelector("title"),
			)
			time.Sleep(250 * time.Millisecond)
		}
		sse.MergeFragments(
			"<title>Progress Bar</title>",
			datastar.WithSelector("title"),
		)
	})

	return nil
}
