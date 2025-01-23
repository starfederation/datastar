package site

import (
	"fmt"
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	datastar "github.com/starfederation/datastar/sdk/go"
)

func setupExamplesQuizSlow(examplesRouter chi.Router) error {

	examplesRouter.Get("/quiz_slow/data", func(w http.ResponseWriter, r *http.Request) {
        type Signals struct {
            LastQuestionId int `json:"lastQuestionId1"`
        }
        signals := &Signals{
			LastQuestionId: 0,
		}
        if err := datastar.ReadSignals(r, signals); err != nil {
            http.Error(w, err.Error(), http.StatusBadRequest)
            return
        }

		sse := datastar.NewSSE(w, r)
		questionID := randomQuestionId(signals.LastQuestionId)
		QA := qaList[questionID]
		time.Sleep(2 * time.Second)
		sse.MergeFragments(fmt.Sprintf(`<div id="question3">%s</div>`, QA.Question))
		sse.MarshalAndMergeSignals(map[string]any{
			"response3":       "",
			"answer3":         QA.Answer,
			"lastQuestionId2": questionID,
		})
	})

	return nil
}
