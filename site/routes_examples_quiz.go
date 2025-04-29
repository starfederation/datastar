package site

import (
	"fmt"
	"math/rand"
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

type QA struct {
	Question string
	Answer   string
}

var qaList = []QA{
	{"What do you put in a toaster?", "bread"},
	{"How many months have 28 days?", "twelve"},
	{"If you’re running in a race and pass the person in second place, what place are you in?", "second"},
	{"What do you get if you divide 30 by half and add 10?", "seventy"},
	{"What gets wetter the more it dries?", "towel"},
}

func randomQuestionId(lastQuestionId int) int {
	var newQuestionId int
	for {
		newQuestionId = rand.Intn(len(qaList))
		if newQuestionId != lastQuestionId {
			break
		}
	}
	return newQuestionId
}

func setupExamplesQuiz(examplesRouter chi.Router) error {

	examplesRouter.Get("/quiz/data", func(w http.ResponseWriter, r *http.Request) {
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
		sse.MergeFragments(fmt.Sprintf(`<div id="question2">%s</div>`, QA.Question))
		sse.MarshalAndMergeSignals(map[string]any{
			"response2":       "",
			"answer2":         QA.Answer,
			"lastQuestionId1": questionID,
		})
	})

	return nil
}
