package site

import (
	"fmt"
	"math/rand"
	"net/http"

	"github.com/go-chi/chi/v5"
	"github.com/gorilla/sessions"
	datastar "github.com/starfederation/datastar/sdk/go"
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

func setupExamplesQuiz(examplesRouter chi.Router, signals sessions.Store) error {
	sessionKey := "datastar-quiz-example"

	examplesRouter.Get("/quiz/data", func(w http.ResponseWriter, r *http.Request) {
		session, err := signals.Get(r, sessionKey)
		if err != nil {
			return
		}

		// Get the last question ID from the session
		lastQuestionId, ok := session.Values["lastQuestionId1"].(int)
		if !ok {
			lastQuestionId = -1
		}

		sse := datastar.NewSSE(w, r)
		questionID := randomQuestionId(lastQuestionId)
		QA := qaList[questionID]
		sse.MergeFragments(fmt.Sprintf(`<div id="questionxxx2">%s</div>`, QA.Question))
		sse.MarshalAndMergeSignals(map[string]any{
			"response2":       "",
			"answer2":         QA.Answer,
			"lastQuestionId1": questionID,
		})
	})

	return nil
}
