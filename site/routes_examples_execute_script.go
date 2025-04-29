package site

import (
	"fmt"
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesExecuteScript(examplesRouter chi.Router) error {
	const maxCount = 5

	examplesRouter.Get("/execute_script/log", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)

		for i := 0; i < maxCount; i++ {
			sse.ConsoleLogf("This is log message %d/%d", i+1, maxCount)
			time.Sleep(1 * time.Second)
		}
	})

	examplesRouter.Get("/execute_script/error", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)

		for i := 0; i < maxCount; i++ {
			err := fmt.Errorf("this is error message %d/%d", i+1, maxCount)
			sse.ConsoleError(err, datastar.WithExecuteScriptAutoRemove(false))
			time.Sleep(1 * time.Second)
		}
	})

	return nil
}
