package site

import (
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/go-faker/faker/v4"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesDispatchCustomEvent(examplesRouter chi.Router) error {
	examplesRouter.Get("/dispatch_custom_event/events", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		ctx := r.Context()

		sse.ExecuteScript(`
document.addEventListener('example-event-from-server', (e) => {
    const container = document.getElementById('container');
    container.innerHTML = JSON.stringify(e.detail, null,2);
});
		`, datastar.WithExecuteScriptAutoRemove(false))

		type ExampleEventDetails struct {
			Name          string `faker:"name"`
			UserName      string `faker:"username"`
			UUIDHypenated string `faker:"uuid_hyphenated"`
		}

		t := time.NewTicker(1 * time.Second)

		for {
			select {
			case <-ctx.Done():
				return
			case <-t.C:
				detail := &ExampleEventDetails{}
				if err := faker.FakeData(detail); err != nil {
					sse.ConsoleError(err)
					return
				}

				sse.DispatchCustomEvent("example-event-from-server", detail)
			}
		}
	})

	return nil
}
