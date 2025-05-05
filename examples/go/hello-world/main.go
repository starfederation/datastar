package main

import (
	_ "embed"
	"net/http"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

//go:embed hello-world.html
var helloWorldHTML []byte

func main() {
	r := chi.NewRouter()

	const message = "Hello, world!"
	type Store struct {
		Delay time.Duration `json:"delay"` // delay in milliseconds between each character of the message.
	}

	r.Get("/", func(w http.ResponseWriter, r *http.Request) {
		w.Write(helloWorldHTML)
	})

	r.Get("/hello-world", func(w http.ResponseWriter, r *http.Request) {
		store := &Store{}
		if err := datastar.ReadSignals(r, store); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		sse := datastar.NewSSE(w, r)

		for i := 0; i < len(message); i++ {
			sse.MergeFragments(`<div id="message">` + message[:i+1] + `</div>`)
			time.Sleep(store.Delay * time.Millisecond)
		}
	})

	http.ListenAndServe(":8080", r)
}
