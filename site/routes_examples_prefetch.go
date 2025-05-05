package site

import (
	"fmt"
	"math/rand"
	"net/http"
	"strconv"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

const pokemonCount = 151
const pokemonURLFormat = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/%d.png"

func setupExamplesPrefetch(examplesRouter chi.Router) error {

	examplesRouter.Get("/prefetch/load", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		c := prefetchCarousel(rand.Intn(pokemonCount) + 1)
		sse.MergeFragmentTempl(c)

		prefetchURLs := make([]string, 0, pokemonCount)
		for i := 0; i < pokemonCount; i++ {
			prefetchURLs = append(prefetchURLs, fmt.Sprintf(pokemonURLFormat, i+1))
		}
		sse.Prefetch(prefetchURLs...)
	})

	examplesRouter.Get("/prefetch/{idRaw}", func(w http.ResponseWriter, r *http.Request) {
		idRaw := chi.URLParam(r, "idRaw")
		id, err := strconv.Atoi(idRaw)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		sse := datastar.NewSSE(w, r)
		c := prefetchCarousel(id)
		sse.MergeFragmentTempl(c)
	})

	return nil
}
