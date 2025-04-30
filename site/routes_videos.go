package site

import (
	"net/http"
	"strconv"

	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

type Video struct {
	Code  string
	Title string
}

func setupVideos(router chi.Router) error {
	videos := []Video{
		{Code: "4vb9C_K7zCU", Title: " Delaney exposes Alien Signals—not so extraterrestrial after all"},
		{Code: "0K71AyAF6E4", Title: "Real-time Hypermedia - Delaney Gillilan"},
		{Code: "OV6xS865pF0", Title: "Immutability & Event Sourcing "},
		{Code: "YRzPqPELZdY", Title: "DataStar, Engineering, and Web Apps with Delaney Gillilan"},
		{Code: "IrtBBqyDrJU", Title: "Codeiomorph with Micah "},
		{Code: "HbTFlUqELVc", Title: "Hypermedia at 144fps!?"},
		{Code: "DTURjpV2ZHQ", Title: "Getting to grips with Datastar "},
		{Code: "QPRigsY_4E8", Title: "054: Datastar with Delaney Gillilan"},
		{Code: "zu92P9wyUfI", Title: "Tuning the engine; Codeiomorph; Pub/Sub; NATS "},
		{Code: "a6ByFsFCN0c", Title: "Beta 3; Full-stack framework; CQRS"},
		{Code: "hUqFY9TQvdM", Title: "Datastar – The progressive performance framework"},
		{Code: "p4X02rEPkJY", Title: "What Datastar is Not, with JLarky"},
		{Code: "99wTA9sFEWE", Title: "Datastar v1"},
		{Code: "J-DzgNA6F-4", Title: "[D*#4] ‐ Build a dopamine hell with Datastar!"},
		{Code: "1cbqmVkzcJQ", Title: "[D*#3] ‐ Build a chess game with Datastar"},
		{Code: "t2NC7jGtD60", Title: "[D*#2] ‐ Build a Twitch room with Datastar "},
		{Code: "vLekrUywdRI", Title: "[D*#1] - Build a game with Python and Datastar!"},
		{Code: "UXPD3LblwVA", Title: "I recreated Google's worst product feature using datastar"},
		{Code: "aVjU1st-52g", Title: "Intro to Datastar (and Craft CMS)"},
		{Code: "FMKdE4QFyNk", Title: "Puffy does Realtime Hypermedia - Patrick Marchand - EuroBSDCon 2024"},
	}

	router.Route("/videos", func(videosRouter chi.Router) {
		videosRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			PageVideos(r, videos...).Render(r.Context(), w)
		})

		videosRouter.Get("/data/{index}", func(w http.ResponseWriter, r *http.Request) {
			sse := datastar.NewSSE(w, r)

			indexStr := chi.URLParam(r, "index")
			if indexStr == "" {
				http.Error(w, "Missing index", http.StatusBadRequest)
				return
			}

			index, err := strconv.Atoi(indexStr)
			if err != nil || index < 0 || index >= len(videos) {
				http.Error(w, "Invalid index", http.StatusBadRequest)
				return
			}
			code := videos[index].Code

			sse.MergeFragments(`<div id="video-` + indexStr + `" class="w-full max-w-xl aspect-video bg-black"><iframe class="w-full h-full" src="https://www.youtube.com/embed/` + code + `?autoplay=1" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>`)
		})
	})

	return nil
}
