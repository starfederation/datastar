package site

import (
	"context"
	"net/http"
	"strings"

	"github.com/a-h/templ"
	"github.com/go-chi/chi/v5"
	"github.com/samber/lo"
)

func setupGuide(ctx context.Context, router chi.Router) error {
	mdDataset, err := markdownRenders(ctx, "guide")
	if err != nil {
		return err
	}

	sidebarGroups := []*SidebarGroup{
		{
			Label: "Guide",
			Links: []*SidebarLink{
				{ID: "getting_started"},
				{ID: "going_deeper"},
				{ID: "datastar_expressions"},
				{ID: "stop_overcomplicating_it"},
			},
		},
	}
	lo.ForEach(sidebarGroups, func(group *SidebarGroup, grpIdx int) {
		lo.ForEach(group.Links, func(link *SidebarLink, linkIdx int) {
			link.URL = templ.SafeURL("/guide/" + link.ID)
			link.Label = strings.ToUpper(strings.ReplaceAll(link.ID, "_", " "))

			if linkIdx > 0 {
				link.Prev = group.Links[linkIdx-1]
			} else if grpIdx > 0 {
				prvGrp := sidebarGroups[grpIdx-1]
				link.Prev = prvGrp.Links[len(prvGrp.Links)-1]
			}

			if linkIdx < len(group.Links)-1 {
				link.Next = group.Links[linkIdx+1]
			} else if grpIdx < len(sidebarGroups)-1 {
				nxtGrp := sidebarGroups[grpIdx+1]
				link.Next = nxtGrp.Links[0]
			}
		})
	})

	router.Route("/guide", func(guideRouter chi.Router) {
		guideRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			http.Redirect(w, r, string(sidebarGroups[0].Links[0].URL), http.StatusFound)
		})

		// Redirect legacy pages to “Going Deeper”.
		legacyPages := []string{"go_deeper", "howl", "batteries_included", "streaming_backend"}
		for _, page := range legacyPages {
			guideRouter.Get("/"+page, func(w http.ResponseWriter, r *http.Request) {
				http.Redirect(w, r, "/guide/going_deeper", http.StatusMovedPermanently)
			})
		}

		guideRouter.Get("/{name}", func(w http.ResponseWriter, r *http.Request) {
			name := chi.URLParam(r, "name")
			mdData, ok := mdDataset[name]
			if !ok {
				http.Error(w, "not found", http.StatusNotFound)
				return
			}

			var currentLink *SidebarLink
			for _, group := range sidebarGroups {
				for _, link := range group.Links {
					if link.ID == name {
						currentLink = link
						break
					}
				}
			}

			SidebarPage(r, sidebarGroups, currentLink, mdData.Title, mdData.Description, mdData.Contents).Render(r.Context(), w)
		})
	})

	return nil

}
