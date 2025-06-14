package site

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"strings"

	"github.com/a-h/templ"
	"github.com/go-chi/chi/v5"
	"github.com/samber/lo"
)

func setupHowTos(ctx context.Context, router chi.Router) error {
	mdDataset, err := markdownRenders(ctx, "how_tos")
	if err != nil {
		return err
	}

	sidebarGroups := []*SidebarGroup{
		{
			Label: "How Tos",
			Links: []*SidebarLink{
				{ID: "how_to_bind_keydown_events_to_specific_keys"},
				{ID: "how_to_load_more_list_items"},
				{ID: "how_to_poll_the_backend_at_regular_intervals"},
				{ID: "how_to_redirect_the_page_from_the_backend"},
				{ID: "how_to_send_toast_notifications_from_the_backend"},
			},
		},
	}
	lo.ForEach(sidebarGroups, func(group *SidebarGroup, grpIdx int) {
		lo.ForEach(group.Links, func(link *SidebarLink, linkIdx int) {
			link.URL = templ.SafeURL("/how_tos/" + link.ID)
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

	router.Route("/how_tos", func(howTosRouter chi.Router) {
		howTosRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			http.Redirect(w, r, string(sidebarGroups[0].Links[0].URL), http.StatusFound)
		})

		howTosRouter.Get("/{name}", func(w http.ResponseWriter, r *http.Request) {
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

		if err := errors.Join(
			setupHowTosLoadMore(howTosRouter),
			setupHowTosPolling(howTosRouter),
			setupHowTosRedirect(howTosRouter),
		); err != nil {
			panic(fmt.Sprintf("error setting up examples routes: %s", err))
		}
	})

	return nil

}
