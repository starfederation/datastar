package site

import (
	"context"
	"net/http"
	"strings"

	"github.com/a-h/templ"
	"github.com/go-chi/chi/v5"
	"github.com/samber/lo"
)

func setupReferences(ctx context.Context, router chi.Router) error {
	mdDataset, err := markdownRenders(ctx, "reference")
	if err != nil {
		return err
	}

	sidebarGroups := []*SidebarGroup{
		{
			Label: "Reference",
			Links: []*SidebarLink{
				{ID: "attribute_plugins"},
				{ID: "action_plugins"},
				{ID: "expression_context"},
				{ID: "sse_events"},
				{ID: "sdks"},
				{ID: "security"},
				{ID: "custom_builds"},
			},
		},
	}
	lo.ForEach(sidebarGroups, func(group *SidebarGroup, grpIdx int) {
		lo.ForEach(group.Links, func(link *SidebarLink, linkIdx int) {
			link.URL = templ.SafeURL("/reference/" + link.ID)
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

	router.Route("/reference", func(referenceRouter chi.Router) {
		referenceRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			http.Redirect(w, r, string(sidebarGroups[0].Links[0].URL), http.StatusFound)
		})

		// Redirect legacy pages to “Attribute Plugins”.
		legacyPages := []string{"core", "dom", "browser", "backend", "logic"}
		for _, page := range legacyPages {
			referenceRouter.Get("/plugins_"+page, func(w http.ResponseWriter, r *http.Request) {
				http.Redirect(w, r, "/reference/attribute_plugins", http.StatusMovedPermanently)
			})
		}

		// Redirect legacy “JavaScript APIs” page.
		referenceRouter.Get("/javascript_api", func(w http.ResponseWriter, r *http.Request) {
			http.Redirect(w, r, "/reference/expression_context", http.StatusMovedPermanently)
		})

		referenceRouter.Get("/{name}", func(w http.ResponseWriter, r *http.Request) {
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
