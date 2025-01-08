package site

import (
	"context"
	"net/http"
	"net/url"
	"slices"
	"strings"

	"github.com/a-h/templ"
	"github.com/go-chi/chi/v5"
	"github.com/microcosm-cc/bluemonday"
	"github.com/samber/lo"
)

func setupErrors(ctx context.Context, router chi.Router) error {
	santizePolicy := bluemonday.UGCPolicy()

	mdDataset, err := markdownRenders(ctx, "errors")
	if err != nil {
		return err
	}

	sidebarLinks := make([]*SidebarLink, 0, len(mdDataset))
	for id := range mdDataset {
		sidebarLinks = append(sidebarLinks, &SidebarLink{ID: id})
	}
	slices.SortFunc(sidebarLinks, func(a, b *SidebarLink) int {
		return strings.Compare(a.ID, b.ID)
	})

	sidebarGroups := []*SidebarGroup{
		{
			Label: "Errors",
			Links: sidebarLinks,
		},
	}
	lo.ForEach(sidebarGroups, func(group *SidebarGroup, grpIdx int) {
		lo.ForEach(group.Links, func(link *SidebarLink, linkIdx int) {
			link.URL = templ.SafeURL("/errors/" + link.ID)
			link.Label = link.ID

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

	router.Route("/errors", func(errorsRouter chi.Router) {
		errorsRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			http.Redirect(w, r, string(sidebarGroups[0].Links[0].URL), http.StatusFound)
		})

		errorsRouter.Get("/exec", func(w http.ResponseWriter, r *http.Request) {
			ErrorsPage(r).Render(r.Context(), w)
		})

		errorsRouter.Get("/{id}", func(w http.ResponseWriter, r *http.Request) {
			id := chi.URLParam(r, "id")
			mdData, ok := mdDataset[id]
			if !ok {
				http.Error(w, "not found", http.StatusNotFound)
				return
			}

			var currentLink *SidebarLink
			for _, group := range sidebarGroups {
				for _, link := range group.Links {
					if link.ID == id {
						currentLink = link
						break
					}
				}
			}

			params, err := url.ParseQuery(r.URL.RawQuery)
			if err != nil {
				http.Error(w, "bad request", http.StatusBadRequest)
				return
			}

			contents := mdData.Contents
			for key, values := range params {
				contents = strings.ReplaceAll(contents, "{ "+key+" }", strings.Join(values, ","))
				contents = santizePolicy.Sanitize(contents)
			}

			SidebarPage(r, sidebarGroups, currentLink, mdData.Title, mdData.Description, contents).Render(r.Context(), w)
		})
	})

	return nil
}
