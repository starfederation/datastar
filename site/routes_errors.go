package site

import (
	"fmt"
	"net/http"
	"slices"
	"strings"

	"github.com/a-h/templ"
	"github.com/delaneyj/toolbelt"
	"github.com/go-chi/chi/v5"
	"github.com/goccy/go-json"
	"github.com/grafana/regexp"
	"github.com/samber/lo"
	"github.com/valyala/bytebufferpool"
)

type RuntimeErrorInfo struct {
	Plugin struct {
		Name string `json:"name"`
		Type string `json:"type"`
	} `json:"plugin"`
	Element struct {
		ID  string `json:"id"`
		Tag string `json:"tag"`
	} `json:"element"`
	Raw struct {
		Key   string `json:"key"`
		Value string `json:"value"`
	} `json:"raw"`
	Expression struct {
		Key              string   `json:"key"`
		Value            string   `json:"value"`
		ValidSignalNames []string `json:"validSignalNames"`
		FnContent        string   `json:"fnContent"`
	} `json:"expression"`
	Error string `json:"error"`
}

var isSignalRe = regexp.MustCompile("(\\$\\S*) is not defined")

func setupErrors(router chi.Router) error {

	type runtimeComponentFn func(params *RuntimeErrorInfo) templ.Component
	type anyComponentFn func(params any) templ.Component
	type componentGenerator struct {
		ComponentFn anyComponentFn
		Type        string
	}

	runtimeFn := func(fn runtimeComponentFn) componentGenerator {
		return componentGenerator{
			Type: "runtime",
			ComponentFn: func(params any) templ.Component {
				return fn(params.(*RuntimeErrorInfo))
			},
		}
	}

	reasonComponents := map[string]componentGenerator{
		"runtime_expression_failed": runtimeFn(ErrViewRuntimeExpression),
	}

	sidebarLinks := make([]*SidebarLink, 0, len(reasonComponents))
	for id := range reasonComponents {
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
			typ := reasonComponents[link.ID].Type
			link.URL = templ.SafeURL(fmt.Sprintf("/errors/%s/%s", typ, link.ID))
			link.Label = toolbelt.Pascal(link.ID)

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

		errorsRouter.Get("/{type}/{reason}", func(w http.ResponseWriter, r *http.Request) {
			typ := chi.URLParam(r, "type")
			reason := chi.URLParam(r, "reason")

			var currentLink *SidebarLink
			for _, group := range sidebarGroups {
				for _, link := range group.Links {
					if link.ID == reason {
						currentLink = link
						break
					}
				}
			}

			metadataJSON := r.URL.Query().Get("metadata")
			if metadataJSON == "" {
				metadataJSON = "{}"
			}

			var params any
			switch typ {
			case "runtime":
				params = &RuntimeErrorInfo{}
			default:
				http.Error(w, fmt.Sprintf("unknown error type %q", typ), http.StatusBadRequest)
				return
			}

			if err := json.Unmarshal([]byte(metadataJSON), params); err != nil {
				http.Error(w, fmt.Sprintf("failed to unmarshal metadata: %v", err), http.StatusBadRequest)
				return
			}
			compGen, ok := reasonComponents[reason]
			if !ok {
				http.Error(w, "not found", http.StatusNotFound)
				return
			}
			buf := bytebufferpool.Get()
			defer bytebufferpool.Put(buf)

			ctx := r.Context()
			if err := compGen.ComponentFn(params).Render(ctx, buf); err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			contents := buf.String()
			SidebarPage(r, sidebarGroups, currentLink, "Errors", "Error details", contents).Render(ctx, w)
		})
	})

	return nil
}
