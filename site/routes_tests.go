package site

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"strings"

	"github.com/a-h/templ"
	"github.com/go-chi/chi/v5"
	"github.com/go-sanitize/sanitize"
	"github.com/samber/lo"
)

func setupTests(ctx context.Context, router chi.Router) (err error) {
	mdDataset, err := markdownRenders(ctx, "tests")
	if err != nil {
		return err
	}

	sanitizer, err = sanitize.New()
	if err != nil {
		return fmt.Errorf("error creating sanitizer: %w", err)
	}

	sidebarGroups := []*SidebarGroup{
		{
			Label: "tests",
			Links: []*SidebarLink{
				{ID: "aliased"},
				{ID: "attr_false"},
				{ID: "attr_object_false"},
				{ID: "checkbox_array"},
				{ID: "checkbox_boolean"},
				{ID: "checkbox_boolean_checked"},
				{ID: "checkbox_value"},
				{ID: "checkbox_value_checked"},
				{ID: "custom_plugin"},
				{ID: "indicator"},
				{ID: "indicator_element_removed"},
				{ID: "input_array"},
				{ID: "input_signal"},
				{ID: "input_value"},
				{ID: "json_merged_signals"},
				{ID: "key_casing"},
				{ID: "local_signals"},
				{ID: "merge_fragment"},
				{ID: "merge_fragment_on_load"},
				{ID: "merge_fragment_outer_multiple_targets"},
				{ID: "merge_fragment_signals"},
				{ID: "merge_fragment_whitespace"},
				{ID: "on_load"},
				{ID: "on_load_delay"},
				{ID: "persist_signal"},
				{ID: "persist_signal_key"},
				{ID: "plugin_name_prefix"},
				{ID: "radio_value"},
				{ID: "ref"},
				{ID: "remove_fragment"},
				{ID: "select_multiple"},
				{ID: "select_single"},
				{ID: "signal_value_usage"},
				{ID: "signals_change"},
				{ID: "signals_change_path"},
				{ID: "signals_change_path_once"},
				{ID: "sse_events"},
			},
		},
	}
	lo.ForEach(sidebarGroups, func(group *SidebarGroup, grpIdx int) {
		lo.ForEach(group.Links, func(link *SidebarLink, linkIdx int) {
			link.URL = templ.SafeURL("/tests/" + link.ID)
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

	router.Route("/tests", func(testsRouter chi.Router) {
		testsRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			http.Redirect(w, r, string(sidebarGroups[0].Links[0].URL), http.StatusFound)
		})

		testsRouter.Get("/{name}", func(w http.ResponseWriter, r *http.Request) {
			ctx := r.Context()
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

			SidebarPage(r, sidebarGroups, currentLink, mdData.Title, mdData.Description, mdData.Contents).Render(ctx, w)
		})

		if err := errors.Join(
			setupTestsIndicator(testsRouter),
			setupTestsIndicatorElementRemoved(testsRouter),
			setupTestsMergeFragment(testsRouter),
			setupTestsMergeFragmentOnLoad(testsRouter),
			setupTestsMergeFragmentOuterMuplipleTargets(testsRouter),
			setupTestsMergeFragmentSignals(testsRouter),
			setupTestsMergeFragmentWhitespace(testsRouter),
			setupTestsOnLoad(testsRouter),
			setupTestsRemoveFragment(testsRouter),
			setupTestsSseEvents(testsRouter),
		); err != nil {
			panic(fmt.Sprintf("error setting up tests routes: %s", err))
		}
	})

	return nil
}
