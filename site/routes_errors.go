package site

import (
	"context"
	"fmt"
	"html"
	"io"
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

type ErrorInfo interface {
	isErrorInfo()
}

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
		Key          string   `json:"key"`
		Value        string   `json:"value"`
		ValidSignals []string `json:"validSignals"`
		FnContent    string   `json:"fnContent"`
	} `json:"expression"`
	Action      string `json:"action"`
	ContentType string `json:"contentType"`
	Error       string `json:"error"`
	Method      string `json:"method"`
	ResultType  string `json:"resultType"`
	Selector    string `json:"selector"`
	URL         string `json:"url"`
}

func (RuntimeErrorInfo) isErrorInfo() {}

type InitErrorInfo struct {
	Plugin struct {
		Name string `json:"name"`
		Type string `json:"type"`
	} `json:"plugin"`
	MergeMode    string `json:"mergeMode"`
	PluginType   string `json:"pluginType"`
	SelectorOrID string `json:"selectorOrID"`
}

func (InitErrorInfo) isErrorInfo() {}

type InternalErrorInfo map[string]any

func (InternalErrorInfo) isErrorInfo() {}

var isSignalRe = regexp.MustCompile("(\\$\\S*) is not defined")

func setupErrors(router chi.Router) error {

	type initComponentFn func(name string, params *InitErrorInfo) templ.Component
	type internalComponentFn func(name string, params *InternalErrorInfo) templ.Component
	type runtimeComponentFn func(name string, params *RuntimeErrorInfo) templ.Component

	type anyComponentFn func(name string, params ErrorInfo) templ.Component
	type componentGenerator struct {
		ComponentFn anyComponentFn
		Type        string
	}

	initFn := func(fn initComponentFn) componentGenerator {
		return componentGenerator{
			Type: "init",
			ComponentFn: func(name string, params ErrorInfo) templ.Component {
				return fn(name, params.(*InitErrorInfo))
			},
		}
	}

	internalFn := func(fn internalComponentFn) componentGenerator {
		return componentGenerator{
			Type: "internal",
			ComponentFn: func(name string, params ErrorInfo) templ.Component {
				return fn(name, params.(*InternalErrorInfo))
			},
		}
	}

	runtimeFn := func(fn runtimeComponentFn) componentGenerator {
		return componentGenerator{
			Type: "runtime",
			ComponentFn: func(name string, params ErrorInfo) templ.Component {
				return fn(name, params.(*RuntimeErrorInfo))
			},
		}
	}

	sharedInternalErrFn := internalFn(InternalErrorView)

	reasonComponents := map[string]componentGenerator{
		"batch_error":                        sharedInternalErrFn,
		"cleanup_effect_error":               sharedInternalErrFn,
		"end_effect_error":                   sharedInternalErrFn,
		"get_computed_error":                 sharedInternalErrFn,
		"invalid_morph_style":                sharedInternalErrFn,
		"invalid_signal_key":                 sharedInternalErrFn,
		"new_element_could_not_be_created":   sharedInternalErrFn,
		"no_best_match_found":                sharedInternalErrFn,
		"no_content_found":                   sharedInternalErrFn,
		"no_parent_element_found":            sharedInternalErrFn,
		"no_temporary_element_found":         sharedInternalErrFn,
		"signal_cycle_detected":              sharedInternalErrFn,
		"signal_not_found":                   sharedInternalErrFn,
		"invalid_merge_mode":                 initFn(InvalidMergeMode),
		"invalid_plugin_type":                initFn(InvalidPluginType),
		"morph_failed":                       initFn(MorphFailed),
		"no_fragments_found":                 initFn(NoFragmentsFound),
		"no_paths_provided":                  initFn(NoPathsProvided),
		"no_selector_provided":               initFn(NoSelectorProvided),
		"no_script_provided":                 initFn(NoScriptProvided),
		"no_targets_found":                   initFn(NoTargetsFound),
		"attr_value_required":                runtimeFn(AttrValueRequired),
		"bind_key_and_value_provided":        runtimeFn(BindKeyAndValueProvided),
		"bind_key_or_value_required":         runtimeFn(BindKeyOrValueRequired),
		"bind_unsupported_signal_type":       runtimeFn(BindUnsupportedSignalType),
		"class_value_required":               runtimeFn(ClassValueRequired),
		"clipboard_not_available":            runtimeFn(ClipboardNotAvailable),
		"computed_key_required":              runtimeFn(ComputedKeyRequired),
		"computed_value_required":            runtimeFn(ComputedValueRequired),
		"custom_validity_invalid_element":    runtimeFn(CustomValidityInvalidElement),
		"custom_validity_invalid_expression": runtimeFn(CustomValidityInvalidExpression),
		"execute_expression":                 runtimeFn(ExecuteExpression),
		"generate_expression":                runtimeFn(GenerateExpression),
		"invalid_content_type":               runtimeFn(InvalidContentType),
		"invalid_data_uri":                   runtimeFn(InvalidDataUri),
		"invalid_file_result_type":           runtimeFn(InvalidFileResultType),
		"scroll_into_view_invalid_element":   runtimeFn(ScrollIntoViewInvalidElement),
		"sse_closest_form_not_found":         runtimeFn(SseClosestFormNotFound),
		"sse_fetch_failed":                   runtimeFn(SseFetchFailed),
		"sse_form_not_found":                 runtimeFn(SseFormNotFound),
		"sse_invalid_content_type":           runtimeFn(SseInvalidContentType),
		"sse_no_url_provided":                runtimeFn(SseNoUrlProvided),
		"text_invalid_element":               runtimeFn(TextInvalidElement),
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

			var params ErrorInfo
			switch typ {
			case "runtime":
				params = &RuntimeErrorInfo{}
			case "init":
				params = &InitErrorInfo{}
			case "internal":
				params = &InternalErrorInfo{}
			default:
				http.Error(w, fmt.Sprintf("unknown error type %q", typ), http.StatusBadRequest)
				return
			}

			if err := json.Unmarshal([]byte(metadataJSON), params); err != nil {
				http.Error(w, fmt.Sprintf("failed to unmarshal metadata: %v", err), http.StatusBadRequest)
				return
			}

			buf := bytebufferpool.Get()
			defer bytebufferpool.Put(buf)

			var (
				c templ.Component
			)
			compGen, ok := reasonComponents[reason]
			if ok {
				c = compGen.ComponentFn(currentLink.Label, params)
			} else {
				if typ == "internal" {
					p := params.(*InternalErrorInfo)
					c = InternalErrorView(reason, p)
				} else {
					http.Error(w, "not found", http.StatusNotFound)
					return
				}
			}

			ctx := r.Context()
			if err := c.Render(ctx, buf); err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}
			contents := buf.String()
			SidebarPage(r, sidebarGroups, currentLink, "Errors", "Error details", contents).Render(ctx, w)
		})
	})

	return nil
}

func code(lang, source string) templ.Component {
	buf := bytebufferpool.Get()
	defer bytebufferpool.Put(buf)
	unescapedSource := html.UnescapeString(source)
	err := htmlHighlight(buf, unescapedSource, lang, "")
	contents := buf.String()
	return templ.Raw(contents, err)
}

func htmlSource() templ.Component {
	return templ.ComponentFunc(func(ctx context.Context, w io.Writer) error {
		children := templ.GetChildren(ctx)
		buf := bytebufferpool.Get()
		defer bytebufferpool.Put(buf)
		if err := children.Render(ctx, buf); err != nil {
			return err
		}
		return code("html", buf.String()).Render(ctx, w)
	})
}
