package site

import (
	"bytes"
	"context"
	"fmt"
	"net/http"
	"strconv"
	"time"

	"github.com/blevesearch/bleve/v2"
	"github.com/delaneyj/toolbelt"
	"github.com/delaneyj/toolbelt/embeddednats"
	"github.com/dustin/go-humanize"
	"github.com/go-chi/chi/v5"
	"github.com/goccy/go-json"
	"github.com/gorilla/sessions"
	"github.com/nats-io/nats.go/jetstream"
	"github.com/samber/lo"
	datastar "github.com/starfederation/datastar/sdk/go"
	"github.com/wcharczuk/go-chart/v2"
	"github.com/wcharczuk/go-chart/v2/drawing"
)

func setupHome(router chi.Router, signals sessions.Store, ns *embeddednats.Server, searchIndex bleve.Index) error {

	nc, err := ns.Client()
	if err != nil {
		return fmt.Errorf("error creating nats client: %w", err)
	}
	js, err := jetstream.New(nc)
	if err != nil {
		return fmt.Errorf("error creating jetstream client: %w", err)
	}
	kv, err := js.CreateOrUpdateKeyValue(context.Background(), jetstream.KeyValueConfig{
		Bucket:      "todos",
		Description: "Datastar Todos",
		Compression: true,
		TTL:         time.Hour,
		MaxBytes:    16 * 1024 * 1024,
	})
	if err != nil {
		return fmt.Errorf("error creating key value: %w", err)
	}

	saveMVC := func(ctx context.Context, sessionID string, mvc *TodoMVC) error {
		b, err := json.Marshal(mvc)
		if err != nil {
			return fmt.Errorf("failed to marshal mvc: %w", err)
		}
		if _, err := kv.Put(ctx, sessionID, b); err != nil {
			return fmt.Errorf("failed to put key value: %w", err)
		}
		return nil
	}

	resetMVC := func(mvc *TodoMVC) {
		mvc.Mode = TodoViewModeAll
		mvc.Todos = []*Todo{
			{Text: "Learn any backend language", Completed: true},
			{Text: "Learn Datastar", Completed: false},
			{Text: "???", Completed: false},
			{Text: "Profit", Completed: false},
		}
		mvc.EditingIdx = -1
	}

	mvcSession := func(w http.ResponseWriter, r *http.Request) (string, *TodoMVC, error) {
		ctx := r.Context()
		sessionID, err := upsertSessionID(signals, r, w)
		if err != nil {
			return "", nil, fmt.Errorf("failed to get session id: %w", err)
		}

		mvc := &TodoMVC{}
		if entry, err := kv.Get(ctx, sessionID); err != nil {
			if err != jetstream.ErrKeyNotFound {
				return "", nil, fmt.Errorf("failed to get key value: %w", err)
			}
			resetMVC(mvc)

			if err := saveMVC(ctx, sessionID, mvc); err != nil {
				return "", nil, fmt.Errorf("failed to save mvc: %w", err)
			}
		} else {
			if err := json.Unmarshal(entry.Value(), mvc); err != nil {
				return "", nil, fmt.Errorf("failed to unmarshal mvc: %w", err)
			}
		}
		return sessionID, mvc, nil
	}

	router.Get("/", func(w http.ResponseWriter, r *http.Request) {
		Home().Render(r.Context(), w)
	})

	chartWidth := 475
	alpineJS := 15.3 * 1024
	htmx := 17.4 * 1024
	hyperscript := 26.8 * 1024

	graph := chart.BarChart{
		Title:  "File Size Comparison",
		Width:  chartWidth,
		Height: chartWidth,
		Background: chart.Style{
			FillColor: drawing.Color{R: 1, G: 1, B: 1, A: 0},
			FontColor: drawing.ColorWhite,
		},
		Canvas: chart.Style{
			FillColor: drawing.Color{R: 1, G: 1, B: 1, A: 0},
			FontColor: drawing.ColorWhite,
			FontSize:  6,
		},
		TitleStyle: chart.Style{
			FontColor: drawing.ColorWhite,
		},
		XAxis: chart.Style{
			FontColor: drawing.ColorWhite,
		},
		YAxis: chart.YAxis{
			Style: chart.Style{
				FontColor: drawing.ColorWhite,
			},
			ValueFormatter: func(v any) string {
				return humanize.Bytes(uint64(v.(float64)))
			},
			Range: &chart.ContinuousRange{
				Min: 0.001,
				Max: 40000,
			},
		},
		// https://bundlephobia.com/package/@starfederation/datastar
		// https://bundlephobia.com/package/htmx.org
		// https://bundlephobia.com/package/alpinejs
		// https://bundlephobia.com/package/hyperscript.org

		Bars: []chart.Value{
			{Label: "HTMX+\nhyperscript", Value: hyperscript + htmx},
			{Label: "HTMX+\nAlpine.js", Value: alpineJS + htmx},
			{Label: "HTMX", Value: htmx},
			{Label: "Alpine.js", Value: alpineJS},
			{Label: "Datastar+\nAll Plugins", Value: float64(datastar.VersionClientByteSizeGzip)},
			{Label: "Datastar Core", Value: 4.3 * 1024},
		},
	}

	buffer := bytes.NewBuffer([]byte{})
	if err = graph.Render(chart.SVG, buffer); err != nil {
		panic(err)
	}
	homePageChartSVG := buffer.Bytes()
	router.Get("/chart", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "image/svg+xml")
		w.Write(homePageChartSVG)
	})

	router.Route("/api", func(apiRouter chi.Router) {
		apiRouter.Get("/search", func(w http.ResponseWriter, r *http.Request) {
			type Result struct {
				ID    string  `json:"id"`
				Score float64 `json:"score"`
			}
			query := r.URL.Query().Get("q")
			if query == "" {
				http.Error(w, "missing query", http.StatusBadRequest)
				return
			}

			searchRequest := bleve.NewSearchRequest(bleve.NewQueryStringQuery(query))
			searchRequest.Fields = []string{"*"}
			searchRequest.Size = 5

			searchResult, err := searchIndex.Search(searchRequest)
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}

			results := make([]Result, 0, len(searchResult.Hits))
			for _, hit := range searchResult.Hits {
				results = append(results, Result{
					ID:    hit.ID,
					Score: hit.Score,
				})
			}

			b, err := json.MarshalIndent(results, "", " ")
			if err != nil {
				http.Error(w, err.Error(), http.StatusInternalServerError)
				return
			}

			w.Header().Set("Content-Type", "application/json")
			w.Write(b)
		})
		apiRouter.Route("/todos", func(todosRouter chi.Router) {
			todosRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {

				sessionID, mvc, err := mvcSession(w, r)
				if err != nil {
					http.Error(w, err.Error(), http.StatusInternalServerError)
					return
				}

				sse := datastar.NewSSE(w, r)

				// Watch for updates
				ctx := r.Context()
				watcher, err := kv.Watch(ctx, sessionID)
				if err != nil {
					http.Error(w, err.Error(), http.StatusInternalServerError)
					return
				}
				defer watcher.Stop()

				for {
					select {
					case <-ctx.Done():
						return
					case entry := <-watcher.Updates():
						if entry == nil {
							continue
						}
						if err := json.Unmarshal(entry.Value(), mvc); err != nil {
							http.Error(w, err.Error(), http.StatusInternalServerError)
							return
						}
						c := TodosMVCView(mvc)
						if err := sse.MergeFragmentTempl(c); err != nil {
							sse.ConsoleError(err)
							return
						}
					}
				}
			})

			todosRouter.Put("/reset", func(w http.ResponseWriter, r *http.Request) {
				sessionID, mvc, err := mvcSession(w, r)
				if err != nil {
					http.Error(w, err.Error(), http.StatusInternalServerError)
					return
				}

				resetMVC(mvc)
				if err := saveMVC(r.Context(), sessionID, mvc); err != nil {
					http.Error(w, err.Error(), http.StatusInternalServerError)
					return
				}
			})

			todosRouter.Put("/cancel", func(w http.ResponseWriter, r *http.Request) {

				sessionID, mvc, err := mvcSession(w, r)
				sse := datastar.NewSSE(w, r)
				if err != nil {
					sse.ConsoleError(err)
					return
				}

				mvc.EditingIdx = -1
				if err := saveMVC(r.Context(), sessionID, mvc); err != nil {
					sse.ConsoleError(err)
					return
				}
			})

			todosRouter.Put("/mode/{mode}", func(w http.ResponseWriter, r *http.Request) {

				sessionID, mvc, err := mvcSession(w, r)
				if err != nil {
					http.Error(w, err.Error(), http.StatusInternalServerError)
					return
				}

				modeStr := chi.URLParam(r, "mode")
				modeRaw, err := strconv.Atoi(modeStr)
				if err != nil {
					http.Error(w, err.Error(), http.StatusBadRequest)
					return
				}

				mode := TodoViewMode(modeRaw)
				if mode < TodoViewModeAll || mode > TodoViewModeCompleted {
					http.Error(w, "invalid mode", http.StatusBadRequest)
					return
				}

				mvc.Mode = mode
				if err := saveMVC(r.Context(), sessionID, mvc); err != nil {
					http.Error(w, err.Error(), http.StatusInternalServerError)
					return
				}
			})

			todosRouter.Route("/{idx}", func(todoRouter chi.Router) {
				routeIndex := func(w http.ResponseWriter, r *http.Request) (int, error) {
					idx := chi.URLParam(r, "idx")
					i, err := strconv.Atoi(idx)
					if err != nil {
						http.Error(w, err.Error(), http.StatusBadRequest)
						return 0, err
					}
					return i, nil
				}

				todoRouter.Post("/toggle", func(w http.ResponseWriter, r *http.Request) {
					sessionID, mvc, err := mvcSession(w, r)

					sse := datastar.NewSSE(w, r)
					if err != nil {
						sse.ConsoleError(err)
						return
					}

					i, err := routeIndex(w, r)
					if err != nil {
						sse.ConsoleError(err)
						return
					}

					if i < 0 {
						setCompletedTo := false
						for _, todo := range mvc.Todos {
							if !todo.Completed {
								setCompletedTo = true
								break
							}
						}
						for _, todo := range mvc.Todos {
							todo.Completed = setCompletedTo
						}
					} else {
						todo := mvc.Todos[i]
						todo.Completed = !todo.Completed
					}

					saveMVC(r.Context(), sessionID, mvc)
				})

				todoRouter.Route("/edit", func(editRouter chi.Router) {
					editRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
						sessionID, mvc, err := mvcSession(w, r)
						if err != nil {
							http.Error(w, err.Error(), http.StatusInternalServerError)
							return
						}

						i, err := routeIndex(w, r)
						if err != nil {
							return
						}

						mvc.EditingIdx = i
						saveMVC(r.Context(), sessionID, mvc)
					})

					editRouter.Put("/", func(w http.ResponseWriter, r *http.Request) {
						type Signals struct {
							Input string `json:"input"`
						}
						signals := &Signals{}

						if err := datastar.ReadSignals(r, signals); err != nil {
							http.Error(w, err.Error(), http.StatusBadRequest)
							return
						}

						if signals.Input == "" {
							return
						}

						sessionID, mvc, err := mvcSession(w, r)
						if err != nil {
							http.Error(w, err.Error(), http.StatusInternalServerError)
							return
						}

						i, err := routeIndex(w, r)
						if err != nil {
							return
						}

						if i >= 0 {
							mvc.Todos[i].Text = signals.Input
						} else {
							mvc.Todos = append(mvc.Todos, &Todo{
								Text:      signals.Input,
								Completed: false,
							})
						}
						mvc.EditingIdx = -1

						saveMVC(r.Context(), sessionID, mvc)

					})
				})

				todoRouter.Delete("/", func(w http.ResponseWriter, r *http.Request) {
					i, err := routeIndex(w, r)
					if err != nil {
						return
					}

					sessionID, mvc, err := mvcSession(w, r)
					if err != nil {
						http.Error(w, err.Error(), http.StatusInternalServerError)
						return
					}

					if i >= 0 {
						mvc.Todos = append(mvc.Todos[:i], mvc.Todos[i+1:]...)
					} else {
						mvc.Todos = lo.Filter(mvc.Todos, func(todo *Todo, i int) bool {
							return !todo.Completed
						})
					}
					saveMVC(r.Context(), sessionID, mvc)
				})
			})
		})
	})

	return nil
}

func MustJSONMarshal(v any) string {
	b, err := json.MarshalIndent(v, "", " ")
	if err != nil {
		panic(err)
	}
	return string(b)
}

func upsertSessionID(signals sessions.Store, r *http.Request, w http.ResponseWriter) (string, error) {

	sess, err := signals.Get(r, "connections")
	if err != nil {
		return "", fmt.Errorf("failed to get session: %w", err)
	}
	id, ok := sess.Values["id"].(string)
	if !ok {
		id = toolbelt.NextEncodedID()
		sess.Values["id"] = id
		if err := sess.Save(r, w); err != nil {
			return "", fmt.Errorf("failed to save session: %w", err)
		}
	}
	return id, nil
}
