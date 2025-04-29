package site

import (
	"fmt"
	"math/rand"
	"net/http"
	"slices"
	"sync"
	"time"

	"github.com/CAFxX/httpcompression"
	"github.com/go-chi/chi/v5"
	"github.com/starfederation/datastar/sdk/go/datastar"
)

func setupExamplesDbmon(examplesRouter chi.Router) error {
	dbs := NewDbmonDatabases(6)
	mutationRate := 0.2
	mu := &sync.RWMutex{}
	fps := 60

	examplesRouter.Get("/dbmon/contents", func(w http.ResponseWriter, r *http.Request) {
		sse := datastar.NewSSE(w, r)
		mu.RLock()
		c := pageDBmon(dbs.databases, mutationRate, 0)
		mu.RUnlock()
		sse.MergeFragmentTempl(c)
	})

	examplesRouter.Post("/dbmon/inputs", func(w http.ResponseWriter, r *http.Request) {
		type dbmonInput struct {
			MutationRate float64 `json:"mutationRate"`
		}
		input := &dbmonInput{}

		if err := datastar.ReadSignals(r, input); err != nil {
			datastar.NewSSE(w, r).ConsoleError(err)
			return
		}

		mu.Lock()
		mutationRate = input.MutationRate
		mu.Unlock()
	})

	compress, err := httpcompression.DefaultAdapter()
	if err != nil {
		return err
	}

	examplesRouter.Route("/dbmon/updates", func(r chi.Router) {
		r.Use(compress)

		r.Get("/", func(w http.ResponseWriter, r *http.Request) {

			sse := datastar.NewSSE(w, r)
			d := time.Second / time.Duration(fps)
			t := time.NewTicker(d)

			renderTime := 0 * time.Second
			renderCount := time.Duration(0)
			renderTimer := time.NewTicker(time.Second)
			noVT := datastar.WithoutViewTransitions()

			for {
				select {
				case <-r.Context().Done():
					return
				case <-t.C:
					mu.Lock()
					dbs.randomUpdate(0.2)
					mu.Unlock()

					now := time.Now()
					sse.MergeFragmentTempl(dbmonApp(dbs.databases))
					renderTime += time.Since(now)
					renderCount++
				case <-renderTimer.C:
					avg := renderTime / renderCount
					renderTime = 0
					renderCount = 0
					sse.MergeFragmentTempl(dbmonFPS(avg), noVT)
				}
			}
		})
	})

	return nil
}

type DbmonQuery struct {
	elapsed time.Duration
	query   string
}

func randomQuery() DbmonQuery {
	elapsed := time.Duration(rand.Float64()*15) * time.Millisecond
	query := `SELECT blah from something`

	if rand.Float32() < 0.2 {
		query = `<IDLE> in transaction`
	}

	if rand.Float32() < 0.1 {
		query = `vacuum`
	}

	return DbmonQuery{
		elapsed: elapsed,
		query:   query,
	}
}

type DbmonDatabase struct {
	name    string
	queries []DbmonQuery
}

func NewDbmonDatabase(format string, args ...any) *DbmonDatabase {
	db := &DbmonDatabase{
		name: fmt.Sprintf(format, args...),
	}
	db.update()
	return db
}

func (db *DbmonDatabase) update() {
	db.queries = db.queries[:0]

	r := rand.Intn(15) + 1
	for j := 0; j < r; j++ {
		db.queries = append(db.queries, randomQuery())
	}
}

func (db *DbmonDatabase) top5Queries() []DbmonQuery {
	qs := make([]DbmonQuery, len(db.queries))
	copy(qs, db.queries)

	slices.SortFunc(qs, func(a, b DbmonQuery) int {
		return int(a.elapsed - b.elapsed)
	})

	if len(qs) > 5 {
		qs = qs[:5]
	}

	for len(qs) < 5 {
		qs = append(qs, DbmonQuery{})
	}

	return qs
}

type DbmonDatabases struct {
	databases []*DbmonDatabase
}

func NewDbmonDatabases(n int) *DbmonDatabases {
	dbs := &DbmonDatabases{
		databases: make([]*DbmonDatabase, 0, n*2),
	}

	for i := 0; i < n; i++ {
		dbs.databases = append(dbs.databases,
			NewDbmonDatabase("cluster%d", i),
			NewDbmonDatabase("cluster%dslave", i),
		)
	}

	return dbs
}

func (dbs *DbmonDatabases) randomUpdate(r float64) {
	for _, db := range dbs.databases {
		if rand.Float64() < r {
			db.update()
		}
	}
}

func dbmonCounterClasses(count int) string {
	if count >= 15 {
		return "bg-error text-error-content"
	} else if count >= 10 {
		return "bg-warning text-warning-content"
	}

	return "bg-success text-success-content"
}
