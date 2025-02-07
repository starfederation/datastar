package site

import (
	"context"
	"embed"
	"errors"
	"fmt"
	"io/fs"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"time"

	"github.com/a-h/templ"
	"github.com/benbjohnson/hashfs"
	"github.com/blevesearch/bleve/v2"
	"github.com/delaneyj/toolbelt"
	"github.com/delaneyj/toolbelt/embeddednats"
	"github.com/go-chi/chi/v5"
	"github.com/go-chi/chi/v5/middleware"
	"github.com/gorilla/sessions"
	"github.com/huantt/plaintext-extractor"
	natsserver "github.com/nats-io/nats-server/v2/server"
)

//go:embed static/*
var staticFS embed.FS

var (
	staticSys    = hashfs.NewFS(staticFS)
	highlightCSS templ.Component
	indexPath    = filepath.Join(os.TempDir(), "data-star.bleve")
)

func staticPath(path string) string {
	return "/" + staticSys.HashName("static/"+path)
}

func staticAbsolutePath(path string) string {
	return "https://data-star.dev/" + staticSys.HashName("static/"+path)
}

func canonicalUrl(uri string) string {
	return "https://data-star.dev" + uri
}

func RunBlocking(port int, readyCh chan struct{}) toolbelt.CtxErrFunc {
	return func(ctx context.Context) error {

		router := chi.NewRouter()

		router.Use(
			middleware.Recoverer,
			// middleware.Logger,
		)

		if err := setupRoutes(ctx, router); err != nil {
			return fmt.Errorf("error setting up routes: %w", err)
		}

		srv := &http.Server{
			Addr:    fmt.Sprintf(":%d", port),
			Handler: router,
		}

		go func() {
			<-ctx.Done()
			srv.Shutdown(context.Background())
		}()

		if readyCh != nil {
			close(readyCh)
		}
		return srv.ListenAndServe()
	}
}

func setupRoutes(ctx context.Context, router chi.Router) (err error) {
	defer router.Handle("/static/*", hashfs.FileServer(staticSys))

	// Redirect `datastar.fly.dev`
	router.Use(func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if r.Host == "datastar.fly.dev" {
				target := "https://data-star.dev" + r.URL.Path
				http.Redirect(w, r, target, http.StatusMovedPermanently)
				return
			}
			next.ServeHTTP(w, r)
		})
	})

	natsPort, err := toolbelt.FreePort()
	if err != nil {
		return fmt.Errorf("error getting free port: %w", err)
	}

	ns, err := embeddednats.New(ctx, embeddednats.WithNATSServerOptions(&natsserver.Options{
		JetStream: true,
		StoreDir:  "./data/nats",
		Port:      natsPort,
	}))
	if err != nil {
		return fmt.Errorf("error creating embedded nats server: %w", err)
	}
	ns.WaitForServer()

	index, err := bleve.Open(indexPath)
	if err == bleve.ErrorIndexPathDoesNotExist {
		log.Printf("Creating new index...")
		mapping := bleve.NewIndexMapping()
		index, err = bleve.New(indexPath, mapping)
		if err != nil {
			log.Fatal(fmt.Errorf("failed to create index: %w", err))
		}

	} else if err != nil {
		log.Fatal("failed to open index: %w", err)
	} else {
		log.Printf("Opening existing index...")
	}

	if err := indexSiteContent(ctx, index); err != nil {
		log.Fatal("failed to index site content, ", err)
	}

	log.Println("Indexed site, index can be found at: ", indexPath)

	sessionSignals := sessions.NewCookieStore([]byte("datastar-session-secret"))
	sessionSignals.MaxAge(int(24 * time.Hour / time.Second))

	if err := errors.Join(
		setupHome(router, sessionSignals, ns, index),
		setupGuide(ctx, router),
		setupReferences(ctx, router),
		setupHowTos(ctx, router),
		setupExamples(ctx, router, sessionSignals),
		setupEssays(ctx, router),
		setupErrors(router),
		setupMemes(router),
		setupBundler(router),
	); err != nil {
		return fmt.Errorf("error setting up routes: %w", err)
	}

	return nil
}

type SiteIndexDoc struct {
	Title       string
	Description string
	Contents    string
}

func indexSiteContent(ctx context.Context, index bleve.Index) error {
	markdownDir := "./site/static/md"
	extractor := plaintext.NewHtmlExtractor()

	return filepath.WalkDir(markdownDir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return fmt.Errorf("error accessing path %s: %w", path, err)
		}

		if d.IsDir() && path != markdownDir {
			relDirName, err := filepath.Rel(markdownDir, path)
			if err != nil {
				return fmt.Errorf("failed to compute relative path for %s: %w", path, err)
			}

			log.Printf("Indexing directory: %s", relDirName)

			dataset, err := markdownRenders(ctx, relDirName)
			if err != nil {
				return fmt.Errorf("failed to render markdown directory %s: %w", relDirName, err)
			}

			// walks through each file in the directory and indexes it
			for key, value := range dataset {
				url := fmt.Sprintf("/%s/%s", relDirName, key)

				output, err := extractor.PlainText(value.Contents)
				if err != nil {
					return fmt.Errorf("failed to extract plain text from markdown %w", err)
				}

				doc := SiteIndexDoc{
					Title:       value.Title,
					Description: value.Description,
					Contents:    *output,
				}

				if err := index.Index(url, doc); err != nil {
					return fmt.Errorf("error indexing %s: %w", url, err)
				}
			}
		}

		return nil
	})
}
