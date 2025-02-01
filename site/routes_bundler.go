package site

import (
	"archive/zip"
	"compress/gzip"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"time"

	"github.com/delaneyj/toolbelt"
	"github.com/evanw/esbuild/pkg/api"
	"github.com/go-chi/chi/v5"
	"github.com/segmentio/encoding/json"
	datastar "github.com/starfederation/datastar/sdk/go"
	"github.com/valyala/bytebufferpool"
	"github.com/zeebo/xxh3"
)

var (
	datastarBundlerRegexp = regexp.MustCompile(`import { (?P<name>[^']*) } from '(?P<path>[^']*)'\n`)
	pluginTypeRegexp      = regexp.MustCompile(`pluginType: "(?P<name>.*)",`)
	nameRegexp            = regexp.MustCompile(`name: "(?P<name>.*)",`)
	prefixRegexp          = regexp.MustCompile(`prefix: "(?P<name>.*)",`)
)

type BundlerStore struct {
	IncludedPlugins map[string]bool `json:"includedPlugins"`
	Alias           string          `json:"alias"`
}

type PluginDetails struct {
	Label       string `json:"label"`
	Name        string `json:"name"`
	Path        string `json:"path"`
	Type        string `json:"type"`
	Authors     string `json:"author"`
	Slug        string `json:"slug"`
	Description string `json:"description"`
	Icon        string `json:"icon"`
	Key         string `json:"key"`
	Contents    string `json:"contents"`
}

type PluginManifest struct {
	Version string          `json:"version"`
	Plugins []PluginDetails `json:"plugins"`
	Alias   string          `json:"alias"`
}

func setupBundler(router chi.Router) error {

	tmpDir, err := os.MkdirTemp("", "datastar-bundler")
	if err != nil {
		return fmt.Errorf("error creating temp dir: %w", err)
	}

	distDir := filepath.Join(tmpDir, "dist")
	if err = os.MkdirAll(distDir, 0755); err != nil {
		return fmt.Errorf("error creating out dir: %w", err)
	}

	// Copy the static files to the temp dir.
	if err := fs.WalkDir(staticFS, "static/librarySource", func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}

		relDir := strings.TrimPrefix(path, "static/librarySource")

		if d.IsDir() {
			if err := os.MkdirAll(filepath.Join(tmpDir, relDir), 0755); err != nil {
				return err
			}
		} else {
			src, err := staticFS.Open(path)
			if err != nil {
				return fmt.Errorf("error opening static file: %w", err)
			}
			defer src.Close()

			dst, err := os.Create(filepath.Join(tmpDir, relDir))
			if err != nil {
				return fmt.Errorf("error creating file: %w", err)
			}

			if _, err := io.Copy(dst, src); err != nil {
				return fmt.Errorf("error copying static file: %w", err)
			}
		}

		return nil
	}); err != nil {
		return fmt.Errorf("error copying static files: %w", err)
	}

	manifest := PluginManifest{
		Version: datastar.Version,
	}

	allIncludedBundler, err := staticFS.ReadFile("static/librarySource/bundles/datastar.ts")
	if err != nil {
		return fmt.Errorf("error reading all included bundler: %w", err)
	}

	allIncludedBundlerContents := string(allIncludedBundler)
	matches := datastarBundlerRegexp.FindAllStringSubmatch(allIncludedBundlerContents, -1)
	for _, match := range matches {
		name := match[1]
		path := match[2]

		if !strings.HasPrefix(path, "../plugins") {
			continue
		}

		tsRelpath := path[2:] + ".ts"
		tsSrcFilepath := filepath.Join("static", "librarySource", tsRelpath)
		b, err := staticFS.ReadFile(tsSrcFilepath)
		if err != nil {
			return fmt.Errorf("error reading plugin file: %w", err)
		}
		contents := string(b)

		key := toolbelt.Snake(strings.ReplaceAll(tsSrcFilepath, string(filepath.Separator), "_"))

		details := PluginDetails{
			Label:    name,
			Name:     name,
			Path:     path,
			Icon:     "ic:baseline-rocket-launch",
			Key:      key,
			Contents: contents,
		}

		for _, match := range pluginTypeRegexp.FindAllStringSubmatch(contents, 1) {
			details.Type = toolbelt.Upper(match[1])
		}

		switch details.Type {
		case "ACTION":
			for _, match := range nameRegexp.FindAllStringSubmatch(contents, 1) {
				details.Label = match[1]
			}
		case "ATTRIBUTE":
			for _, match := range prefixRegexp.FindAllStringSubmatch(contents, 1) {
				details.Label = "data-" + toolbelt.Kebab(match[1])
			}
		}

		lines := strings.Split(contents, "\n")
		for _, line := range lines {
			if !strings.HasPrefix(line, "//") {
				break
			}

			line = strings.TrimPrefix(line, "//")
			lineParts := strings.SplitN(line, ":", 2)

			if len(lineParts) != 2 {
				continue
			}

			key := strings.TrimSpace(lineParts[0])
			value := strings.TrimSpace(lineParts[1])

			switch key {
			case "Slug":
				details.Slug = value
			case "Description":
				details.Description = value
			case "Authors":
				details.Authors = value
			case "Icon":
				details.Icon = value
			}
		}

		manifest.Plugins = append(manifest.Plugins, details)
	}

	slices.SortFunc(manifest.Plugins, func(a, b PluginDetails) int {
		typ := strings.Compare(a.Type, b.Type)
		if typ != 0 {
			return typ
		}

		return strings.Compare(a.Path, b.Path)
	})

	router.Route("/bundler", func(bundlerRouter chi.Router) {
		bundlerRouter.Get("/", func(w http.ResponseWriter, r *http.Request) {
			signals := &BundlerStore{
				IncludedPlugins: map[string]bool{},
			}
			for _, plugin := range manifest.Plugins {
				signals.IncludedPlugins[plugin.Key] = true
			}
			PageBundler(r, manifest, signals).Render(r.Context(), w)
		})

		bundlerRouter.Post("/", func(w http.ResponseWriter, r *http.Request) {
			store := &BundlerStore{}
			if err := datastar.ReadSignals(r, store); err != nil {
				http.Error(w, "error parsing request: "+err.Error(), http.StatusBadRequest)
				return
			}

			sse := datastar.NewSSE(w, r)

			revisedManifest := PluginManifest{
				Version: manifest.Version,
				Alias:   toolbelt.Lower(store.Alias),
			}
			for _, plugin := range manifest.Plugins {
				if !store.IncludedPlugins[plugin.Key] {
					continue
				}

				revisedManifest.Plugins = append(revisedManifest.Plugins, plugin)
			}

			bundleContents, err := bundlePlugins(tmpDir, revisedManifest)
			if err != nil {
				sse.ConsoleError(err)
				return
			}

			c := bundlerResultsFragment(*bundleContents)
			sse.MergeFragmentTempl(c)
		})

		bundlerRouter.Get("/download/{filename}", func(w http.ResponseWriter, r *http.Request) {
			filename := chi.URLParam(r, "filename")
			if !strings.HasSuffix(filename, ".zip") {
				http.Error(w, "invalid filename", http.StatusBadRequest)
				return
			}

			filename = filepath.Join(distDir, filename)
			bundleResults, err := os.ReadFile(filename)
			if err != nil {
				http.Error(w, "error reading bundle results: "+err.Error(), http.StatusInternalServerError)
				return
			}

			w.Header().Set("Content-Type", "application/zip")
			w.Write(bundleResults)
		})
	})

	return nil
}

type BundleResults struct {
	Hash              string         `json:"hash"`
	SourceSize        uint64         `json:"sourceSize"`
	SourceSizeGzipped uint64         `json:"sourceSizeGzipped"`
	CompileTime       time.Duration  `json:"compileTime"`
	Name              string         `json:"name"`
	DownloadURL       string         `json:"downloadURL"`
	Manifest          PluginManifest `json:"manifest"`
}

func bundlePlugins(tmpDir string, manifest PluginManifest) (results *BundleResults, err error) {
	start := time.Now()

	distDir := filepath.Join(tmpDir, "dist")

	h := xxh3.New()
	h.WriteString(manifest.Version)
	for _, plugin := range manifest.Plugins {
		h.WriteString(plugin.Contents)
	}
	hash := h.Sum64()
	hashedName := fmt.Sprintf("datastar-%s-%x", toolbelt.Kebab(manifest.Version), hash)

	bundleResultsPath := filepath.Join(distDir, hashedName+".json")

	// check if the bundle already exists
	results = &BundleResults{}
	if _, err := os.Stat(bundleResultsPath); err == nil {
		bundleResults, err := os.ReadFile(bundleResultsPath)
		if err != nil {
			return nil, fmt.Errorf("error reading bundle results: %w", err)
		}

		if err = json.Unmarshal(bundleResults, results); err != nil {
			return nil, fmt.Errorf("error unmarshalling bundle results: %w", err)
		}

		return results, nil
	}

	bundleTypescriptPath := filepath.Join("bundles", hashedName+".ts")

	// distFile := filepath.Join(distDir, hashedName+".js")

	bundleOutFile := filepath.Join(tmpDir, bundleTypescriptPath)
	bundleFileContents := bundlerContent(manifest)
	if err = os.WriteFile(bundleOutFile, []byte(bundleFileContents), 0644); err != nil {
		return nil, fmt.Errorf("error writing bundle file: %w", err)
	}

	buildResult := api.Build(api.BuildOptions{
		EntryPoints: []string{bundleOutFile},
		Banner: map[string]string{
			"js": "// Datastar v" + manifest.Version,
		},
		Outdir:            distDir,
		Bundle:            true,
		Write:             true,
		LogLevel:          api.LogLevelSilent,
		MinifyWhitespace:  true,
		MinifyIdentifiers: true,
		MinifySyntax:      true,
		Format:            api.FormatESModule,
		Sourcemap:         api.SourceMapLinked,
		Target:            api.ES2023,
	})

	if len(buildResult.Errors) > 0 {
		errs := make([]error, len(buildResult.Errors))
		for i, err := range buildResult.Errors {
			errs[i] = errors.New(err.Text)
		}
		if err = errors.Join(errs...); err != nil {
			return nil, fmt.Errorf("error joining errors: %w", err)
		}
	}

	var contents []byte
	for _, file := range buildResult.OutputFiles {
		if strings.HasSuffix(file.Path, ".js") {
			contents = file.Contents
			break
		}
	}

	buf := bytebufferpool.Get()
	defer bytebufferpool.Put(buf)

	w := gzip.NewWriter(buf)
	if _, err := w.Write(contents); err != nil {
		return nil, fmt.Errorf("error writing gzipped contents: %w", err)
	}
	if err := w.Close(); err != nil {
		return nil, fmt.Errorf("error closing gzip writer: %w", err)
	}
	gzipContents := buf.Bytes()

	results = &BundleResults{
		Name:              hashedName,
		Manifest:          manifest,
		Hash:              fmt.Sprintf("%x", hash),
		SourceSize:        uint64(len(contents)),
		SourceSizeGzipped: uint64(len(gzipContents)),
		CompileTime:       time.Since(start),
		DownloadURL:       "/bundler/download/" + hashedName + ".zip",
	}

	// write the results to the dist dir with a gzipped file
	BundleResultsContents, err := json.Marshal(results)
	if err != nil {
		return nil, fmt.Errorf("error marshalling results: %w", err)
	}
	if err = os.WriteFile(bundleResultsPath, BundleResultsContents, 0644); err != nil {
		return nil, fmt.Errorf("error writing results file: %w", err)
	}

	bundleResultsGzippedPath := filepath.Join(distDir, hashedName+".zip")
	zipFile, err := os.Create(bundleResultsGzippedPath)
	if err != nil {
		return nil, fmt.Errorf("error creating zip file: %w", err)
	}
	defer zipFile.Close()

	zipWriter := zip.NewWriter(zipFile)
	defer zipWriter.Close()

	// add the results file to the zip
	resultsWriter, err := zipWriter.Create(hashedName + ".json")
	if err != nil {
		return nil, fmt.Errorf("error creating zip file: %w", err)
	}
	if _, err := resultsWriter.Write(BundleResultsContents); err != nil {
		return nil, fmt.Errorf("error writing zip file: %w", err)
	}

	for _, file := range buildResult.OutputFiles {
		relPath, err := filepath.Rel(distDir, file.Path)
		if err != nil {
			return nil, fmt.Errorf("error getting relative path: %w", err)
		}
		w, err := zipWriter.Create(relPath)
		if err != nil {
			return nil, fmt.Errorf("error creating zip file: %w", err)
		}
		if _, err := w.Write(file.Contents); err != nil {
			return nil, fmt.Errorf("error writing zip file: %w", err)
		}
	}

	return results, nil
}
